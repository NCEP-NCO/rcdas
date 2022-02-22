subroutine sorttemps(erlon0,erlat0,iordges,lbig2ges,lbig3ges,lhalfges, &
           pdres01,tgges,qgges,hgges,imeta,jmeta,lmetaex,lmh, &
           etamex,etaiex,ptop,delhour,grosst,wbglb,dlon0,sbglb,dlat0,imetaglb,jmetaglb,iuniterr,npes)


!-------- evaluate guess at each obs point. then write out obs to temp
!-------- storage.

  include 'mpif.h'
      include "my_comm.h"
  include 'writges.h'

!-------- external arrays

  real(4) pdres01(imeta*jmeta),tgges(imeta*jmeta,lmetaex),qgges(imeta*jmeta,lmetaex)
  real(4) hgges(imeta*jmeta,lmetaex+1)
  integer(4) lmh(imeta*jmeta)
  real(4) etamex(lmetaex),etaiex(lmetaex+1)

!--------  internal work space

  real(4),allocatable::terr(:),tlon(:),tlat(:),tlong(:),tlatg(:)
  real(4),allocatable::tpres(:),tobs(:)
  integer(4),allocatable::icode(:)
  real(4),allocatable::tletaobs(:),bight(:,:)
  integer(4),allocatable::ibight(:,:)
  character(8),allocatable::tstaid(:)
  real(4),allocatable::tges(:),tges0(:),qges(:)
  real(4),allocatable::ttime(:),telev(:),tqm(:)
  real(4),allocatable::ttype(:)
  integer(4),allocatable::iqtflg(:)
  integer(8),allocatable::itlabel(:)
  real(4),allocatable::tlontest(:),tlattest(:)
  integer(4),allocatable::ngross_table(:),ngross_disp(:)
  character(90),allocatable::jwrite(:),jwrite0(:)
  real(8) ttbar_8,ttrms_8,count_8
  real(8) ttbarall_8,ttrmsall_8,countall_8
  character(8),allocatable::testaid(:)
  real(4),allocatable::tepres(:),tetime(:),tetype(:),teobs(:)
  real(4),allocatable::wgts3(:,:)
  integer(4),allocatable::iwgts3(:,:)
  character(8) tail(100)
  character*16 filename,filenameg
  real(4) xmsg
  character(10)eventfile

  call mpi_comm_rank(my_comm,mype,ierr)
  xmsg=9999.

!-------- open dataset to hold diagnostic statistics output

  iwrite=156
  if(mype.eq.0) open(iwrite,file='fitt',form='formatted')

!-------- bring in all temps from common

  call count_temps(ntdata)
  call mpi_allreduce(ntdata,ntdataall,1,mpi_integer,mpi_sum,my_comm,ierr)
  if(ntdataall.le.0) return      !   no data to process

  if(mype.eq.0) write(0,*)' sorttemps--mype,ntdata: ',mype,ntdata
  allocate(terr(max(1,ntdata))) ; allocate(tlon(max(1,ntdata)))
  allocate(tlat(max(1,ntdata)))
  allocate(tlong(max(1,ntdata))) ; allocate(tlatg(max(1,ntdata)))
  allocate(tpres(max(1,ntdata))) ; allocate(tobs(max(1,ntdata)))
  allocate(tletaobs(max(1,ntdata)))
  allocate(bight(lbig3ges,max(1,ntdata)))
  allocate(ibight(lbig3ges,max(1,ntdata)))
  allocate(tstaid(max(1,ntdata)))
  allocate(ttime(max(1,ntdata))) ; allocate(telev(max(1,ntdata))) ; allocate(tqm(max(1,ntdata)))
  allocate(ttype(max(1,ntdata))) ; allocate(iqtflg(max(1,ntdata))) ; allocate(itlabel(max(1,ntdata)))
  allocate(tges(max(1,ntdata))) ; allocate(qges(max(1,ntdata)))
  allocate(tges0(max(1,ntdata)))
  allocate(tepres(max(1,ntdata))); allocate(testaid(max(1,ntdata)))
  allocate(tetime(max(1,ntdata))); allocate(tetype(max(1,ntdata)))
  allocate(teobs(max(1,ntdata)))
  allocate(icode(max(1,ntdata)))
  icode=0
  call rdtemps(terr,tlon,tlat,tlong,tlatg,tpres,tobs,tges, &
              tletaobs,bight,ibight, &
                tstaid,ttime,telev,tqm,ttype,iqtflg,itlabel,ntdata,lbig3ges)
  ttype = nint(ttype)

!-------- test eta transform

  if(ntdata.gt.0) then
   dg2rad=atan(1.)/45.
   cerlat0=cos(erlat0*dg2rad)
   serlat0=sin(erlat0*dg2rad)
   allocate(tlontest(ntdata)) ; allocate(tlattest(ntdata))
   call invtllv(tlong,tlatg,erlon0,dg2rad,cerlat0,serlat0,tlontest,tlattest,ntdata)
   errlon=0.
   errlat=0.
   do i=1,ntdata
    errlat=max(abs(tlat(i)-tlattest(i)),errlat)
    errlon=max(min(abs(tlon(i)-tlontest(i)), &
                  abs(tlon(i)-tlontest(i)+360.), &
                  abs(tlon(i)-tlontest(i)+720.), &
                  abs(tlon(i)-tlontest(i)-720.), &
                  abs(tlon(i)-tlontest(i)-360.)),errlon)
   end do
   deallocate(tlontest) ; deallocate(tlattest)
  end if
  call mpi_reduce(errlon,errlonall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(errlat,errlatall,1,mpi_real,mpi_max,0,my_comm,ierror)
  if(mype.eq.0) print *,' errlat,lon=',errlatall,errlonall

!-------- evaluate guess temp at obs locations and moisture
!-------- at obs locations

!-------- do conversion from elevation to pressure for sfc obs
!--------   (convert to guess pressure of station elevation for obs
!--------     with missing surface pressure)
!--------  make error proportional to local uncertainty of surface
!--------  elevation (presumably large when difference between obs
!--------  and model terrain is large and/or difference between 
!--------  height of adajacent local steps is large)

  istaghglb=0
  allocate(wgts3(max(1,ntdata),lbig3ges))
  allocate(iwgts3(max(1,ntdata),lbig3ges))
  call gettges(tges,qges,tlong,tlatg,tpres,telev,tstaid,ntdata, &
               iordges,lbig2ges,lbig3ges,lhalfges, &
               tletaobs,wgts3,iwgts3, &
               imeta,jmeta,lmetaex,pdres01,tgges,qgges, &
               lmh,etamex,ptop,ttype,hgges,etaiex,terr,tobs, &
               wbglb,dlon0,sbglb,dlat0,istaghglb,imetaglb,jmetaglb)
  tges0=tges
  if(ntdata.gt.0) then
   do k=1,lbig3ges
    do i=1,ntdata
     bight(k,i)=wgts3(i,k)
     ibight(k,i)=iwgts3(i,k)
    end do
   end do
  end if
  deallocate(wgts3)
  deallocate(iwgts3)

!-------- make sure all points outside domain are tagged
!--------  (except surface points, which will probably lie
!--------   somewhat outside in vertical)

  ttimemax=-huge(ttimemax)
  ttimemin=huge(ttimemax)
  if(ntdata.gt.0) then
   do i=1,ntdata
    if(tobs(i).gt.9.e4) then
     icode(i)=6
     tges(i)=1.e20
    end if
    if(terr(i).gt.1000.) then
     icode(i)=5
     tges(i)=1.e20
    end if
    ittype=nint(ttype(i))
    ittypea=nint(abs(ttype(i)))
    if(ittype.lt.0) then
     icode(i)=7
     tges(i)=1.e20
    end if
    if(ttime(i).lt.-delhour.or.ttime(i).gt.delhour) then
     icode(i)=3
     tges(i)=1.e20
    end if
    ttimemax=max(ttime(i),ttimemax)
    ttimemin=min(ttime(i),ttimemin)
   end do
  end if
  call mpi_reduce(ttimemax,ttimemaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(ttimemin,ttimeminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  if(mype.eq.0) print *,' in sorttemps, ttimemax,min,delhour=',ttimemaxall,ttimeminall,delhour
  ttmax=-huge(ttmax)
  ttmin=huge(ttmax)
  ttrms_8=0._8
  ttbar_8=0._8
  count_8=0._8
  if(ntdata.gt.0) then
   do i=1,ntdata
    if(tges(i).lt.1.e19) then
     ittype=nint(ttype(i))
     if(iqtflg(i).eq.1.and.(ittype.lt.160 &
         .or.ittype.ge.180)) tobs(i)=tobs(i)*(1.+.608*qges(i))
     ttmax=max(tobs(i)-tges(i),ttmax)
     ttmin=min(tobs(i)-tges(i),ttmin)
     diff=tobs(i)-tges(i)
     ttrms_8=ttrms_8+1._8*diff*diff
     ttbar_8=ttbar_8+1._8*diff
     count_8=count_8+1._8
    end if
   end do
  end if

!------------------ print out stats on fit of ges to data

  call restmp(tpres,tobs,tges,ttype,ntdata,iwrite)
  call mpi_reduce(ttmax,ttmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
  call mpi_reduce(ttmin,ttminall,1,mpi_real,mpi_min,0,my_comm,ierror)
  call mpi_reduce(ttrms_8,ttrmsall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(ttbar_8,ttbarall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(count_8,countall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0.and.countall_8.gt.0._8) then
   ttrmsall_8=sqrt(ttrmsall_8/countall_8)
   ttbarall_8=ttbarall_8/countall_8
   write(iwrite,'('' in sorttemps, residual tmax,min='',2es14.5)')ttmaxall,ttminall
   write(iwrite,'('' residual mean, rmst ='',2es14.5)')ttbarall_8,ttrmsall_8
  end if

!-------- do gross check for really bad temps

    rewind(20)
    ntail = 0
!   do i=1,100
!     read(20,505,err=200,end=200) tail(i)
!     if(mype.eq.0) print *,' sorttemps: tail(',i,'): ',tail(i)
!     ntail = ntail+1
!   enddo
!200 continue
!   if(mype.eq.0) print *,' sorttemps: ntail = ',ntail
!   close(20)
!505 format(1x,a8)

  ttmax=-huge(ttmax)
  ttmin=huge(ttmax)
  ttrms_8=0._8
  ttbar_8=0._8
  count_8=0._8
  ngrosst=0
  if(ntdata.gt.0) then
   allocate(jwrite(1000))
   do i=1,ntdata
!   if(ttype(i).eq.133.) print *,' sorttemps: tstaid: ',tstaid(i)
!   do j=1,ntail
!     if(tstaid(i)(1:6).eq.tail(j)(1:6)) then
!       if(mype.eq.0) print *,' keep tail no.: ',tail(j)
!       goto 201
!     endif
!   enddo
    if(tges(i).lt.1.e19) then
     if(abs(tobs(i)-tges(i)).gt.grosst) then
      icode(i)=2
      ngrosst=ngrosst+1
      if(ngrosst.lt.1000) then 
       rtpres=exp(tpres(i))
       write(jwrite(ngrosst),347)tobs(i),tges(i),terr(i),tlat(i), &
         tlon(i),rtpres,ttime(i),ttype(i),tstaid(i)
347          format(1h ,2f8.1,f6.1,f7.2,f9.2,f8.1,f12.2,f6.0,2x,a8)
       tepres(ngrosst) = min(1.e7,exp(tpres(i)))
       testaid(ngrosst) = tstaid(i)
       tetime(ngrosst) = ttime(i)
       tetype(ngrosst) = ttype(i)
       teobs(ngrosst) = tobs(i)
      end if
      tges(i)=1.e20
     else
      ttmax=max(tobs(i)-tges(i),ttmax)
      ttmin=min(tobs(i)-tges(i),ttmin)
      diff=tobs(i)-tges(i)
      ttrms_8=ttrms_8+1._8*diff*diff
      ttbar_8=ttbar_8+1._8*diff
      count_8=count_8+1._8
     end if
    end if
201 continue
   end do
  end if
! if(mype.eq.0) write(0,*) ' in sorttemps, at 13, mype =',mype
  itemp = 5
  irc = 1
! if(mype.eq.0) write(0,*)' sorttemps--ngrosst,itemp: ',ngrosst,itemp
  iunit = iuniterr + mype
! write(filename,'(''qcerror'',i3.3)')mype
! open(iunit,file=filename,form='formatted')
!!if(ngrosst.gt.0) then
!!  write(iunit,500) ngrosst, itemp, irc
!!  write(iunit,501) (testaid(i),tepres(i),tetime(i),tetype(i),i=1,ngrosst)
!!endif
  if(mype.eq.0) write(6,500) ngrosst, itemp, irc
  if(mype.eq.0) write(6,501) (testaid(i),tepres(i),tetime(i),tetype(i),i=1,ngrosst)
500 format(1x,3i8)
501 format(1x,a10,f10.1,f10.2,f10.0)
502 format(1x,a10,f10.1,f10.2,f10.0,f10.1)

!--------write out guess values

    irc = 3
    if(writges .and. ntdata.gt.0) then
!---------------------replace flagged tges with 999999.9 for output
!     do i=1,ntdata
!      if(tges(i).lt.1.e19) then
!       tges0(i)=tges(i)
!      else
!       tges0(i)=999999.9
!      end if
!     end do
      iunitg = iuniterr + mype + npes + 1
!     write(iunitg,500) ntdata, itemp, irc
!     write(iunitg,502) (tstaid(i),min(1.e7,abs(exp(tpres(i)))),ttime(i),ttype(i),tges0(i),i=1,ntdata)
    endif

!------------------ print out stats on fit of ges to data after gross 
!-------------------             check

  call mpi_allreduce(ngrosst,ngrosstall,1,mpi_integer,mpi_sum,my_comm,ierror)
  if(ngrosstall.gt.0) then
   allocate(ngross_table(0:npes-1)) ; allocate(ngross_disp(0:npes-1))
   call mpi_gather(ngrosst,1,mpi_integer,ngross_table,1,mpi_integer,0,my_comm,ierror)
   ngross_disp=0
   if(mype.eq.0) then
    do ipe=0,npes-1
     if(ipe.gt.0) ngross_disp(ipe)=ngross_disp(ipe-1)+ngross_table(ipe-1)
    end do
   end if
   allocate(jwrite0(ngrosstall))
   call mpi_gatherv(jwrite,90*ngrosst,mpi_character, &
          jwrite0,90*ngross_table,90*ngross_disp,mpi_character,0,my_comm,ierror)
   deallocate(ngross_table) ; deallocate(ngross_disp)
   if(mype.eq.0) then
    write(iwrite,*)' temps which fail gross check follow:'
    write(iwrite,*)'  tobs, tges, terr, tlat,  tlon, tpres,',' ttime,  ttype,  tid '
    do i=1,ngrosstall
     write(iwrite,*)jwrite0(i)
    end do
    write(iwrite,*)' ngrosst=',ngrosstall
    write(iwrite,*)' fit to temps after gross errors removed follows:'
   end if
   deallocate(jwrite0)
   call restmp(tpres,tobs,tges,ttype,ntdata,iwrite)
   call mpi_reduce(ttmax,ttmaxall,1,mpi_real,mpi_max,0,my_comm,ierror)
   call mpi_reduce(ttmin,ttminall,1,mpi_real,mpi_min,0,my_comm,ierror)
   call mpi_reduce(ttrms_8,ttrmsall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(ttbar_8,ttbarall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   call mpi_reduce(count_8,countall_8,1,mpi_real8,mpi_sum,0,my_comm,ierror)
   if(mype.eq.0.and.countall_8.gt.0._8) then
    ttrmsall_8=sqrt(ttrmsall_8/countall_8)
    ttbarall_8=ttbarall_8/countall_8
    write(iwrite,'('' in sorttemps, residual tmax,min='',2es14.5)')ttmaxall,ttminall
    write(iwrite,'('' residual mean, rmst ='',2es14.5)')ttbarall_8,ttrmsall_8
   end if
  end if

!   before removing flagged obs, write to events file
 
 if(ntdata.gt.0) then
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
  do i=1,ntdata
   if(icode(i).gt.0)then
    write(ievout,'("TT.",i3.3,11e13.5,2x,a8)')icode(i), &
     tobs(i),tges0(i),xmsg,tlon(i),tlat(i),exp(tpres(i)),telev(i), &
     ttime(i),tqm(i),terr(i),ttype(i),tstaid(i)
   end if
  end do
  close(ievout)
 end if

!   multiply surface temperature data by large factor.  we want it to go through
!     the analysis, so it can get written to the events file, but we don't want
!     any weight given to this data in the analysis

  error_mult=1000000000000.
           print *,' in sorttemps, mype,error_mult,error_mult**2=',mype,error_mult,error_mult**2
  if(ntdata.gt.0) then
   do i=1,ntdata
    if(ttype(i).gt.180.5) terr(i)=terr(i)*error_mult
   end do
  end if

!   remove flagged obs

  ii=0
  if(ntdata.gt.0) then
   do i=1,ntdata
    if(tges(i).lt.1.e19) then
     ii=ii+1
     terr(ii)=terr(i)
     tlon(ii)=tlon(i)
     tlat(ii)=tlat(i)
     tlong(ii)=tlong(i)
     tlatg(ii)=tlatg(i)
     tpres(ii)=tpres(i)
     tobs(ii)=tobs(i)
     tges(ii)=tges(i)
     tletaobs(ii)=tletaobs(i)
     bight(1:lbig3ges,ii)=bight(1:lbig3ges,i)
     ibight(1:lbig3ges,ii)=ibight(1:lbig3ges,i)
     tstaid(ii)=tstaid(i)
     ttime(ii)=ttime(i)
     telev(ii)=telev(i)
     tqm(ii)=tqm(i)
     ttype(ii)=ttype(i)
     iqtflg(ii)=iqtflg(i)
     itlabel(ii)=itlabel(i)
    end if
   end do
  end if
  mtdata=ii
! if(mype.eq.0) write(0,*)' sorttemps--mype,ntdata,mtdata: ',mype,ntdata,mtdata

  call wrtemps(terr,tlon,tlat,tlong,tlatg,tpres,tobs,tges, &
              tletaobs,bight,ibight, &
                tstaid,ttime,telev,tqm,ttype,iqtflg,itlabel,mtdata,lbig3ges)

   deallocate(terr)
   deallocate(tlon) ; deallocate(tlat)
   deallocate(tlong) ; deallocate(tlatg)
   deallocate(tpres) ; deallocate(tobs)
   deallocate(tges)
   deallocate(tges0)
   deallocate(tstaid) ; deallocate(ttime)
   deallocate(telev) ; deallocate(ttype)
   deallocate(iqtflg) ; deallocate(itlabel)
   deallocate(testaid); deallocate(tepres)
   deallocate(tetime); deallocate(tetype)
   deallocate(teobs)
   deallocate(tqm)
   deallocate(tletaobs)
   deallocate(bight)
   deallocate(ibight)
   deallocate(icode)

  call mpi_reduce(ntdata,ntdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  call mpi_reduce(mtdata,mtdataall,1,mpi_integer,mpi_sum,0,my_comm,ierror)
  if(mype.eq.0) then
   write(iwrite,*)' in sorttemps, ntdata,mtdata=',ntdataall,mtdataall
!  close(iwrite)
  end if

return
end subroutine sorttemps
