subroutine lastevents_t(wtt,mtdata,lbig3ges,mype)

!-------- last write to events file.  write all data, with flag of
!--------  zero if successfully used, and 1 if rejected by non-lin qc
!--------

  include 'qcparam.h'

  real(4) wtt(4,max(1,mtdata))
  real(4) yot(max(1,mtdata)),xbarbt(max(1,mtdata)),bight(lbig3ges,max(1,mtdata))
  real(4) eyot00(max(1,mtdata))
  integer(4) ibight(lbig3ges,max(1,mtdata))
  character(8) tstaid(max(1,mtdata))

  integer(8),allocatable::itlabel(:)
  real(4),allocatable::tlon(:),tlat(:),tpres(:),tletaobs(:)
  real(4),allocatable::tlone(:),tlate(:),telev(:)
  real(4),allocatable::ttime(:),ttype(:),tqm(:)
  integer(4),allocatable::iqtflg(:)
  character(10)eventfile

  if(mtdata.le.0) return
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
  allocate(tlon(max(1,mtdata))) ; allocate(tlat(max(1,mtdata)))
  allocate(tlone(max(1,mtdata))) ; allocate(tlate(max(1,mtdata)))
  allocate(tpres(max(1,mtdata)))
  allocate(telev(max(1,mtdata)))
  allocate(ttime(max(1,mtdata)))
  allocate(ttype(max(1,mtdata))) ; allocate(tqm(max(1,mtdata)))
  allocate(iqtflg(max(1,mtdata)))
  allocate(itlabel(max(1,mtdata)))
  allocate(tletaobs(max(1,mtdata)))

  call rdtemps(eyot00,tlone,tlate,tlon,tlat,tpres,yot,xbarbt, &
               tletaobs,bight,ibight, &
               tstaid,ttime,telev,tqm,ttype,iqtflg,itlabel,mtdata,lbig3ges)

!  restore original error for land surface obs

  error_mult=1000000000000.
           print *,' in lastevents_t, mype,error_mult,error_mult**2=',mype,error_mult,error_mult**2
  C = 1.2533/b
  pgnot = 1.-pgt
  pgc = pgt*C
  do i=1,mtdata
   if(ttype(i).gt.180.5) then
    eyot00(i)=eyot00(i)/error_mult
    wtt(2,i)=error_mult*wtt(2,i)          !  yot
    wtt(3,i)=error_mult*wtt(3,i)          !  xbarbt
    wtt(4,i)=error_mult*wtt(4,i)          !  xbart
    arg = exp(-.5*(wtt(2,i)-wtt(3,i)-wtt(4,i))**2)
    wtt(1,i) = pgnot*arg/(pgnot*arg+pgc)
   end if
  end do

  wtlim=.1
  do i=1,mtdata
   tanl=wtt(4,i)*eyot00(i)+xbarbt(i)
   icode=0
   if(abs(wtt(1,i)).lt.wtlim) icode=1
   write(ievout,'("TT.",i3.3,11e13.5,2x,a8)')icode, &
     yot(i),xbarbt(i),tanl,tlone(i),tlate(i),exp(tpres(i)),telev(i), &
     ttime(i),tqm(i),eyot00(i),ttype(i),tstaid(i)
  end do
  close(ievout)
     print *,' in lastevents_t, mype,mtdata=',mype,mtdata


  deallocate(tlone) ; deallocate(tlate)
  deallocate(telev)
  deallocate(ttime) ; deallocate(tqm)
  deallocate(ttype)
  deallocate(tletaobs)
  deallocate(iqtflg)
  deallocate(tpres)
  deallocate(tlon) ; deallocate(tlat)
  deallocate(itlabel)

return
end subroutine lastevents_t
