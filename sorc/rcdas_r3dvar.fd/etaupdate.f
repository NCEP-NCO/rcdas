       subroutine etaupdate(inpes_out,jnpes_out, &
              tetaanl,qetaanl,q2etaanl,cwmetaanl,uetaanl,vetaanl,pdr01etaanl, &
              imeta,jmeta,lmeta,iayear,iamonth,iaday,iahour,coldstart,cwm_adjust)

!-------- input is analysis on eta grid.  transfer it to eta common and generate coldstart, nbc files


  include 'mpif.h'
      include "my_comm.h"
         INCLUDE "parmsoil"

  logical coldstart,cwm_adjust
  real(4) tetaanl(imeta,jmeta,lmeta)
  real(4) qetaanl(imeta,jmeta,lmeta)
  real(4) q2etaanl(imeta,jmeta,lmeta)
  real(4) cwmetaanl(imeta,jmeta,lmeta)
  real(4) uetaanl(imeta,jmeta,lmeta)
  real(4) vetaanl(imeta,jmeta,lmeta)
  real(4) pdr01etaanl(imeta,jmeta)

  real(4),pointer::etam(:)
  real(4),allocatable::etai(:)
         logical run,first
  integer(4) idat(3)
  real(4),allocatable::pd(:,:),res(:,:)
  real(8) wb8,sb8
  integer(4),pointer::ishifth(:),ishiftv(:)
  real(4),allocatable::htm(:,:,:),vtm(:,:,:),u00(:,:)

  call mpi_comm_rank(my_comm,mype,ierr)

  special_value=1.e20
  test_value=.98*special_value

!   get ishifth,ishiftv, which define which points are dummy points (on east global boundary only)

  allocate(ishifth(jmeta)) ; allocate(ishiftv(jmeta))
  call getshift(ishifth,ishiftv)

!-------- read in everything needed from nhb file

  allocate(etam(lmeta)) ; allocate(etai(lmeta+1))
  call getetacons(lmeta,imeta,jmeta,erlon0,erlat0,dlon0,dlat0,ptop, &
         wb8,sb8,wbglb,sbglb,istagh,istagv,etai,etam)
!   note:  wb,sb are coordinates of sw corner of local domain,
!               imeta,jmeta are dimensions of local domain, and
!                istagh,istagv define sw corner point
!        istagh=0, sw corner is h-point
!              =1, sw corner is v-point
!        istagv=0, sw corner is v-point
!              =1, sw corner is h-point
     
!-------- transfer everything needed from eta common to update guess

  allocate(pd(imeta,jmeta))
  allocate(res(imeta,jmeta))
  allocate(u00(imeta,jmeta))
  allocate(htm(imeta,jmeta,lmeta))
  allocate(vtm(imeta,jmeta,lmeta))
  call someof_etages(test_value,run,idat,ihrst,ntsd,htm,vtm,res, &
        u00,imeta,jmeta,myis2,myie2,myjs2,myje2)
  do j=myjs2,myje2
   do i=myis2,myie2
    pd(i,j)=pdr01etaanl(i,j)/(.01*res(i,j))
   end do
  end do
  call exch(pd,1,5,5)

!     convert t from virtual back to sensible temp

  do k=1,lmeta
   do j=myjs2,myje2
    do i=myis2,myie2
     tetaanl(i,j,k)=tetaanl(i,j,k)/(1.+.608*qetaanl(i,j,k))
    end do
   end do
  end do

!  now create analysis restart file

  if(mype.eq.0) print *,' guess run,idat,ihrst,ntsd=',run,idat,ihrst,ntsd
  idat(1)=iamonth
  idat(2)=iaday
  idat(3)=iayear
  ihrst=iahour
  ntsd=0
  do k=1,lmeta
   call satqlim(qetaanl(1,1,k),tetaanl(1,1,k),cwmetaanl(1,1,k),u00,etam(k), &
                 imeta,jmeta,pd,ptop,res,htm(1,1,k), &
                 myis2,myie2,myjs2,myje2,ishifth,mype,cwm_adjust)
  end do
  deallocate(etam)

  do k=1,lmeta
   do j=myjs2,myje2
    do i=myis2,myie2
     tetaanl(i,j,k)=htm(i,j,k)*tetaanl(i,j,k)
     qetaanl(i,j,k)=htm(i,j,k)*qetaanl(i,j,k)
     q2etaanl(i,j,k)=htm(i,j,k)*q2etaanl(i,j,k)
     uetaanl(i,j,k)=vtm(i,j,k)*uetaanl(i,j,k)
     vetaanl(i,j,k)=vtm(i,j,k)*vetaanl(i,j,k)
     cwmetaanl(i,j,k)=htm(i,j,k)*cwmetaanl(i,j,k)
    end do
   end do
  end do
  call exch(tetaanl,lmeta,5,5)
  call exch(qetaanl,lmeta,5,5)
  call exch(q2etaanl,lmeta,5,5)
  call exch(uetaanl,lmeta,5,5)
  call exch(vetaanl,lmeta,5,5)
  call exch(cwmetaanl,lmeta,5,5)
  call moveback_etages(run,idat,ihrst,ntsd,tetaanl,qetaanl,q2etaanl, &
                       uetaanl,vetaanl,cwmetaanl,pd,imeta,jmeta,ishifth,ishiftv)
  call out_restart_pieces_newijpe(inpes_out,jnpes_out)
  call BOCOoutan
  if(coldstart) call makecoldstart
  deallocate(ishifth) ; deallocate(ishiftv)

  deallocate(htm) ; deallocate(vtm)
  deallocate(pd)
  deallocate(res)
  deallocate(u00)

return
end subroutine etaupdate
