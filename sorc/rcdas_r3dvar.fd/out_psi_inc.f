subroutine out_psi_inc(tcor, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

!     generate output maps of analysis increment on internal analysis grid
! wne mods
  integer(4) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)


  real(4) tcor(ims:ime,jms:jme,kms:kme)
         character*50 dsname,title
         data dsname/'psiinc.dat'/
         data title/'psiinc'/
         character*80 datdes(1000)
         character*1 blank
         data blank/' '/
         data undef/-9.99e33/
         real(4) out(ids:ide,jds:jde)

         nx=ide-ids+1
         ny=jde-jds+1
         np=kde-kds+1
         icor=70
         idat=71
        kout=1
        if(kout.eq.1.and.mype.eq.0) then
         open(unit=icor,file='psiinc.des',form='formatted')
         open(unit=idat,file='psiinc.dat',form='unformatted')
         ntime=koutmax
         rlonmap0=1.
         dlonmap=1.
         rlatmap0=1.
         dlatmap=1.
         startp=1.
         pinc=1.
         do i=1,1000
          write(datdes(i),10)(blank,k=1,80)
10        format(80a1)
         end do
         write(datdes(1),100)dsname
100      format('DSET ',a50)
         write(datdes(2),200)
200      format('options big_endian sequential')
         write(datdes(3),300)title
300      format('TITLE ',a50)
         write(datdes(4),400)undef
400      format('UNDEF ',e11.2)
         write(datdes(5),500)nx,rlonmap0,dlonmap
500      format('XDEF ',i5,' LINEAR ',f7.2,f7.2)
         write(datdes(6),600)ny,rlatmap0,dlatmap
600      format('YDEF ',i5,' LINEAR ',f7.2,f7.2)
         next=7
         write(datdes(next),700)np,startp,pinc
700      format('ZDEF ',i5,' LINEAR ',f7.2,f7.2)
         next=next+1
         write(datdes(next),800)kout
800      format('TDEF ',i5,' LINEAR 0Z23may1992 24hr')
         next=next+1
         write(datdes(next),810)
810      format('VARS 1')
         next=next+1
         write(datdes(next),920)np
920      format('psiinc ',i5,' 99 psiinc')
         next=next+1
         write(datdes(next),980)
980      format('ENDVARS')
         last=next
         write(icor,2000)(datdes(i),i=1,last)
2000     format(a80)

        end if

  do k=kme,kms,-1
   call gather_grid(tcor(ims:ime,jms:jme,k),out,ims,ime,jms,jme, &
               ids, ide, jds, jde, &                              ! domain indices
               ips, ipe, jps, jpe, &                              ! patch indices
               ims, ime, jms, jme, &                              ! memory indices
         inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info
   if(mype.eq.0) write(idat)out
  end do

  close(icor)
  close(idat)

return
end subroutine out_psi_inc
