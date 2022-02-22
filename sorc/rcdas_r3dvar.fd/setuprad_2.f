subroutine setuprad_2(nsig,lbig2ges,imeta,jmeta,rad_dat0,rad_dat,nrad_dat,mrad_dat, &
     varst,varprd,npred,mype,npes,nsat,diag_rad,idiag_rad,iout_rad,sattype, &
     saterr_inflate,saterr_runfact,jpch,jpchus, &
     idate5,nangs_max,nbias_loop,nbias_loop1,mbias_loop, &
     model_id,tbias,tcbias,dt_assimilation,cbias_step_clen,allbias_old,allbias_new, &
     nusat,nuchan,iuse,varch0)


!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuprad_2  2nd step in processing radiances
!   prgmmr: parrish         org: w/nmc22    date: 02-08-11
!
! abstract: read in processed data, do final qc, iterative bias update
!
! program history log:
!   95-07-06  derber
!   96-11     wu  data from prepbufr file
!   96-12-    mcnally -changes for diagnostic file and bugfix
!   98-04-30  weiyu yang    mpi version
!   99-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   99-11-24  parrish,  modify extensively for use in eta 3dvar analysis
!   01-06-05  parrish,  bring eta version up to date with global version (noaa16 + new predictors,qc)
!   01-10-12  parrish,  bring eta version up to date with global version (Van Delst radiance + some qc mods)
!
!   input argument list:
!     rad_dat0  - radiance obs data structure
!     nrad_dat - size of rad_dat0 and rad_dat, equal to max number of satellite observations,
!                   as counted up earlier during distribution to processors
!     npred    - number of predictors
!     mype     - current pe number
!     npes     _ total number of pes
!     nsat     - number of satellites
!     diag_rad - name of file to write diagnostic info for use by Russ Treadon's monitoring program
!     idiag_rad - .true., then write diagnostic info to file diag_rad
!     iout_rad -  unit number to write out some info about radiances processed
!     sattype  - list of satellite names
!     cbias    -   input parms
!     tlapmean -     for used in constructing
!     predx    -       bias correctors
!     saterr_inflate - multiply all sat err variances by this factor to toss less data--
!                           used if trying to spin up bias correction coefficients
!     jpch     -      no. of channels*(number of sats)
!     jpchus   -      max. no. of channels used        ! tovs
!
!   output argument list:
!     rad_dat  - radiance obs data structure
!     mrad_dat - number of radiance obs kept
!     varst    - variance for skin temperature
!     varprd   - variance for predictor coefficients
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!
! Declare include files

  include 'mpif.h'
      include "my_comm.h"
  include 'types.h'
  include 'types0.h'
  include 'satbias_type.h'

  type(satbias_stuff) allbias_old(jpch),allbias_new(jpch)
  integer(4) idate5(5)
  integer(8) model_id



! Declare variables

!
!
! Declare input/output arrays

  real(4) varprd(jpch*npred),varst(imeta*jmeta)
  real(4) saterr_inflate
  type(rad_obs0) rad_dat0(max(1,nrad_dat))
  type(rad_obs) rad_dat(max(1,nrad_dat))
  character*12 diag_rad
  logical idiag_rad
  integer iout_rad,mype_rad

! Declare local arrays

  real(4),dimension(nangs_max,jpch):: cbias
  real(4),dimension(jpch)::tlapmean
  real(4) predx(jpch,npred)
  real(4) svar4,bcor4(2*(npred+1))
  real(4) diagbuf(12)
  real(4) pred(npred+jpchus-1)
  real(4),dimension(jpchus):: varinv,errf,btm,tbc,tbcnob,tlapchn,emissav
  real(4),dimension(npred+1,jpchus):: bcor
  real(4),dimension(jpchus):: var,tmpvar,tnoise
  real(4) rcount(1000),rcount_all(1000),rpenal(1000),rcount_allall(1000)

  real(4),dimension(jpchus):: ts,pems
  real(4),dimension(3*nsig+1,jpchus):: htlto
  real(4),dimension(15,nsat):: aivals,aivals1
  real(4),dimension(6+2*(npred+1),jpch):: stats,stats1

!     varch: rms error for each channel
!     nuchan: satellite channel numbers
!     nusat:  satellite numbers

  real(4),dimension(jpch):: varch,varch0

  integer(4),dimension(jpchus):: kchan,kochan
  integer(4) lndsea,nadir,knchan
  integer(4),dimension(5):: idiagbuf
  integer,dimension(55):: kidsat,kidsat1
  integer(4),dimension(jpch):: nuchan,nusat
  integer(4),dimension(0:jpch):: iuse,iouse
  integer(4),dimension(jpchus):: icx,iochan
  integer(4),dimension(jpchus):: ich

  character*10,dimension(50)::sattype
  character*10 obstype

  real(4),dimension(jpchus):: obsbt,gesbt


  logical hirs2,msu,goes,hirs3,amsua,amsub,airs,eos_amsua,hsb,goesimg
  logical,dimension(jpchus):: kuse
  logical keep,coast,land,ice
  logical delt,first,diagsave
  logical ifirst(1000),kfirst(1000)

  integer(4) insane(jpchus,nsat)
  integer(4) insane0(jpchus,nsat)
  integer(4) itotal_in(jpch)
  integer(4) itotal_in0(jpch)

  mype_rad=0

       if(mype.eq.0) write(0,*)' at 1 in setuprad_2'
!
!
! Initialize variables and constants.

       jiter=1
       deltat=1.
  delt = deltat .gt. 0.
  first = jiter .eq. 1
  nsigx    = 3*nsig
  nsigx1   = 3*nsig+1
  nstats   = 6+2*(npred+1)
  dg2rad = atan(1.0)/45.0
  rad2dg = 1.0/dg2rad
  d1=.754
  d2=-2.265
  constoz  = 10.*604229. * 9.80665 * 21.4e-9       !  extra factor of 10, for mb instead of cb, i think
!
!
! Set skin temperature and bias correction standard deviations
  varst  = sqrt(1.0)
  errbias0=1./sqrt(.1)
  varprd = 1./sqrt(.1)

!  compute saterr_inflate_inc

  saterr_inflate_inc=(saterr_inflate-1.)/max(1.,float(nbias_loop-nbias_loop1))
  
!  bring in old bias information and initialize allbias_new with some of allbias_old

                    if(mype.eq.0) write(0,*)' at 3 in setuprad_2'
  call read_satbias(allbias_old,allbias_new,nusat,nuchan,jpch, &
        nangs_max,npred,model_id,idate5,tbias,tcbias,cbias_step_clen,mype)
                    if(mype.eq.0) write(0,*)' at 4 in setuprad_2'

!    big loop to spin up bias correction coefs

  r_infl=saterr_inflate+saterr_inflate_inc
do ibias_loop=1,nbias_loop

  r_infl=max(1.,r_infl-saterr_inflate_inc)

  diagsave = first .and. idiag_rad .and. ibias_loop.eq.1
  aivals = 0.
  stats  = 0.
  kidsat=0

!   transfer bias variables

  cbias=0.
  do j=1,jpch
   predx(j,1:npred)=allbias_new(j)%predx(1:npred)
   cbias(1:nangs_max,j)=allbias_old(j)%cbias(1:nangs_max)
   tlapmean(j)=allbias_old(j)%tlapmean
         if(mype.eq.0.and.allbias_old(j)%kidsat.eq.210.and. &
              allbias_old(j)%ichan.eq.1) then
            print *,' ibias_loop,saterr_inflate,tlapmean=', &
                ibias_loop,r_infl,tlapmean(j)
            do i=1,npred
              print *,' new bias, age_bias=',predx(j,i), &
               allbias_new(j)%age_bias
            end do
         end if
  end do
  
     do jch=1,jpch
      if(varch0(jch).lt.99.9) varch(jch)=min(20.,r_infl*varch0(jch))
     end do

  if (diagsave) then
     open(4,file=diag_rad,form='unformatted')
     rewind 4
! Initialize/write parameters for satellite diagnostic file on
! first outer iteration.
     iint   = 5
     ireal  = 12
     ipchan = 7
     if(mype==0) then
        write(4) mype,iint,ireal,ipchan,jpchus
        write(6,*)'SETUPRAD_2:  write header record for mype=',&
             mype,iint,ireal,ipchan,jpchus,' ',diag_rad
     endif
  endif

!  MAIN LOOP OVER PREVIOUSLY PROCESSED SATELLITE DATA

 mrad_dat=0
 insane=0
 itotal_in=0
                    if(mype.eq.0) write(0,*)' at 5 in setuprad_2, nrad_dat=',nrad_dat
 if(nrad_dat.gt.0) then
  do n=1,nrad_dat
   obstype=rad_dat0(n)%obstype
   isat=rad_dat0(n)%isat
   isz=rad_dat0(n)%isz
   nele=rad_dat0(n)%nele
     if(mype.eq.0) write(0,*)' at 5.1 in setuprad_2, n=',n

!    Initialize logical flags for satellite platform

     hirs2      = obstype == 'hirs/2'
     hirs3      = obstype == 'hirs/3'
     msu        = obstype == 'msu'
     goes       = obstype == 'goes'
     amsua      = obstype == 'amsua'
     amsub      = obstype == 'amsub'
     airs       = obstype == 'airs'
     hsb        = obstype == 'hsb'
     eos_amsua  = obstype == 'eos_amsua'
     goesimg    = obstype == 'goesimg'
     if(.not. (amsua     .or. amsub  .or. msu       .or.&
               hirs2     .or. hirs3  .or. goes      .or. &
               airs      .or. hsb    .or. eos_amsua .or. &
               goesimg))then
       write(0,*) ' inconsistent data at 1 in setuprad_2, mype=',mype
       stop
     end if
         if(mype.eq.0) write(0,*)' at 5.2 in setuprad_2, n=',n
!
!    Based on isat, set new satellite number
!
     kidsat(isz)=isat
     if(goes)kidsat(isz)=isat+50
     if(msu)kidsat(isz)=isat+200
     if(goesimg)kidsat(isz)=isat+250
     if(amsua)kidsat(isz)=isat+300
     if(eos_amsua)kidsat(isz)=isat+300
     if(amsub)kidsat(isz)=isat+400
     if(hsb)kidsat(isz)=isat+400
     if(airs)kidsat(isz)=isat+500


!    If AMSU-A data set channel indices for later use
     if (amsua .or. eos_amsua) then
        ich(1) = newchn(kidsat(isz),1,nusat,nuchan,jpch)
        ich(2) = newchn(kidsat(isz),2,nusat,nuchan,jpch)
        ich(3) = newchn(kidsat(isz),3,nusat,nuchan,jpch)
        ich(4) = newchn(kidsat(isz),4,nusat,nuchan,jpch)
        ich(5) = newchn(kidsat(isz),5,nusat,nuchan,jpch)
        ich(15)= newchn(kidsat(isz),15,nusat,nuchan,jpch)
     endif

     nchan=0

     if (hirs2 .or. hirs3) nchan  = 19
     if (msu)nchan  = 4
     if (goes)nchan  = 18
     if (amsua .or. eos_amsua)nchan  = 15
     if (amsub)nchan  = 5
     if (airs) then
       write(0,*) ' inconsistent data in at 2 setuprad_2, mype=',mype
       stop
     end if
     if (hsb)nchan  = 5


     tnoise = 1.e10
     itoss = 1
     do jc=1,nchan
!
!     Load channel numbers into local array based on satellite type

        iochan(jc)=newchn(kidsat(isz),jc,nusat,nuchan,jpch)
!
!          Temporary fix for problems with NOAA 11 channel 9 coefficients
!          Use the coefficients from NOAA 14, channel 9.
!       if ( (kidsat(isz).eq.11) .and. (ichan(jc).eq.9) ) &    !??????? commented out in new operational
!            iochan(jc) = newchn(14,9,nusat,nuchan,jpch)       !??????? but do we need for reanalysis???
!
!     Set error instrument channels

        iouse(jc)=iuse(iochan(jc))
!       if((iouse(jc) < 0 .and. .not. first) .or. iouse(jc) < -1) then
        if((iouse(jc).lt.0.and..not.first).or.iouse(jc).le.-1) then
          tnoise(jc)=1.e10
        else
          tnoise(jc) = varch(iochan(jc))
        end if
        if (tnoise(jc).lt.1e4) itoss = 0
     end do
     if (itoss.eq.1) go to 135


     idiagbuf= 0
     diagbuf = 0.
     pred=0.0

      tbc     = 0.
      bcor    = 0.

       aivals(1,isz) = aivals(1,isz) + 1.
         

!    INITIAL PROCESSING OF SATELLITE DATA
!*****
!
!    Process data


!      Extract lon and lat.
       cenlat = rad_dat0(n)%lat
       cenlon = rad_dat0(n)%lon
       cenlatg= rad_dat0(n)%latg
       cenlong= rad_dat0(n)%long
       zasat  = rad_dat0(n)%zasat
       cosza  = cos(zasat)
       xpath  = 1./cosza        ! view angle path factor
       panglr = rad_dat0(n)%panglr
       cld    = rad_dat0(n)%cld

!      Extract nadir (scan step position)
       nadir  = rad_dat0(n)%nadir
       pangs  = rad_dat0(n)%pangs
       xpaths = rad_dat0(n)%xpaths
       sfchgt = rad_dat0(n)%sfchgt
       lndsea = rad_dat0(n)%lndsea

!          Load channel data into work array.
       do i = 1,nchan
          btm(i) = rad_dat0(n)%obsbt(i)
          tbcnob(i)=rad_dat0(n)%gesbt(i)
          pems(i)=rad_dat0(n)%pems(i)
          ts(i)=rad_dat0(n)%htlto(nsigx1,i)
          emissav(i)=rad_dat0(n)%emissav(i)
          tlapchn(i)=rad_dat0(n)%tlapchn(i)
          ichanthis=newchn(kidsat(isz),i,nusat,nuchan,jpch)
          itotal_in(ichanthis)=itotal_in(ichanthis)+1
       end do

!    Prepare for application of bias correction to simulated values.
!    Construct predictors for 1B radiance bias correction.

       pred(1) = 0.01
       pred(2) = .1*(xpath-1.)**2-.015
       pred(3) = 0.0

!          Save data in diagnostic arrays.
       idiagbuf(1) = kidsat(isz)
       idiagbuf(2) = nchan
       idiagbuf(5) = nadir
       diagbuf(1)  = cenlat
       diagbuf(2)  = cenlon
       diagbuf(3) = rad2dg*zasat
       diagbuf(5)  = pangs
       diagbuf(8)  = cld
       diagbuf(9) = pred(2)
       diagbuf(12) = 1.
       aivals(3,isz)  = aivals(3,isz) + 1

       keep = .true.
       lndsea = rad_dat0(n)%lndsea
       isflg  = rad_dat0(n)%isflg
       icst   = rad_dat0(n)%icst
       land = lndsea .eq. 1
       ice  = isflg .eq. 1
       coast= icst .eq. 1
!
!    Initial QC of data.  Set keep to false if we don't want the data.
!    The qc tests screen data over land, snow/ice, and coastal points.

!       Toss obs over land
        if (land) then
           keep = .false.
           aivals(4,isz)   = aivals(4,isz) + 1.
        end if

        if (ice) then
!
!       Toss obs over snow or ice
           keep = .false.
           aivals(5,isz) = aivals(5,isz) + 1.
        end if
        if (coast) then
!
!       Toss obs over coastal points
           keep = .false.
           aivals(6,isz)  = aivals(6,isz) + 1.
        end if

        ts5 = rad_dat0(n)%ts5

        if (hirs2 .or. goes .or. hirs3) then
!
!    GROSS QC Check.  Compare HIRS and GOES channel 8 with sst.  If the
!    difference is too large, flag the observation as "bad".

           ch8flg = btm(8) - ts5
           ch8ch10= btm(8) - btm(10)
           if(kidsat(isz) .eq. 11)ch8flg=ch8flg-2.19955+1.28578*ch8ch10
           if(kidsat(isz) .eq. 14)ch8flg=ch8flg-2.61089+1.32519*ch8ch10
           if(kidsat(isz) .eq. 15)ch8flg=ch8flg-1.85483+1.30573*ch8ch10
           if(kidsat(isz) .eq. 16)ch8flg=ch8flg-1.85483+1.30573*ch8ch10
           if(kidsat(isz) .eq. 58)ch8flg=ch8flg-2.09303+.277606*ch8ch10
           if(kidsat(isz) .eq. 60)ch8flg=ch8flg-4.22641+.331097*ch8ch10
           if (abs(ch8flg).gt.8.*r_infl) then
              keep = .false.
              aivals(10,isz)  = aivals(10,isz) + 1.
           endif
        endif
!
!       Save data in diagnostic arrays 
         zz = rad_dat0(n)%zz
         idiagbuf(3)= lndsea
         idiagbuf(4)= isflg
         diagbuf(4) = ts5
         diagbuf(6) = sfchgt
         diagbuf(7) = zz

!
!
!
!*****
!    IDENTIFY CHANNELS TO PROCESS
!*****
!     
!    Load arrays used by forward model to indicate which 
!    profiles and which channels to compute radiances for.
!
!    The tests below screen from the dataset those profiles/
!    channels which are not used (as of 3 Dec 01).
!
     kchan  = 0
     kochan = 0
     knchan = 0
     errf   = 0.0
     varinv = 2.e-12
     kuse=.false.
!
!    1B HIRS and GOES:  retain all channels over water
!                over land, retain channels 2-5, 12.
!
     if (hirs2 .or. hirs3 .or. goes) then
         do jc = 1,nchan
           kuse(jc)=(keep .or. jc.le.5 &
            .or. jc .eq. 12) .and. tnoise(jc).lt.1.e4
         end do
     elseif(msu .or. amsua .or. eos_amsua .or. amsub .or. hsb .or. goesimg)then
          do jc = 1,nchan
!
!    retain all data, provided assigned channel error
!           is "small"

           kuse(jc)= tnoise(jc) < 1.e4
          end do
     endif
     knchpf = 0
       kcount = 0
       do jc = 1,nchan
         if(kuse(jc))then
            kcount  = kcount+1
            knchpf  = knchpf+1
            kchan(knchpf)  = iochan(jc)
            kochan(knchpf) = jc
            varinv(jc)     = 1.0/tnoise(jc)**2
            errf(jc)       = tnoise(jc)
         endif
       end do
       knchan = kcount
     if (knchpf.lt.1) go to 130

!
!*****
!     COMPUTE AND APPLY BIAS CORRECTION TO SIMULATED VALUES
!*****

!    Compute predictor for AMSU-A cloud liquid water bias correction.

           if(mype.eq.0) write(0,*)' at 5.7 in setuprad_2, n=',n
     do i=1,knchpf
       tbc(kochan(i)) = tbcnob(kochan(i)) + cbias(nadir,kchan(i))
     end do
     if (amsua .or. eos_amsua) then
!          d0=8.240-(2.622-1.846*cosza)*cosza
           dqval=0.0
           if (tbc(1)<=284. .and. tbc(2)<=284. .and. keep) &
                dqval=d1*(tbc(1)-btm(1))/(285.-tbc(1))+ &
                         d2*(tbc(2)-btm(2))/(285.-tbc(2))
           pred(3) = dqval*cosza*cosza
     endif

!   Perform bias correction

     do i=1,knchpf
        m=kochan(i)
        mm=kchan(i)
        tlap = tlapchn(m)-tlapmean(mm)
        sum = tlap*(predx(mm,npred)+tlap*predx(mm,npred-1))
        bcor(1,m) = cbias(nadir,mm)             !global_satangbias
        bcor(2,m) = tlap*predx(mm,npred)        !tlap
        bcor(3,m) = tlap*tlap*predx(mm,npred-1) !tlap*tlap
        do j = 1,npred-2
           term = predx(mm,j)*pred(j)
           sum  = sum + term
           bcor(j+3,m) = term
        end do
        tbc(m) = tbc(m) + sum
     end do
!
!
!
!******
!     QC OBSERVATIONS BASED ON VARIOUS CRITERIA
!             Separate blocks for various instruments.
!******
!
!
!    QC HIRS/2, GOES, HIRS/3 data
!
           if(mype.eq.0) write(0,*)' at 5.8 in setuprad_2, n=',n
     if (hirs2 .or. goes .or. hirs3 .or. airs) then

     !default channel numbers
        ichan8 = 8
        ichan18 = 18
        ichan12 = 12
        ichan4  = 4
        ichan5  = 5

        ! use proxy channels for AIRS
        !============================
        if (airs) then
          ichan8 = 8
          ichan18 = 18
          ichan12 = 12
          ichan4  = 4
          ichan5  = 5
        endif

!
           efact   = 1.
           vfact   = 0.25
           efactir = 1.
           vfactir = 1.
           efacts  = 1.
           vfacts  = 1.
           vfact2  = 1.0
           vfact3  = 1.0
           vfact4  = 1.0
!
!          Apply window check.  Toss obs if fail window test.
           ch4flg  = btm(ichan4) - tbc(ichan4)
           ch5flg  = btm(ichan5) - tbc(ichan5)
           ch8flg  = btm(ichan8) - tbc(ichan8)
           ch12flg = btm(ichan12)- tbc(ichan12)
           ch18flg = btm(ichan18)- tbc(ichan18)
           demisf  = .01
           dtempf  = .5
!
!             Cloud test for channel 2
           if (ch4flg.lt.-20.0*r_infl) then
              vfact2 = 0.0
              vfact3 = 0.0
              vfact4 = 0.0
              vfactir = 0.0
              aivals(11,isz)   = aivals(11,isz) + 1.

           else if (ch4flg.lt. -1.0*r_infl) then
!
!           Cloud test for channel 3

              vfact3 = 0.0
              vfact4 = 0.0
              vfactir = 0.0
              aivals(12,isz)   = aivals(12,isz) + 1.
           else if(abs(ch4flg).gt. 1.0*r_infl .or. abs(ch5flg) .gt. 1.0*r_infl)then
              vfact4 = 0.0
              vfactir = 0.0
              aivals(13,isz) = aivals(13,isz) + 1.

           else if (ch12flg < -10.0*r_infl)then  !ch12 sees cloud
              vfactir = 0.0
              vfact4 = 0.0
              aivals(14,isz) = aivals(14,isz) + 1.

!           Channel 8 cloud test
!            + chan 18 cloud test?

           else if (abs(ch8flg) > 1.0*r_infl .or. .not. keep)then
!             .or. (pangs > 85. .and. abs(ch18flg) > 2.0*r_infl) )then
!test
!test
              vfactir = 0.0
              if(keep)aivals(15,isz) = aivals(15,isz) + 1.
           end if
!
!          Reduce qc bounds in tropics
           if ( (cenlat .gt. -25.) .and.&
                (cenlat .lt.  25.) ) then
              aivals(7,isz) = aivals(7,isz) + 1.
              efact   = (abs(cenlat)*.5/25.+.5)*efact
           endif
!
!          Reduce weight given to obs for shortwave ir if
!          solar zenith angle small
           if (pangs .le. 60. ) then
              vfacts = .5*vfacts
              efacts = .5*efacts
           endif
           if(ice)then
             demisf = .01
             dtempf = 3.0
           else if(land)then
             demisf = .01
             dtempf = 2.0
           end if
!
!
!          Reduce weight for obs over higher topography
           if (sfchgt .gt. 2000.) then
              fact    = 2000./sfchgt
              efactir = fact*efactir
              vfact4  = fact*vfact4
!             if(sfchgt > 2000)then
!               vfact4 = 0.0
!             end if
!              write(6,*)'qc:  height=',mype,n,sfchgt,fact,efactif
           endif
!
!          Reduce weight if model and obs topography too different
           if (abs(zz-sfchgt) .gt. 200.) then
              fact    = 200./abs(zz-sfchgt)
              efactir = fact*efactir
              vfact4  = fact*vfact4
              aivals(8,isz)   = aivals(8,isz) + 1.
           endif
!
!
!
!             Generate q.c. bounds and modified variances.
           do ll=1,knchan
              l = kochan(ll)
              varinv(l)=vfact*varinv(l)
              errf(l)=3.*efact*errf(l)

!             Channel 2 test

              if (l.eq.2) then

                 varinv(l) = vfact2*varinv(l)
!
!                Channel 3 test

              else if (l.eq.3) then

                 varinv(l) = vfact3*varinv(l)

!                Channel 4 and Channel 12 test

              else if (l.eq.4 .or. l .eq. 12) then

                 varinv(l) = vfact4*varinv(l)

              else if (l .ne. 1)then

!             Channels 4 and above
                 errf(l)   = efactir*errf(l)
                 varinv(l) = vfactir*varinv(l)
                 if (l .ge. 13) then
                    errf(l)   = efacts*errf(l)
                    varinv(l) = vfacts*varinv(l)
                 end if
              end if
 ! incorrect  dtbf = demisf*abs(pems(l))+dtempf*abs(ts(l))
              dtbf = demisf*abs(pems(ll))+dtempf*abs(ts(ll)) ! correct (dp 04/09/2003)
    ! old     varinv(l)=1./(1./varinv(l)+dtbf**2)
              term = dtbf*dtbf                             ! new
              if (varinv(l)>1.e-12 .and. term>1.e-12) &    ! corresponds to new global 
                   varinv(l)=1./(1./varinv(l)+term)        ! radiance code (dp 04/09/2003)
!             emix=abs(pems(l))
!             if(emix > .3 .and. (land .or. ice &
!                ) .and. varinv(l) > 1.e-9)then
!             print *,' emiss ',l,ll,emix,lndsea,isflg
!             varinv(l)=0.0
!             end if

           end do
           if(mype.eq.0) write(0,*)' at 5.9 in setuprad_2, n=',n
!
!
!    End of HIRS and GOES QC blocks

!
!
!    QC MSU data
     else if (msu) then
!
           efact   = 1.
           vfact   = 0.25
           efactmc = 1.
           efact21 = 1.
           efact22 = 1.
           vfactmc = 1.
           vfact21 = 1.
           vfact22 = 1.
!
!          Reduce qc bounds in tropics
           if ( (cenlat .gt. -25.) .and.&
                (cenlat .lt.  25.) ) then
              aivals(7,isz) = aivals(7,isz) + 1.
              efact   = (abs(cenlat)*.5/25.+.5)*efact
           endif
           demisf = .015
           dtempf = 0.5
!
!          Reduce q.c. bound over land
           if (land) then
!             efact22 = 0.0
!             vfact22 = 0.0
              demisf = .03
              dtempf = 2.5
           end if
           if (coast) then
!             efact22 = 0.0
!             vfact22 = 0.0
              demisf = .20
              dtempf = 4.5
           end if
!
!          Over water apply window test to channel 2 using channel 1
           if (.not. land .or. coast) then
              if (abs(btm(1) - tbc(1)).gt.5.0*r_infl) then
                 efact22 = 0.0
                 vfact22 = 0.0
                 aivals(10,isz)   = aivals(10,isz) + 1.
              endif
           endif
!
!          Reduce q.c. bound for likely snow or ice
           if (ice) then
!             efact22 = .5*efact22
!             vfact22 = .5*vfact22
              demisf = .05
              dtempf = 3.0
           end if
!
!
!          Reduce q.c. bounds over higher topography
           if (sfchgt > 2000.) then
              fact = 2000./sfchgt
              efactmc = fact*efactmc
              vfactmc = fact*vfactmc
           end if
!    
!          Reduce q.c. bounds if model and obs topography
!          too different.
           if (abs(zz-sfchgt) > 200.) then
              fact = 200./abs(zz-sfchgt)
              efactmc = fact*efactmc
              vfactmc = fact*vfactmc
              aivals(8,isz)   = aivals(8,isz) + 1.
           end if
!    
!          Generate q.c. bounds and modified variances.
           errf(1) = efactmc*efact21*errf(1)
           errf(2) = efactmc*efact22*errf(2)
           errf(3) = 2.0*efactmc*errf(3)
           errf(4) = 2.0*errf(4)
           varinv(1) = vfactmc*vfact21*varinv(1)
           varinv(2) = vfactmc*vfact22*varinv(2)
           varinv(3) = vfactmc*varinv(3)
           varinv(4) = vfactmc*varinv(4)
           do ll=1,knchan
              l = kochan(ll)
              errf(l)   = 3.*efact*errf(l)
              varinv(l) = vfact*varinv(l)
  !incorrect  emix=abs(pems(l))
  !incorrect  dtbf = demisf*abs(pems(l))+dtempf*abs(ts(l))
              emix=abs(pems(ll))                             !  correct (dp 04/09/2003)
              dtbf = demisf*abs(pems(ll))+dtempf*abs(ts(ll)) !  correct (dp 04/09/2003)
              if(l == 1)diagbuf(9) = dtbf
              if(l == 2)diagbuf(10) = dtbf
              if(l == 3)diagbuf(11) = dtbf
    ! old     varinv(l)=1./(1./varinv(l)+dtbf**2)
              term = dtbf*dtbf                             ! new
              if (varinv(l)>1.e-12 .and. term>1.e-12) &    ! corresponds to new global 
                   varinv(l)=1./(1./varinv(l)+term)        ! radiance code (dp 04/09/2003)
!             if(emix > .3 .and. (land .or. ice &
!                ) .and. varinv(l) > 1.e-9)then
!             print *,' emiss msu',l,ll,emix,lndsea,isflg
!             varinv(l)=0.0
!             end if
           end do
!    
!
!    End of MSU QC block
!
!
!    QC AMSU-A data
     else if (amsua .or. eos_amsua) then

          if(knchan .gt. 0)then
           bcbtm1=btm(1)  -cbias(nadir,ich(1))
!          bcbtm2=btm(2)  -cbias(nadir,ich(2))
!          bcbtm15=btm(15)-cbias(nadir,ich(15))
           ch1dbc=btm(1)-tbc(1)
           ch2dbc=btm(2)-tbc(2)
           ch4dbc=btm(4)-tbc(4)
           ch6dbc=btm(6)-tbc(6)
           ch15dbc=btm(15)-tbc(15)
!          sval=-113.2+(2.41-.0049*bcbtm1)*bcbtm1+.454*bcbtm2-bcbtm15
           dsval=(2.41-.0098*bcbtm1)*ch1dbc+.454*ch2dbc-ch15dbc

!          factch6x=((sval-5.)/10.)**2+(ch6dbc/.8)**2
           factch6=(dsval/10.)**2+(ch6dbc/.8)**2
           factch4=(cosza*dqval/.3)**2+(ch4dbc/1.8)**2

!          QC based on ratio of obs-ges increment versus the sensitivity of
!          the simulated brightness temperature to the surface emissivity
!          Y2K hurricane season runs by QingFu Liu found the hurricane
!          forecast tracks to be degraded without this QC.
!
           dtde1 = pems(1)
           de1   = 0.
           if (dtde1 /= 0.) de1=abs((btm(1)-tbcnob(1))/dtde1)
           dtde2 = pems(2)
           de2   = 0.
           if (dtde2 /= 0.) de2=abs((btm(2)-tbcnob(2))/dtde2)
           dtde3 = pems(3)
           de3   = 0.
           if (dtde3 /= 0.) de3=abs((btm(3)-tbcnob(3))/dtde3)
           diagbuf(8)  = factch4
!          diagbuf(9)  = de1
!          diagbuf(10) = de2
!          diagbuf(11) = de3


           efact   = 1.0
           efact6  = 1.0
           efact7  = 1.0
           efactmc = 1.0
           vfact   = 1.0
           vfact6  = 1.0
           vfact7  = 1.0
           vfactmc = 1.0
           demisf = .01
           dtempf = .5

           if (ice) then

!          Decrease likelyhood of using data over snow and ice

              demisf = .05
              dtempf = 3.0
           else if(land)then
!
!          Decrease likelyhood and weight of data over land

              demisf = .02
              dtempf = 2.0
           end if
!
!          Reduce qc bounds in tropics
           if ( (cenlat > -25.) .and.&
                (cenlat <  25.) ) then
              aivals(7,isz) = aivals(7,isz) + 1.
              efact   = (abs(cenlat)*.25/25.+.75)*efact
           endif
!
           if(factch6 >= 1.*r_infl**2 .or. coast)then
             efactmc=0.0
             vfactmc=0.0
             efact6=0.0
             vfact6=0.0
             if(.not. coast)aivals(11,isz) = aivals(11,isz) + 1.

           else if (factch4 > 1.*r_infl**2 .or. de2 > .03*r_infl .or.  &
                 de3 > .05*r_infl .or. de1 > .05*r_infl) then
!
              efactmc=0.0
              vfactmc=0.0
              if(.not. coast .and. factch4 > 1.*r_infl**2) &
                     aivals(10,isz) = aivals(10,isz) + 1.
           else if (abs(zz-sfchgt) > 200.) then
!     
!          Reduce q.c. bounds if model and obs topography
!          too different.

              fact = 200./abs(zz-sfchgt)
              efactmc = fact*efactmc
              vfactmc = fact*vfactmc
              aivals(8,isz)   = aivals(8,isz) + 1.
           end if
           if (sfchgt > 2000.) then
!
!          Reduce q.c. bounds over higher topography

              fact = 2000./sfchgt
              efactmc = fact*efactmc
              efact6 = fact*efact6
              vfactmc = fact*vfactmc
              vfact6 = fact*vfact6
              if(sfchgt > 4000.)then
                fact = 4000./sfchgt
                efact7 = fact*efact7
                vfact7 = fact*vfact7
              end if
           end if
!
!          Generate q.c. bounds and modified variances.
           do ll=1,knchan
              l = kochan(ll)
              errf(l)   = 3.*efact*errf(l)
              varinv(l) = vfact*varinv(l)
              if(l <= 5 .or. l == 15)then

!          Adjust observation error based on magnitude of liquid water correction
!                  0.2 is empirical factor

                 cor=0.2*(predx(ich(l),3)*pred(3))**2
                 varinv(l) = varinv(l)/(1.+varinv(l)*cor)

                 errf(l)   = efactmc*errf(l)
                 varinv(l) = vfactmc*varinv(l)
              else if(l == 6)then
                 errf(l)=efact6*errf(l)
                 varinv(l) = vfact6*varinv(l)
              else if(l == 7)then
                 errf(l)=efact7*errf(l)
                 varinv(l) = vfact7*varinv(l)
              end if
 ! incorrect  dtbf = demisf*abs(pems(l))+dtempf*abs(ts(l))
              dtbf = demisf*abs(pems(ll))+dtempf*abs(ts(ll))  ! correct (dp 04/09/2003)
    ! old     varinv(l)=1./(1./varinv(l)+dtbf**2)
              term = dtbf*dtbf                             ! new
              if (varinv(l)>1.e-12 .and. term>1.e-12) &    ! corresponds to new global 
                   varinv(l)=1./(1./varinv(l)+term)        ! radiance code (dp 04/09/2003)
!             if(l == 3)diagbuf(8) = dtbf
              if(l == 4)diagbuf(9) = dtbf
              if(l == 5)diagbuf(10) = dtbf
              if(l == 6)diagbuf(11) = dtbf

              if ( (l <= 5 .or. l == 15) .and. &
                   (varinv(l)<1.e-9) ) pred(3) = 0.0

           end do
          end if
!
!
!    End of AMSU-A QC block
!
!    QC AMSU-B data

     else if (amsub .or. hsb) then
!
         if(knchan > 0)then
           ch1diff=btm(1)-tbc(1)
           ch2diff=btm(2)-tbc(2)
           efact   = 1.0
           efactmc = 1.0
           vfact   = 1.0
           vfactmc = 1.0
           if(land)then
            dsi=0.85*ch1diff-ch2diff
            si=42.72+0.85*btm(1)-btm(2)
            demisf = .03
            dtempf = 2.0
            else
            dsi=999.
            if(btm(2) < 300.)then
             dsi=0.13*(ch1diff-33.58*ch2diff/(300.-btm(2)))
            end if
            si=42.72+0.85*ch1diff-ch2diff
            demisf = .015
            dtempf = 0.5
           end if
           if(ice)then
            demisf = .05
            dtempf = 3.0
           end if
           if(coast)then
            demisf = .25
            dtempf = 5.0
           end if
           diff1=ch1diff-7.5*dsi
           fact1=(diff1/10.)**2+(dsi/1.)**2
           diagbuf(9) = diff1
           diagbuf(10) = dsi
           diagbuf(11) = fact1
!
!          Reduce q.c. bounds over higher topography
           if (sfchgt > 2000.) then
              fact = 2000./sfchgt
              efactmc = fact*efactmc
              vfactmc = fact*vfactmc
           end if
!    
!          Reduce q.c. bounds if model and obs topography
!          too different.
           if (abs(zz-sfchgt) > 200.) then
              fact = 200./abs(zz-sfchgt)
              efactmc = fact*efactmc
              vfactmc = fact*vfactmc
              aivals(8,isz)   = aivals(8,isz) + 1.
           end if

           if(fact1 > 1.*r_infl**2)then
            efactmc=0.0
            vfactmc=0.0
            if(coast)aivals(10,isz) = aivals(10,isz) + 1.
           else
            efact = (1.-fact1*fact1)*efact
            vfact = (1.-fact1*fact1)*vfact
           end if
!    
!          Generate q.c. bounds and modified variances.
           do ll=1,knchan
              l = kochan(ll)
              errf(l)   = 3.*efact*efactmc*errf(l)
              varinv(l) = vfact*vfactmc*varinv(l)
  ! incorrect dtbf = demisf*abs(pems(l))+dtempf*abs(ts(l))
              dtbf = demisf*abs(pems(ll))+dtempf*abs(ts(ll))  ! correct (dp 04/09/2003)
    ! old     varinv(l)=1./(1./varinv(l)+dtbf**2)
              term = dtbf*dtbf                             ! new
              if (varinv(l)>1.e-12 .and. term>1.e-12) &    ! corresponds to new global 
                   varinv(l)=1./(1./varinv(l)+term)        ! radiance code (dp 04/09/2003)
           end do
!    
        end if
!
!    End of AMSU-B QC block
!
!    GOES imager Q C
!
     else if(goesimg)then
         if(knchan > 0)then
           efact = 1.0
           vfact = 1.0
           fact45 = 1.0
           fact2 = 1.0
           demisf = .01
           dtempf = 0.5
           if(land)then
            fact45=0.
            fact2=0.
            demisf = .01
            dtempf = 2.0
           end if
           if(ice)then
            fact45=0.
            fact2=0.
            demisf = .02
            dtempf = 3.0
           end if
           if(coast)then
            fact45=0.
            fact2=0.
            demisf = .02
            dtempf = 5.0
           end if
!
!    
!          Reduce weight for obs over higher topography

           ch2diff  = btm(2) - tbc(2)
           ch4diff  = btm(4) - tbc(4)
           ch5diff  = btm(5) - tbc(5)
           if(rad2dg*zasat >60.)then
              vfact=0.
              aivals(10,isz)= aivals(10,isz) + 1.
           end if
           if (sfchgt > 2000.) then
              fact    = 2000./sfchgt
              efact   = fact*efact
              vfact   = fact*vfact
              aivals(11,isz)= aivals(11,isz) + 1.
!             if(sfchgt > 2000)then
!               vfact4 = 0.0
!             end if
!              write(6,*)'qc:  height=',mype,n,sfchgt,fact,efactif
           end if
           if(abs(ch4diff) > 4.0*r_infl .or. abs(ch5diff) > 4.0*r_infl)then
              fact45 = 0.
              aivals(12,isz)= aivals(12,isz) + 1.
           end if
           if(pangs >= 60. )then
             if(abs(ch2diff) > 3.0*r_infl)then
              fact45 = 0.
              aivals(13,isz)= aivals(13,isz) + 1.
             end if
           else
             fact2 = 0.
           end if

!    
!          Generate q.c. bounds and modified variances.
           do ll=1,knchan
              l = kochan(ll)
              errf(l)   = 3.*efact*errf(l)
              varinv(l) = vfact*varinv(l)
              if(l == 4 .or. l == 5)varinv(l) = fact45*varinv(l)
              if(l == 2)varinv(l) = fact2*fact45*varinv(l)
 ! incorrect  dtbf = demisf*abs(pems(l))+dtempf*abs(ts(l))
              dtbf = demisf*abs(pems(ll))+dtempf*abs(ts(ll))  ! correct (dp 04/09/2003)
    ! old     varinv(l)=1./(1./varinv(l)+dtbf**2)
              term = dtbf*dtbf                             ! new
              if (varinv(l)>1.e-12 .and. term>1.e-12) &    ! corresponds to new global 
                   varinv(l)=1./(1./varinv(l)+term)        ! radiance code (dp 04/09/2003)
              if(l == 4)diagbuf(7) = pems(l)
              if(l == 4)diagbuf(9) = ts(l)
              if(l == 5)diagbuf(10) = pems(l)
              if(l == 5)diagbuf(11) = ts(l)
           end do
!
        end if

     end if
           if(mype.eq.0) write(0,*)' at 5.11 in setuprad_2, n=',n

!                      sanity check
          do k=1,knchpf
           if(btm(kochan(k)).lt.50..or.btm(kochan(k)).gt.450.) then
                    insane(kochan(k),isz)=insane(kochan(k),isz)+1
                    varinv(kochan(k))=2.e-12
           end if
          end do

!
!
!
!    Apply gross check to observations.  Toss obs failing test.
     do k = 1,knchpf
        if (varinv(kochan(k)) .gt. 1.e-6) then
           drad = abs(btm(kochan(k)) - tbc(kochan(k)))
!
!          If mean obs-ges difference around observations
!          location is too large and difference at the
!          observation location is similarly large, then
!          toss the observation.

           if (drad > errf(kochan(k)) ) then
              varinv(kochan(k)) = 0.
              stats(2,kchan(k)) = stats(2,kchan(k)) + 1.
              aivals(9,isz) = aivals(9,isz) + 1.
           end if
!
        end if
     end do
     if(amsua .or. amsub .or. msu .or. hsb)then
       if(amsua .or. eos_amsua)nlev=6
       if(amsub)nlev=5
       if(hsb)nlev=4
       if(msu)nlev=4
         kval=0
         do k=2,nlev
           if(varinv(k) < 1.e-6)then
             kval=k-1
             if(amsub .or. hsb)kval=nlev
           end if
         end do
         if(kval > 0)then
           do k=1,kval
             varinv(k)=0.
           end do
           if(amsua)varinv(15)=0.
         end if
     end if
     do k = 1,knchpf
        mm = kochan(k)
        if (varinv(mm) > 1.e-6) then
!
!             Accumulate data for diagnostic print.

          drad = btm(mm) - tbc(mm)
          dradnob = btm(mm) - tbcnob(mm)
          varrad = drad*varinv(mm)

          m = kchan(k)

          if (iuse(m) == 1)aivals(2,isz) = aivals(2,isz) + drad*varrad

          stats(1,m)  = stats(1,m) + 1.                     !number of obs
          stats(3,m)  = stats(3,m) + drad                   !obs-mod(w_biascor)
          stats(4,m)  = stats(4,m) + drad*drad              !(obs-mod(w_biascor))**2
          stats(5,m)  = stats(5,m) + drad*varrad            !penalty contribution
          stats(6,m)  = stats(6,m) + dradnob                !obs-mod(w/o_biascor)

          do j=1,npred+1               !jj=7=global_satangbias.txt
                                       !jj=8=tlap,  jj=9=tlap*tlap
                                       !jj=10=mean, jj=11=xpath, jj=12=clw
             jj = j+6
             jjj=jj+6                  !jjj=jj+6 is square of jj term

             term = bcor(j,mm)
             stats(jj, m)= stats(jj, m) + term
             stats(jjj,m)= stats(jjj,m) + term*term

          end do

        endif
     end do


!
!
!
!******
!     CONSTRUCT SENSITIVITY VECTORS.  WRITE TO OUTPUT FILE.
!******
!
!    Generate o3, t, q, and ts sensitivity arrays.

        icc=0
        do k = 1,knchan
           kk = k
           j  = kochan(kk)
           jo = kchan(kk)

!          Only "good" obs are included in calculation.

           if (varinv(j) .gt. 1.e-6 .and. iuse(jo)==1 ) then

              icc      = icc+1
              icx(icc) = jo
              var(icc) = varinv(j)
              obsbt(icc)  = btm(j)
              gesbt(icc)  = tbcnob(j)
               if(ibias_loop.eq.nbias_loop) gesbt(icc) = tbc(j)
              pred(npred-2+icc)=tlapchn(j)-tlapmean(jo)

              if(ibias_loop.eq.nbias_loop) then
               htlto(:nsigx1,icc)=rad_dat0(n)%htlto(:nsigx1,j)

!             If we do not have channel 9, turn off ozone sensitivity.
!             We think we need this when we use the new bias correction
               if ((hirs2 .or. goes .or. hirs3) .and. (varinv(9).lt.1.e-6))then
                 do i = 1,nsig
                   htlto(2*nsig+i,icc) = 0.0
                 end do
               endif

              end if


           end if

!       End loop over channels.
        end do
          if(mype.eq.0) write(0,*)' at 5.14 in setuprad_2, n=',n
!
!   Load data into output arrays
        if (icc>0) then
           mrad_dat=min(mrad_dat+1,nrad_dat)

!          transfer data to output holding array

           ncc = icc
           rad_dat(mrad_dat)%type=rad_dat0(n)%type
           rad_dat(mrad_dat)%group=rad_dat0(n)%group
           rad_dat(mrad_dat)%lon=cenlon
           rad_dat(mrad_dat)%lat=cenlat
           rad_dat(mrad_dat)%long=cenlong
           rad_dat(mrad_dat)%latg=cenlatg
           rad_dat(mrad_dat)%time=rad_dat0(n)%time
           rad_dat(mrad_dat)%nsig=nsig
           if(ibias_loop.lt.nbias_loop) then
            rad_dat(mrad_dat)%nsigx1=nadir
           else
            rad_dat(mrad_dat)%nsigx1=2*nsig+1
           end if
           rad_dat(mrad_dat)%ncc=ncc
           rad_dat(mrad_dat)%npred=rad_dat0(n)%npred
           rad_dat(mrad_dat)%label=rad_dat0(n)%label
           rad_dat(mrad_dat)%kpbot=rad_dat0(n)%kpbot

           rad_dat(mrad_dat)%icx(:ncc)=icx(:ncc)
           rad_dat(mrad_dat)%var(:ncc)=var(:ncc)/(saterr_runfact**2)
           rad_dat(mrad_dat)%pred(:npred+ncc-1)=pred(:npred+ncc-1)
           rad_dat(mrad_dat)%obsbt(:ncc)=obsbt(:ncc)
           rad_dat(mrad_dat)%gesbt(:ncc)=gesbt(:ncc)

           if(ibias_loop.eq.nbias_loop) then
            rad_dat(mrad_dat)%pressure(:nsig)=rad_dat0(n)%pressure(:nsig)
            rad_dat(mrad_dat)%iwgts(:)=rad_dat0(n)%iwgts(:)
            rad_dat(mrad_dat)%wgts(:)=rad_dat0(n)%wgts(:)
            do k=1,ncc
             rad_dat(mrad_dat)%htlto(1:nsigx1,k)=htlto(1:nsigx1,k)
            end do
           end if
!
        end if

        if (diagsave) then
!
!    Write diagnostics to output file.  Only generate
!    file on first outer iteration and first bias spinup iteration.

            do nn=1,nchan
               tmpvar(nn) = varinv(nn)
               if (iouse(nn)<1) tmpvar(nn)=-tmpvar(nn)
            end do
            write(4) mype,(idiagbuf(nn),nn = 1,5),&
                   (diagbuf(nn),nn = 1,12),&
                   (float(nn),btm(nn),&
                   tbc(nn),tbcnob(nn),tmpvar(nn),&
                   emissav(nn),tlapchn(nn),nn=1,nchan)
        endif
!


130  continue        !   all channels rejected for this obs

135  continue        !  here if skip this obs
  end do     !       END OF MAIN LOOP OVER SATELLITE DATA
 end if

! Close unit to diagnostic file.
  if (diagsave)close(4)

!
! Collect statistics

  call mpi_reduce(aivals,aivals1,15*nsat,mpi_real4,mpi_sum,mype_rad,my_comm,ierror)
  call mpi_reduce(stats,stats1,nstats*jpch,mpi_real4,mpi_sum,mype_rad,my_comm,ierror)
  call mpi_reduce(kidsat,kidsat1,nsat,mpi_integer4,mpi_max,mype_rad,my_comm,ierror)

!         get total number of input points

  call mpi_allreduce(itotal_in,itotal_in0,jpch,mpi_integer4,mpi_sum,my_comm,ierr)


!    Compute and print statistics for current satellite.
  if(mype==mype_rad) then
       write(iout_rad, &
           '(" SETUPRAD_2 INFO FOR ibias_loop,nbias_loop = ",2i4)') &
                 ibias_loop,nbias_loop
   penalty_all=0.0
   do is=1,nsat
     obstype=sattype(is)
!
!    Initialize logical flags for satellite platform

     hirs2      = obstype == 'hirs/2'
     hirs3      = obstype == 'hirs/3'
     msu        = obstype == 'msu'
     goes       = obstype == 'goes'
     amsua      = obstype == 'amsua'
     amsub      = obstype == 'amsub'
     airs       = obstype == 'airs'
     hsb        = obstype == 'hsb'
     eos_amsua  = obstype == 'eos_amsua'
     goesimg    = obstype == 'goesimg'

     iobs2 = nint(aivals1(1,is))
     penalty_all=penalty_all+aivals1(2,is)
     if(iobs2 > 0)then

      iobs1 = nint(aivals1(3,is))
      iland   = nint(aivals1(4,is))
      isnoice = nint(aivals1(5,is))
      icoast  = nint(aivals1(6,is))
      ireduce = nint(aivals1(7,is))
      itopo   = nint(aivals1(8,is))
      ivarl   = nint(aivals1(9,is))
      igross  = nint(aivals1(10,is))

      if(hirs2 .or. hirs3 .or. goes .or. airs)then
       ich2    = nint(aivals1(11,is))
       ich3    = nint(aivals1(12,is))
       ich4    = nint(aivals1(13,is))
       ich12   = nint(aivals1(14,is))
       ich8    = nint(aivals1(15,is))
       write(iout_rad,2000) 'sat','type','num','numw','ich2','ich3', &
                            'ich4','ich8','ich12'
       write(iout_rad,2010) kidsat1(is),sattype(is),iobs1,iobs2,ich2,ich3, &
                            ich4,ich8,ich12
       write(iout_rad,2012) 'penalty','iland','isnoice','ireduce','icoast', &
                            'itopo','ivarl','igross'
       write(iout_rad,2011) aivals1(2,is),iland,isnoice,ireduce,icoast,itopo,ivarl,igross
      else if(msu)then
       ich21   = nint(aivals1(10,is))
       write(iout_rad,2000) 'sat','type','num','numw','ich21'
       write(iout_rad,2010) kidsat1(is),sattype(is),iobs1,iobs2,ich21
       write(iout_rad,2012) 'penalty','iland','isnoice','ireduce','icoast', &
                            'itopo','ivarl'
       write(iout_rad,2011) aivals1(2,is),iland,isnoice,ireduce,icoast,itopo,ivarl
      else if(amsua .or. eos_amsua)then
       iemiss = nint(aivals1(10,is))
       iice   = nint(aivals1(11,is))
       write(iout_rad,2000) 'sat','type','num','numw','ch4','ice'
       write(iout_rad,2010) kidsat1(is),sattype(is),iobs1,iobs2,iemiss,iice
       write(iout_rad,2012) 'penalty','iland','isnoice','ireduce','icoast', &
                            'itopo','ivarl'
       write(iout_rad,2011) aivals1(2,is),iland,isnoice,ireduce,icoast,itopo,ivarl
      else if(amsub .or. hsb)then
       ich1   = nint(aivals1(10,is))
       write(iout_rad,2000) 'sat','type','num','numw','ich1'
       write(iout_rad,2010) kidsat1(is),sattype(is),iobs1,iobs2,ich1
       write(iout_rad,2012) 'penalty','iland','isnoice','ireduce','icoast', &
                            'itopo','ivarl'
       write(iout_rad,2011) aivals1(2,is),iland,isnoice,ireduce,icoast,itopo,ivarl
      else if(goesimg)then
       iang   = nint(aivals1(10,is))
       ich1   = nint(aivals1(11,is))
       ich2   = nint(aivals1(12,is))
       ich5   = nint(aivals1(13,is))
       write(iout_rad,2000) 'sat','type','num','numw','hi topo','ch45','ch2'
       write(iout_rad,2010) kidsat1(is),sattype(is),iobs1,iobs2,ich1,ich2,ich5
       write(iout_rad,2012) 'penalty','iland','isnoice','ireduce','icoast', &
                            'itopo','ivarl','iang'
       write(iout_rad,2011) aivals1(2,is),iland,isnoice,ireduce,icoast,itopo,ivarl,iang
      else if (airs) then
       write(iout_rad,2990)
      else
       write(iout_rad,2999)
      end if
      write(iout_rad,'(/)')
     end if

  end do
                             write(0,*)' at 16 in setuprad_2, mype=',mype
  write(iout_rad,*)'total penalty_all=',penalty_all

!       Print counts, bias, rms, stndev as a function of channel.
  rcount=0.0; rcount_all=0.0; rpenal=0.0 ; rcount_allall=0.
  ifirst=.false.
  kfirst=.false.
  if (first) then
     lnbias=59
     open(lnbias,file='satbias_pred',form='unformatted')
  end if
  do i = 1,jpch
     isum = nint(stats1(1,i))
     ksum=itotal_in0(i)
     jsat = nusat(i)
     rcount_allall(jsat) = rcount_allall(jsat) + ksum
     if(ksum.gt.0) kfirst(jsat)=.true.
     if (isum > 0) then
        svar=varch(i)
        rcount_all(jsat) = rcount_all(jsat) + isum
        if (iuse(i)==1) then
           rcount(jsat) = rcount(jsat) + isum
           rpenal(jsat) = rpenal(jsat) + stats1(5,i)
        else
           svar=-svar
        end if
        ifirst(jsat)=.true.
        rsum = 1./float(isum)
        icerr = nint(stats1(2,i))
        do j=3,6   ! j=3=obs-mod(w_biascor)
                   ! j=4=(obs-mod(w_biascor))**2
                   ! j=5=penalty contribution
                   ! j=6=obs-mod(w/o_biascor)

           stats1(j,i) = stats1(j,i)*rsum
        end do
        stats1(4,i) = sqrt(stats1(4,i))
        if (isum > 1) then
!dule           stdev  = sqrt(stats1(4,i)*stats1(4,i)-stats1(3,i)*stats1(3,i))
           stdev  = sqrt(abs(stats1(4,i)*stats1(4,i)-stats1(3,i)*stats1(3,i)))
        else
           stdev = 0.
        end if
        write(iout_rad,1102) i,nuchan(i),nusat(i),isum,icerr,svar,&
             stats1(6,i),stats1(3,i),stats1(5,i),stats1(4,i),stdev
! Print average magnitude of correction for each bias correction term.
!  j=7=satangbias.txt, j=8=tlap, j=9=tlap*tlap, j=10=mean, j=11=xpath, j=12=clw
!  j=13 through j=18 are square of j=7 through j=12
        svar4=svar
        do j=7,nstats
           bcor4(j-6)=stats1(j,i)
        end do
        if(first)write(lnbias) i,nuchan(i),nusat(i),isum,svar4,bcor4
     else
        icerr = nint(stats1(2,i))
        svar4=-99.
        if(ksum.gt.0) write(iout_rad,1102) i,nuchan(i),nusat(i),isum,icerr,-varch(i),&
             svar4,svar4,svar4,svar4,svar4
        if(first)write(lnbias) i,nuchan(i),nusat(i),isum,svar4,bcor4
     endif
  end do

  if(first)close(lnbias)

! Write obs count to runtime output file
  write(iout_rad,1109)
  do i=1,jpch
     jsat=nusat(i)
     if (kfirst(jsat)) then
        kfirst(jsat)=.false.
        cpen=0.0
        if (rcount(jsat)>0.) cpen=rpenal(jsat)/rcount(jsat)
        write(iout_rad,1115) jiter,jsat,rcount_all(jsat),rcount(jsat),&
             rpenal(jsat),cpen,rcount_allall(jsat)
     endif
  end do
2000 format(a7,2x,A4,6x,8(a7,1x))
2010 format(i7,1x,A10,1x,8(i7,1x))
2011 format(3x,f16.8,7(i7,1x))
2012 format(7x,A7,5x,7(a7,1x))
2990 format(' airs data not implemented at this time ')
2999 format(' Illegal satellite type ')
1102          format(1x,2i3,i4,2i6,1x,f8.3,1x,6(f9.4,1x))
1109 format(t5,'it',t13,'sat',t20,'num all',t31,'num use',t41,'penalty',t56,&
          'cpen',t65,'total available')
1115 format('o-g',1x,i2.2,1x,'rad',2x,i3,2x,f9.0,2x,f9.0,2x,g12.6,2x,g12.6,2x,f9.0)


!    End of diagnostic print block.
  if(ibias_loop.eq.nbias_loop) close(iout_rad)
  end if
                             write(0,*)' at 17 in setuprad_2, mype=',mype

! End of routine


!        print out number of insane points

  call mpi_allreduce(insane,insane0,jpchus*nsat,mpi_integer4,mpi_sum,my_comm,ierr)

  if(mype.eq.0) then
   do k=1,nsat
    do j=1,jpchus
     if(insane0(j,k).gt.0) print *,' num insane points for chan ', &
          j,', sat =',kidsat1(k),' is ',insane0(j,k)
    end do
   end do
  end if


!   update bias information

  if(ibias_loop.lt.nbias_loop) then
   do kbias_loop=1,mbias_loop
                             write(0,*)' at 19 in setuprad_2, mype,j,kbias_loop=',mype,j,kbias_loop
    call update_satbias(allbias_new,allbias_old,errbias0, &
                       rad_dat,nrad_dat,mrad_dat,nangs_max,npred,jpch,mype)
   end do
   do j=1,jpch
    call blend_satbias(allbias_new(j),allbias_old(j),idate5, &
                       tbias,tcbias,nangs_max,dt_assimilation)
   end do
  end if
             

end do         !  end of bias-spinup loop

return
end subroutine setuprad_2
