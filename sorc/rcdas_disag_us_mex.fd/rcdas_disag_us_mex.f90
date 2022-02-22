!&&&&&&&& nx_us_mex
! PROGRAM DOCUMENTATION BLOCK
!
!PROGRAM: narr_disag_us_mex
!  PRGMMR:  M. CARRERA               ORG: W/NP52    DATE: 2005-04
!
!ABSTRACT: 
!   Disaggregates us_mex precip using hourly weights from CMORPH.
!
!   The us_mex precip data is a daily average that we want to convert to
!hourly values.  Use hourly CMORPH data to calculate time weights.
!
!            :(1)  IF CMORPH_SUM=0.0 BUT US_MEX P > 0 THEN
!            : WE ATTEMPT TO CALCULATE THE HOURLY WEIGHTS BY
!            : AVERAGING THE SURROUNDING WEIGHTS
!            :(2) IF WE HAVE UNDEFINED CMORPH VALUES THEN
!            : WE ASSUME EQUAL WEIGHTS OF 1/24 FOR EACH HOUR
!
!Program History log:
!   2005-04   M. Carrera
!   2007-02   W. Ebisuzaki change in file_to_process format, close(1), rename -> date(*), 
!                change to f95
!   2013-01    W. Ebisuzaki changed from f95 to f90
!
!USAGE:
!
!   fort.1 has list of input files
!
!   % rcdas_disag_us_mex
!
!
!INPUT FILES:
!    UNIT1 with list of input files
!    UNIT2 dynamic
!
!OUTPUT FILES:
!    UNIT3 date(iii)//'.us_mex'
!    UNIT51  text file: undefined_cmorph
!    UNIT52  text file: bad.sum.weights
!    UNIT53  text file: cmorph == 0 and us_mex > 0 
!
!
!ROUTINES CALLED:
!     ave2 (included)
!
!ATTRIBUTES:
!   LANGUAGE: F90
!
!$$$$$$$$$$$$

      program xxxx
      integer, parameter :: nx_cmorph=1440,ny_cmorph=480,nx_us_mex=321,ny_us_mex=201

      integer :: n_hours_p(nx_us_mex,ny_us_mex)
      real(kind=4) :: cmorph(nx_us_mex+1,ny_us_mex,24), cmorph1(nx_cmorph,ny_cmorph), &
            cmorph_sum(nx_us_mex,ny_us_mex), w(nx_us_mex,ny_us_mex,24), us_mex(nx_us_mex,ny_us_mex), &
            us_mex_disagr(nx_us_mex,ny_us_mex), new_cmorph(nx_us_mex,ny_us_mex,24), &
            lat(nx_us_mex,ny_us_mex), lon(nx_us_mex,ny_us_mex), flag(nx_us_mex,ny_us_mex) 

      real :: c1,c2,c3,c4,d1,d2,summ,rhold, flag_sum
      real :: rmin, rmax, rave
      integer :: count, ave_cnt

      character(len=81) :: filename_cmorph(24), filename_us_mex
      character(len=10) :: date(24)

!===========================================================
 
      open(1,access='sequential',form='formatted',status='old')       

      do i=1,24
         read (1,'(a81)') filename_cmorph(i)         !e.g., [CMORPH_025deg.2004070111]
         read (1,'(a10)') date(i)   !e.g., [2004070111]
         print *, 'file=',filename_cmorph(i), '  date=',date(i)
      enddo

      read(1,'(a81)') filename_us_mex
      print *, filename_us_mex
      close(1)

!     read us_mex daily precip, f77 headers

      write(*,*) 'daily precip=',trim(filename_us_mex)
      open (2,file=trim(filename_us_mex),status='old',form='unformatted',err=30)
      read (2,err=20) us_mex
      close(2)
      goto 40
20    close(2)
30    continue
      write(*,*) 'missing us_mex data'
      us_mex = -9999.0
40    continue

!     convert wgrib undef (9.999e20) to local undef
!     convert to mm/sec -> mm/day

      count = 0
      rmin = 9.999e20
      rmax = 1e20
      ave_cnt = 0
      rave = 0.0
      do j=1,ny_us_mex
          do i=1,nx_us_mex
              if (us_mex(i,j) < 0.0 .or. (us_mex(i,j) >= 9.998e20 .and. us_mex(i,j) <= 10.0e20)) then
                  us_mex(i,j) = -9999.0
                  count = count + 1
              else
!                 new code v1.0.1 prate -> mm / day
                  us_mex(i,j) = us_mex(i,j)*86400.0
                  if (rmin >= 9.99e20) then
                      rmin = us_mex(i,j)
                      rmax = rmin
                  endif
                  rmin = amin1(rmin,us_mex(i,j))
                  rmax = amax1(rmax,us_mex(i,j))
                  rave = rave + us_mex(i,j)
                  ave_cnt = ave_cnt + 1
              endif
          enddo
      enddo
      write(*,*) 'us_mex: daily precip undefined=', count, &
          ny_us_mex* nx_us_mex
      write(*,*) 'daily precip min/max=', rmin, rmax
      if (ave_cnt > 0) write(*,*) 'ave value=',rave/ave_cnt
      write(*,*) 'above is PRATE, mm/sec'

!      open (51,file=date(1)//'.e.undefined_cmorph')
!      open (52,file=date(1)//'.e.bad.sum.weights')
!      open (53,file=date(1)//'.e.cmorph_0_us_mex_gt_0')

!=========================================================
!   DEFINE THE LATITUDE/LONGITUDE ARRAYS FOR PRINTOUTS

       do j=1,ny_us_mex
           do i=1,nx_us_mex
               lat(i,j) = 10.0 + (j-1)*0.25
               lon(i,j) = -140.0 + (i-1)*0.25
           enddo
       enddo


!======================================================================
! REFORMAT THE INDEXING OF THE CMORPH GRID TO FOCUS UPON THE 
! NORTH AMERICAN DOMAIN SIMILAR TO THE US-MEX P PRODUCT  
! NOTE: THE CMORPH GRID IS OFFSET FROM THE US-MEX GRID BY 0.125 DEG
! IN BOTH THE LATITUDE AND LONGITUDE DIRECTIONS, HOWEVER THE RESOLUTION
! IS THE SAME (0.25 DEG)           
!======================================================================

!     read cmorph and copy to ldas grid

      do iii=1,24

	 write(*,*) 'cmorph(iii',iii,')=',trim(filename_cmorph(iii))
         open (2,file=trim(filename_cmorph(iii)),status='old', &
              access='direct',recl=4*nx_cmorph*ny_cmorph,err=50) 
         read(2,rec=1,err=45) cmorph1
         close(2)
         goto 60
45       close(2)
50       cmorph1 = -9999.0
	 write(*,*) 'missing cmorph data ', trim(filename_cmorph(iii))
60       continue

         ! i=880,1201 !(-140.125 to -59.875 domain; size 322)
         ! j=280,480  !(9.875 to 59.875 domain; size 201) 

         cmorph(1:322,1:201,iii)=cmorph1(880:1201,280:480)
     enddo

!    convert wgrib undef (9.999e20) to local undef
!    where (cmorph >= 9.998e20 .and. cmorph <= 10.0e20) cmorph = -9999.0

     count = 0
     do k = 1, 24
         rave = 0.0
         ave_cnt = 0
         do j = 1, ny_us_mex
             do i = 1, nx_us_mex+1
                if (cmorph(i,j,k) < 0.0) then
                    cmorph(i,j,k) = -9999.0
                    count = count + 1
                endif
                if (cmorph(i,j,k) >= 9.998e20 .and. cmorph(i,j,k) <= 10.0e20) then
                    cmorph(i,j,k) = -9999.0
                    count = count + 1
                endif
                if (cmorph(i,j,k) >= 0.0) then
                    rave = rave + cmorph(i,j,k)
                    ave_cnt = ave_cnt + 1
                endif
             enddo
         enddo
         if (ave_cnt > 0) write(*,*) 'cmorph ihour/ave=',k,rave/ave_cnt
         if (ave_cnt .eq. 0) write(*,*) 'cmorph ihour/ave= all missing data'
         write(*,*) 'cmorph has units of mm/hour'
      enddo
     write(*,*) 'hourly cmorph undefined=',count

!--------------------------------------------------------------------------
! INTERPOLATE THE CMORPH DATA TO THE US-MEX GRID: USE A 4-PT INTERPOLATION
!--------------------------------------------------------------------------

       do i=1,nx_us_mex     !(321 points)
       do j=1,ny_us_mex-1   !(200 points)
       do iii=1,24

           summ = 0.0
           wt = 0.0
           if (cmorph(i,j,iii) /= -9999.) then
               summ = summ + cmorph(i,j,iii)
               wt = wt + 1.0
           endif
           if (cmorph(i+1,j,iii) /= -9999.) then
               summ = summ + cmorph(i+1,j,iii)
               wt = wt + 1.0
           endif
           if (cmorph(i,j+1,iii) /= -9999.) then
               summ = summ + cmorph(i,j+1,iii)
               wt = wt + 1.0
           endif
           if (cmorph(i+1,j+1,iii) /= -9999.) then
               summ = summ + cmorph(i+1,j+1,iii)
               wt = wt + 1.0
           endif
           if (wt == 0.0) then      ! then all 4 surrounding cmorph values undefined
               new_cmorph(i,j,iii) = -9999.0 
           else
               new_cmorph(i,j,iii) = summ/wt
           end if
       enddo
       enddo
       enddo

!----------------------------------------------------------------------
! FIX THE TOPMOST LATITUDE

       do i=1,nx_us_mex
       do iii=1,24
          new_cmorph(i,ny_us_mex,iii) = ave2(cmorph(i,ny_us_mex,iii),cmorph(i+1,ny_us_mex,iii))
       enddo
       enddo

!-----------------------------------------------------------------------------------
!  NOW WE NEED TO TAKE CARE OF MISSING VALUES IN CMORPH, USE INTERPOLATION IN TIME 
!-----------------------------------------------------------------------------------
       do j=1,ny_us_mex
           do i=1,nx_us_mex
               if (new_cmorph(i,j,1) == -9999.) new_cmorph(i,j,1)=new_cmorph(i,j,2)
               if (new_cmorph(i,j,24) == -9999.) new_cmorph(i,j,24)=new_cmorph(i,j,23)
           enddo
       enddo

!----------------------------------------------------
       do i=1,nx_us_mex
       do j=1,ny_us_mex
       do iii=2,23
           if (new_cmorph(i,j,iii).eq.-9999.) then
               new_cmorph(i,j,iii) = ave2(new_cmorph(i,j,iii-1),new_cmorph(i,j,iii+1))
           end if
       enddo
       enddo
       enddo


!*********************************************************************************
!************** FINISHED CMORPH PROCESSING ONTO US-MEXICO GRID *******************
!*********************************************************************************

!
!     NOTE THAT HERE IF NEW_CMORPH(I,J,III) IS STILL UNDEFINED THEN
! THERE ARE AT LEAST 2 SUCCESSIVE TIMES WITH UNDEFINED CMORPH VALUES
!
!   NEXT WE DETERMINE THE DAILY CMORPH, convert to mm/day
!=============================================================================
! SET THE INITIAL ARRAYS TO ZEROS

         n_hours_p = 0
         cmorph_sum=0.0

         do iii = 1, 24
             do j = 1, ny_us_mex
                 do i = 1, nx_us_mex
                     if (new_cmorph(i,j,iii) /= -9999.) then
                         cmorph_sum(i,j) = cmorph_sum(i,j) + new_cmorph(i,j,iii)
                         n_hours_p(i,j) = n_hours_p(i,j) + 1
                     endif
                 enddo
             enddo
         enddo
         rmin = -9999.0
         rmax = -9999.0
         rave = 0.0
         ave_cnt = 0
         do j = 1, ny_us_mex
             do i = 1, nx_us_mex
                 if (n_hours_p(i,j) > 0) then
                     cmorph_sum(i,j) = cmorph_sum(i,j)/n_hours_p(i,j)*24.0
                     if (rmin .eq. -9999.0) then
                        rmin = cmorph_sum(i,j)
                        rmax = cmorph_sum(i,j)
                     endif
                     rmin = amin1(rmin, cmorph_sum(i,j))
                     rmax = amax1(rmax, cmorph_sum(i,j))
                     rave = rave + cmorph_sum(i,j)
                     ave_cnt = ave_cnt + 1
                 else
                     cmorph_sum(i,j) = -9999.0
                 endif
             enddo
         enddo
         write(*,*) 'cmorph_sum min/max=', rmin, rmax
         if (ave_cnt > 0) write(*,*) 'ave cmorph_sum=',rave/ave_cnt
!
!   new code: if us_mex is undefined, use cmorph data
!   us_mex is mm/day
!   cmorph_sum is in mm/day
        count = 0
        do j = 1, ny_us_mex
            do i = 1, nx_us_mex
               if(us_mex(i,j) == -9999.0) then
                   us_mex(i,j) = cmorph_sum(i,j)
                   count = count + 1
               endif
            enddo
        enddo
        write(*,*) 'if missing us_mex, use cmorph_sum n=',count

!---------------------------------------------------
! NOW CALCULATE THE WEIGHTS FOR THE GIVEN HOURS
! EACH WEIGHT AT A GIVEN GRID POINT WILL HAVE A
! FLAG ASSOCIATED WITH IT   
! FLAG = 1.0 (TRUE WEIGHT)
! FLAG = 0.0 (ASSIGNED WEIGHT BASED UPON ASSUMPTION)
!----------------------------------------------------

         do 500 i=1,nx_us_mex
         do 525 j=1,ny_us_mex
         
         if (n_hours_p(i,j).eq.24 .and. cmorph_sum(i,j).gt.0.) then
            ! we have an accumulation
            flag(i,j) = 1.0
            w(i,j,:) = new_cmorph(i,j,:)/cmorph_sum(i,j)
         else           
             flag(i,j) = 0.0
             w(i,j,:) = 1./24.                 ! case where precip is defined in cmorph for all 
             if (cmorph_sum(i,j) .eq. 0.0) write(51,*) i,j,lat(i,j),lon(i,j)
         end if

525      continue
500      continue

!----------------------------------------------------------
!            NOW WE HAVE ALL THE WEIGHTS
! CONSIDER THE INSTANCES WHEN CMORPH_SUM=0, BUT US_MEX P>0
! WHERE POSSIBLE REDEFINE THE WEIGHTS BY MEANS OF A 4-POINT
!          AVERAGE OF THE SURROUNDING GRID POINTS
!-----------------------------------------------------------

           do 540 i=2,nx_us_mex-1   ! we do not concern ourselves with the weights at the 
           do 545 j=2,ny_us_mex-1   ! boundaries because us_mex=0.0 there anyways

           if ((cmorph_sum(i,j).eq.0.) .and. &
               (us_mex(i,j).gt.0.)) then            ! search 4 nearest grid points to try and 
                                                    ! find a weight via interpolation
           write(53,*) i,j,lat(i,j),lon(i,j)

           flag_sum = (flag(i-1,j)+flag(i+1,j)+ &
                    flag(i,j-1)+flag(i,j+1))        ! if sum is 0.0 then all weights are 1/24
                                                    ! and no new information can be obtained
                                                    ! we decide to leave the weights at 1/24

           if (flag_sum .ne. 0.0) then

           w(i,j,:) =((flag(i-1,j)*w(i-1,j,:)) + (flag(i+1,j)*w(i+1,j,:)) + &
                        (flag(i,j-1)*w(i,j-1,:)) + (flag(i,j+1)*w(i,j+1,:))) / flag_sum

          end if  
          end if

545       continue
540       continue

!----------------------------------------------------------------------
! NOW PERFORM A CALCULATION TO VERIFY THAT THE SUM OF THE WEIGHTS AT
!              EACH GRID POINT ARE 1.0 (OR VERY CLOSE)
!---------------------------------------------------------------------

          do 580 i=1,nx_us_mex
          do 585 j=1,ny_us_mex
              summ = sum(w(i,j,:))
              if (abs(summ-1.0) .ge. 0.001) write(52,*) summ,i,j,lat(i,j),lon(i,j)
585       continue
580       continue

!========================================================
! HERE WE PERFORM THE DISAGGREGATION OF THE US_MEX P DATA
!========================================================
! 7/2014: old  us_mex_disagr = 25.4 * us_mex * w(:,:,iii)
!         new  us_mex_disag = us_mex * w(:,:,iii)
!         change in input datasets - change in units?

          do 600 iii=1,24
              open (3,file=date(iii)//'.us_mex', form='unformatted',status='unknown', &
                 access='direct',recl=4*nx_us_mex*ny_us_mex)

              print *, iii, date(iii)//'.us_mex'

              do j = 1, ny_us_mex
                  do i = 1, ny_us_mex
                      us_mex_disagr(i,j) = -9999.0
                      if (us_mex(i,j) /= -9999.0 .and. w(i,j,1) /= -9999.) &
                         us_mex_disagr = us_mex * w(:,:,iii)
! 7/2014                         us_mex_disagr = 25.4 * us_mex * w(:,:,iii)
                  enddo
              enddo

!              us_mex_disagr = -9999.0
!              where (us_mex /= -9999.0 .and. w(:,:,1) /= -9999.) &
!                  us_mex_disagr = 25.4 * us_mex * w(:,:,iii)

              write(3,rec=1) us_mex_disagr
              close(3)

600       continue      
!---------------------------------------------------------------------
! TEST TO SEE IF INTERPOLATION OF CMORPH DATA WAS DONE CORRECTLY
!
!          do 700 iii=1,24
!
!          open (6,file=date(iii)//'.cmorph',
!     1          form='unformatted',status='unknown')
!
!          write(6) ((new_cmorph(i,j,iii),i=1,nx_us_mex),j=1,ny_us_mex)
!          close(6)
!
!700      continue
!
!========================================================================
       
         close(51)
         close(52)
         close(53)

         stop
         end

!
!	function to figure average of 2 points
!
	real function ave2(a,b)
	real, intent(in) :: a, b

	if (a .eq. -9999.0) then
            ave2 = b
        else if (b .eq. -9999.0) then
            ave2 = a
        else
            ave2 = 0.5*(a+b)
        endif
	return
	end
