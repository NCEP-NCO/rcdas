c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      program bufr2i3e
 
      character*8 subset
      dimension   rdata(100)
      real*8      arr(10),chn(2,20)
 
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
 
c  initialize various constants
 
      lubfr = 20
      lui3e = 51
      nreal = 14
      iwrot = 0
 
c  read through the bufr tovs records converting each to i3e format
 
      call openbf(lubfr,'IN',lubfr)
      do while(ireadmg(lubfr,subset,idate).eq.0)
      do while(ireadsb(lubfr).eq.0)
 
c  rdata(1) = satellite id
 
      call ufbint(lubfr,arr,10,1,iret,'SAID')
      if(nint(arr(1)).eq.200) rdata(1) = 8
      if(nint(arr(1)).eq.201) rdata(1) = 9
      if(nint(arr(1)).eq.202) rdata(1) = 10
      if(nint(arr(1)).eq.203) rdata(1) = 11
      if(nint(arr(1)).eq.204) rdata(1) = 12
      if(nint(arr(1)).eq.205) rdata(1) = 14
      if(nint(arr(1)).eq.206) rdata(1) = 15
      if(nint(arr(1)).eq.207) rdata(1) = 16
      if(nint(arr(1)).eq.208) rdata(1) = 17
      if(nint(arr(1)).eq.209) rdata(1) = 18
 
c  rdata(2) = satellite instrument
 
      call ufbint(lubfr,arr,10,1,iret,'SIID')
      if(nint(arr(1)).eq.605) rdata(2) =  1       ! hirs/2
      if(nint(arr(1)).eq.623) rdata(2) =  2       ! msu
      if(nint(arr(1)).eq.570) rdata(2) = 10       ! amsu-a
      if(nint(arr(1)).eq.574) rdata(2) = 11       ! amsu-b
      if(nint(arr(1)).eq.606) rdata(2) =  5       ! hirs/3
 
c  rdata(3/4/5/6) = year/month/day/seconds from 00z
 
      call ufbint(lubfr,arr,10,1,iret,'YEAR MNTH DAYS HOUR MINU SECO')
      rdata(3) = nint(arr(1))
      rdata(4) = nint(arr(2))
      rdata(5) = nint(arr(3))
      rdata(6) = nint(arr(4))*3600+nint(arr(5))*60+nint(arr(6))
 
c  rdata(7) = land/sea qualifier
 
      call ufbint(lubfr,arr,10,1,iret,'LSQL')
      rdata(7) = mod(nint(arr(1))+1,2)
 
c  rdata(8) = field of view
 
      call ufbint(lubfr,arr,10,1,iret,'FOVN')
      rdata(8) = nint(arr(1))
 
c  rdata(9/10) latitude/longitude
 
      call ufbint(lubfr,arr,10,1,iret,'CLAT CLON')
      rdata(9)  = arr(1)
      rdata(10) = arr(2)
 
c  rdata(11/12) satellite/solar zenith angle
 
      call ufbint(lubfr,arr,10,1,iret,'SAZA SOZA')
      rdata(11) = arr(1)
      rdata(12) = arr(2)
 
c  rdata(13/14) height above landsfc/sealevel
 
      call ufbint(lubfr,arr,10,1,iret,'HOLS HMSL')
      rdata(13) = arr(1)
      rdata(14) = arr(2)*.001
 
c  rdata(14+i),i-1,nchan = brightness temps
 
      call ufbseq(lubfr,chn,2,20,nchan,'BRIT')
      do n=1,nchan
      nc = nint(chn(1,n))
      rdata(14+nc) = chn(2,n)
      enddo
 
c  write the i3e format tovs report
 
      if(iwrot.eq.0) write(lui3e) nreal,nchan,(float(i),i=1,nchan)
      write(lui3e) (rdata(i),i=1,nreal+nchan)
      iwrot = iwrot+1
 
      enddo
      enddo
 
c  exit
 
      print*,'bufri3e converted ',iwrot,' tovs records'
      stop
      end
