      SUBROUTINE SNO8GET(SNODEP,INSNOAF,LUSAF,iyr,imo,idy,icent,
     *    bitmap,iret2)
C
C USAGE: Program to read in USAF gribbed snow/ice data on 1/8th bedient
C polar stereographic grid. 
C
      INTEGER KPDS(25), KGDS(22), JPDS(25), JGDS(22)
      REAL SNODEP(512,512), snotmp(512,512)  
      LOGICAL*1 BITMAP(512,512), LUSAF
      dimension idate(4)
      dimension ibit(512,512)
C
C     PROCESS USAF N.H. SNOW ANAL (FROM COLA), GRIBBED
C     USAF SNOW ANAL IS DAILY AND HIGH RES (45 KM)             
C                                                                  
C  SNOW IS IN METERS.
c  
c  For Regional Reanalysis, there is no ice, so just read the snow.
C                                            
      print*,'In sno8get,insnoaf=',insnoaf
c     call baopenr(insnoaf,'fort.41',ireto)
c     print*,'ireto=',ireto
c     call baopenr(37,'fort.37',ireto)
c     print*,'ireto=',ireto
      JPDS = -1
      j=0
c     icent = (idate(4) - 1) / 100 + 1
c     jpds(8) = idate(4) - (icent-1)*100
c     jpds(9) = idate(2)
c     jpds(10)= idate(3)
c     jpds(21)=icent
      jpds(5)=66
c     jpds(8)=iyr-100
      jpds(8)=iyr
      jpds(9)=imo
      jpds(10)=idy
      jpds(21)=icent
      print*,'jpds=',jpds
      CALL GETGB(INSNOAF,0,512*512,j,JPDS,JGDS,KF,KNUM,KPDS,KGDS,
     &     BITMAP,SNODEP,IRET2)
      if(iret2.ne.0) then
      print*,'iret2=',iret2
      stop 8
      endif
c     print*,'bitmap=',bitmap
      do j=1,512
      do i=1,512
c     if(snodep(i,j).gt.0.0) print*,'i,j,snodep=',i,j,snodep(i,j)
c    *  ,bitmap(i,j)
c       print*,'i,j,bitmap=',i,j,bitmap(i,j),bitmap(i,j).eqv..T.
        if(bitmap(i,j).eqv..TRUE.) then 
         ibit(i,j)=1
        else
         ibit(i,j)=0
        endif
      enddo
      enddo
c     print*,'ibit=',ibit
c     call printaf(ibit,ibit)
      print*,'kpds=',kpds
      print*,'kgds=',kgds
      WRITE(6,*) 'AFTER GETGB FOR AF SNOW, IRET=', IRET2
c     call baclose(insnoaf,ireto2)
C
c     IYR2D = KPDS(8)
c     IMN = KPDS(9)
c     IDY = KPDS(10)
c     ICENT = KPDS(21)
c     IF(IYR2D.LT.100) THEN
c      IYR = (ICENT - 1) * 100 + IYR2D
c     ELSE
c      IYR = ICENT * 100
c     ENDIF
C
      LUSAF =  IRET2.EQ.0
      WRITE(6,*) 'LUSAF=', LUSAF
      IF (.NOT.LUSAF) RETURN
      
C
c     DO 20 I = 1, 512
c       DO 10 J = 1, 512
c         IF (ABS(ICE(I,J)-1.) .LT. 0.0001) SNODEP(I,J) = 11.
c10     CONTINUE
c20   CONTINUE
      RETURN
      END
