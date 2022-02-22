C UPDATED FROM OPERATIONS 4/2001
 
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE BAKFIT(XFIT,QMAX,HDR,OBS,QMS,BAK,NLEV)
 
      COMMON /MANFAC/ MAXP,MAND,PMAN(21),LEVP(1200)
 
C-CRA DIMENSION XFIT(3,MAND,4,6,100)
 
      PARAMETER(MANDIM=21)
      DIMENSION XFIT(3,MANDIM,4,6,100)
 
      DIMENSION HDR(10),OBS(10,NLEV),QMS(10,NLEV),BAK(10,NLEV)
 
      DATA XMX /320/
 
C-----------------------------------------------------------------------
      IP(X) = LEVP(MAX(MIN(NINT(X),MAXP),1))
C-----------------------------------------------------------------------
 
      IF(NLEV.LE.0) RETURN
 
      SID = HDR(1)
      XOB = HDR(2)
      YOB = HDR(3)
      DHR = HDR(4)
      TYP = HDR(5)
      KXI = TYP
      KXX = MOD(KXI,100)
      LZN = MIN(IFIX((YOB+90.)/30.+1.),6)
 
C  ADD FITS AND NORMALIZED DISTRIBUTIONS TO ACCUMULATORY ARRAYS
C  ------------------------------------------------------------
 
      DO L=1,NLEV
 
      PQM = QMS(1,L)
      QQM = QMS(2,L)
      TQM = QMS(3,L)
      ZQM = QMS(4,L)
      WQM = QMS(5,L)
      PWQ = QMS(7,L)
 
      POB = OBS(1,L)
      QOB = OBS(2,L)
      TOB = OBS(3,L)
      ZOB = OBS(4,L)
      UOB = OBS(5,L)
      VOB = OBS(6,L)
 
      PBK = BAK(1,L)
      QBK = BAK(2,L)
      TBK = BAK(3,L)
      ZBK = BAK(4,L)
      UBK = BAK(5,L)
      VBK = BAK(6,L)
 
      IF(PQM.LE.QMAX .AND. POB.GT.0 .AND. POB.LT.2000) THEN
         IF(KXI.GE.190 .AND. KXI.LE.199) THEN
            CALL FITS(0.,XFIT(1,IP(POB),1,LZN,KXX))
         ENDIF
         IF(KXI.EQ.152) THEN
            IF(PWQ.LE.QMAX) CALL FITS(0.,XFIT(1,IP(POB),1,LZN,KXX))
         ENDIF
C        IF(TQM.GT.QMAX .AND. WQM.GT.QMAX) THEN
C           CALL FITS(0.,XFIT(1,IP(POB),1,LZN,KXX))
C        ENDIF
         IF(TQM.LE.QMAX) THEN
            TINC = MIN(TOB-TBK,XMX)
            CALL FITS(TINC,XFIT(1,IP(POB),1,LZN,KXX))
         ENDIF
         IF(WQM.LE.QMAX) THEN
            UINC = MIN(UOB-UBK,XMX)
            VINC = MIN(VOB-VBK,XMX)
            SINC = MIN(SQRT(UOB**2+VOB**2)-SQRT(UBK**2+VBK**2),XMX)
            CALL FITS(UINC,XFIT(1,IP(POB),2,LZN,KXX))
            CALL FITS(VINC,XFIT(1,IP(POB),3,LZN,KXX))
            CALL FITS(SINC,XFIT(1,IP(POB),4,LZN,KXX))
         ENDIF
      ENDIF
 
      ENDDO
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE FITS(FIT,STAT)
      DIMENSION STAT(3)
      STAT(1) = STAT(1) + 1.
      STAT(2) = STAT(2) + FIT
      STAT(3) = STAT(3) + FIT**2
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE WRTOBO(LIN,LOUT,NLEV,STAT,NMCDATE)
 
      PARAMETER (MREC=5000)
 
      COMMON /WORDL/  NBITW
 
      DIMENSION STAT(3,NLEV,4,6,100),IIN(MREC),IOUT(MREC)
 
      LOGICAL   QOBS,OKAY
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      IBIT = 0
      DO I=1,MREC
      IOUT(I) = 0
      ENDDO
 
      REWIND LIN
      REWIND LOUT
 
C  COMPUTE THE MEAN AND RMS AND WRITE THE PACKED RECORD
C  ----------------------------------------------------
 
      DO 10 KX=1,100
      DO 10 LB=1,6
      DO 10 K=1,4
 
      QOBS = .FALSE.
 
      DO L=1,NLEV
      TOT = STAT(1,L,K,LB,KX)
      AVG = STAT(2,L,K,LB,KX)
      RMS = STAT(3,L,K,LB,KX)
      IF(TOT.GT.0.) THEN
         QOBS = .TRUE.
         STAT(2,L,K,LB,KX) = AVG/TOT
         STAT(3,L,K,LB,KX) = SQRT(RMS/TOT)
      ENDIF
      ENDDO
 
      IF(QOBS) CALL PKLEV(STAT,NLEV,K,LB,KX,IOUT,IBIT)
 
10    CONTINUE
 
      LREC =  IBIT/NBITW + 2
C-CRA LREC =  IBIT/64    + 2
      IF(LREC.GT.MREC) CALL SABORT('WRTOBO - NEW RECORD TOO LONG')
 
C  FIND A PLACE IN THE STAT FILE TO WRITE THE NEW RECORD
C  -----------------------------------------------------
 
      OKAY = .FALSE.
 
25    READ(LIN,END=30,ERR=30) NMCDAT,NLV,LRC,(IIN(I),I=1,LRC)
      IF(LRC.GT.MREC) CALL SABORT('WRTOBO - OLD RECORD TOO LONG')
      IF(NMCDATE.LE.NMCDAT .AND. .NOT.OKAY) THEN
         WRITE(LOUT) NMCDATE,NLEV,LREC,(IOUT(I),I=1,LREC)
         IF(NMCDATE.LT.NMCDAT) THEN
            WRITE(LOUT) NMCDAT,NLV,LRC,(IIN(I),I=1,LRC )
         ENDIF
         OKAY = .TRUE.
      ELSE
         WRITE(LOUT) NMCDAT,NLV,LRC,(IIN(I),I=1,LRC )
      ENDIF
      GOTO 25
 
C  MAKE SURE THE NEW RECORD GETS INTHER
C  ------------------------------------
 
30    IF(.NOT.OKAY) WRITE(LOUT) NMCDATE,NLEV,LREC,(IOUT(I),I=1,LREC)
 
C  COPY UPDATED FILE OVER ORIGINAL ONE
C  -----------------------------------
 
      REWIND LIN
      REWIND LOUT
 
40    READ (LOUT,END=50) NMCDAT,NLEV,LREC,(IOUT(I),I=1,LREC)
      WRITE(LIN        ) NMCDAT,NLEV,LREC,(IOUT(I),I=1,LREC)
      GOTO 40
50    CONTINUE
 
      CLOSE(LIN)
      CLOSE(LOUT)
 
      RETURN
      END
C----------------------------------------------------------------------
C  PACK UP A NUMBER ACCORDING TO SPECS
C----------------------------------------------------------------------
C-CRA SUBROUTINE PKBB(VAL,NBITS,ISCALE,IBAY,IBIT)
C-CRA DIMENSION IBAY(*)
C-CRA NVL = VAL*10**ISCALE + SIGN(.5,VAL)
C-CRA NWD = (IBIT)/64+1
C-CRA NBT = MOD(IBIT,64)
C-CRA INT = SHIFTR(SHIFTL(NVL,64-NBITS),NBT)
C-CRA MSK = SHIFTR(SHIFTL( -1,64-NBITS),NBT)
C-CRA IBAY(NWD) = (IBAY(NWD).AND..NOT.MSK) .OR. INT
C-CRA IF(NBT+NBITS.GT.64) THEN
C-CRA    INT = SHIFTL(NVL ,128-(NBT+NBITS))
C-CRA    MSK = SHIFTL(  -1,128-(NBT+NBITS))
C-CRA    IBAY(NWD+1) = (IBAY(NWD+1).AND..NOT.MSK) .OR. INT
C-CRA ENDIF
C-CRA VALU = UPBIT(NBITS,ISCALE,IBAY,IBIT)
 
C-CRA IF(VALU.NE.NVL*10.**(-ISCALE)) THEN
C-CRA    PRINT*,VAL,NVL,VALU,NBITS,ISCALE
C-CRA    CALL SABORT('PKBB - CANT UNPACK')
C-CRA ENDIF
 
C-CRA RETURN
C-CRA END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE PKLEV(STAT,NLEV,IV,LB,KX,IBAY,IBIT)
 
      DIMENSION STAT(3,NLEV,4,6,100),IBAY(*)
 
      CALL PKBB(FLOAT(KX),8,0,IBAY,IBIT)
      CALL PKBB(FLOAT(LB),4,0,IBAY,IBIT)
      CALL PKBB(FLOAT(IV),4,0,IBAY,IBIT)
 
      DO L=1,NLEV
      IF(STAT(1,L,IV,LB,KX).GT.0) THEN
         CALL PKBB(FLOAT(-1)         , 1,0,IBAY,IBIT)
         CALL PKBB(STAT(1,L,IV,LB,KX),16,0,IBAY,IBIT)
         CALL PKBB(STAT(2,L,IV,LB,KX),16,2,IBAY,IBIT)
         CALL PKBB(STAT(3,L,IV,LB,KX),16,2,IBAY,IBIT)
      ELSE
         CALL PKBB(FLOAT(0)          , 1,0,IBAY,IBIT)
      ENDIF
      ENDDO
 
      RETURN
      END
C----------------------------------------------------------------------
C  UNPACK UP A NUMBER WITH A SIGN
C----------------------------------------------------------------------
C-CRA FUNCTION UPBIT(NBITS,ISCALE,IBAY,IBIT)
C-CRA DIMENSION IBAY(*)
C-CRA NWD = (IBIT)/64+1
C-CRA NBT = MOD(IBIT,64)
C-CRA INT = SHIFTR(SHIFTL(IBAY(NWD),NBT),64-NBITS)
C-CRA LBT = NBT+NBITS
C-CRA IF(LBT.GT.64) INT = INT .OR. SHIFTR(IBAY(NWD+1),128-LBT)
C-CRA IF(LEADZ(INT).EQ.64-NBITS) INT = INT.OR.MASK(64-NBITS)
C-CRA UPBIT  = INT * 10.**(-ISCALE)
C-CRA IBIT = IBIT+NBITS
C-CRA RETURN
C-CRA END
C----------------------------------------------------------------------
C  PACK UP A NUMBER AND CHECK ON THE PACKING
C----------------------------------------------------------------------
      SUBROUTINE PKBB(VAL,NBITS,ISCALE,IBAY,IBIT)
      DIMENSION IBAY(*)
 
      NVAL = NINT(VAL*10.**ISCALE)
      CALL PKBS(NVAL,NBITS,IBAY,IBIT)
      CALL UPBS(MVAL,NBITS,IBAY,IBIT)
 
      IF(MVAL.NE.NVAL) THEN
         PRINT*,VAL,NBITS,ISCALE
         CALL SABORT('PKBB - CANT UNPACK')
      ENDIF
 
      IBIT = IBIT+NBITS
 
      RETURN
      END
C----------------------------------------------------------------------
C  PACK UP AN INTEGER WITH A SIGN
C----------------------------------------------------------------------
      SUBROUTINE PKBS(NVAL,NBITS,IBAY,IBIT)
 
      COMMON /WORDL/  NBITW
 
      DIMENSION IBAY(*)
 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
 
      NWD  = IBIT/NBITW + 1
      NBT  = MOD(IBIT,NBITW)
      IVAL = NVAL
      INT = ISHFT(IVAL,NBITW-NBITS)
      INT = ISHFT(INT,-NBT)
      MSK = ISHFT(  -1,NBITW-NBITS)
      MSK = ISHFT(MSK,-NBT)
      IBAY(NWD) = IREV(IOR(IAND(IREV(IBAY(NWD)),NOT(MSK)),INT))
      IF(NBT+NBITS.GT.NBITW) THEN
         INT = ISHFT(IVAL,2*NBITW-(NBT+NBITS))
         MSK = ISHFT(  -1,2*NBITW-(NBT+NBITS))
         IBAY(NWD+1) = IREV(IOR(IAND(IREV(IBAY(NWD+1)),NOT(MSK)),INT))
      ENDIF
 
      RETURN
      END
C----------------------------------------------------------------------
C  UNPACK UP AN INTEGER WITH A SIGN
C----------------------------------------------------------------------
      SUBROUTINE UPBS(NVAL,NBITS,IBAY,IBIT)
 
      COMMON /WORDL/  NBITW
 
      DIMENSION IBAY(*)
 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
 
      NWD = (IBIT)/NBITW+1
      NBT = MOD(IBIT,NBITW)
      INT = ISHFT(IREV(IBAY(NWD)),NBT)
      INT = ISHFT(INT,NBITS-NBITW)
      LBT = NBT+NBITS
      IF(LBT.GT.NBITW) JNT = IREV(IBAY(NWD+1))
      IF(LBT.GT.NBITW) INT = IOR(INT,ISHFT(JNT,LBT-2*NBITW))
      IF(BTEST(INT,NBITS-1)) THEN
         DO I=NBITS,NBITW-1
         INT = IBSET(INT,I)
         ENDDO
      ENDIF
      NVAL = INT
      RETURN
      END
C      SUBROUTINE SABORT(STR)
C      CHARACTER*(*) STR
C      WRITE(6,*) STR
CC-SGI CALL FLUSH(6)
C      CALL EXIT(99)
C      END
 
      SUBROUTINE FAX(IFAX,N,MODE)
       SAVE
      DIMENSION IFAX(10)
C
      NN=N
      IF (IABS(MODE).EQ.1) GO TO 10
      IF (IABS(MODE).EQ.8) GO TO 10
      NN=N/2
      IF ((NN+NN).EQ.N) GO TO 10
      IFAX(1)=-99
      RETURN
   10 K=1
C     TEST FOR FACTORS OF 4
   20 IF (MOD(NN,4).NE.0) GO TO 30
      K=K+1
      IFAX(K)=4
      NN=NN/4
      IF (NN.EQ.1) GO TO 80
      GO TO 20
C     TEST FOR EXTRA FACTOR OF 2
   30 IF (MOD(NN,2).NE.0) GO TO 40
      K=K+1
      IFAX(K)=2
      NN=NN/2
      IF (NN.EQ.1) GO TO 80
C     TEST FOR FACTORS OF 3
   40 IF (MOD(NN,3).NE.0) GO TO 50
      K=K+1
      IFAX(K)=3
      NN=NN/3
      IF (NN.EQ.1) GO TO 80
      GO TO 40
C     NOW FIND REMAINING FACTORS
   50 L=5
      INC=2
C     INC ALTERNATELY TAKES ON VALUES 2 AND 4
   60 IF (MOD(NN,L).NE.0) GO TO 70
      K=K+1
      IFAX(K)=L
      NN=NN/L
      IF (NN.EQ.1) GO TO 80
      GO TO 60
   70 L=L+INC
      INC=6-INC
      GO TO 60
   80 IFAX(1)=K-1
C     IFAX(1) CONTAINS NUMBER OF FACTORS
C     IFAX(1) CONTAINS NUMBER OF FACTORS
      NFAX=IFAX(1)
C     SORT FACTORS INTO ASCENDING ORDER
      IF (NFAX.EQ.1) GO TO 110
      DO 100 II=2,NFAX
      ISTOP=NFAX+2-II
      DO 90 I=2,ISTOP
      IF (IFAX(I+1).GE.IFAX(I)) GO TO 90
      ITEM=IFAX(I)
      IFAX(I)=IFAX(I+1)
      IFAX(I+1)=ITEM
   90 CONTINUE
  100 CONTINUE
  110 CONTINUE
      RETURN
      END
      SUBROUTINE FFTRIG(TRIGS,N,MODE)
       SAVE
      DIMENSION TRIGS(1)
C
      PI=2.0*ASIN(1.0)
      IMODE=IABS(MODE)
      NN=N
      IF (IMODE.GT.1.AND.IMODE.LT.6) NN=N/2
      DEL=(PI+PI)/FLOAT(NN)
      L=NN+NN
      DO 10 I=1,L,2
      ANGLE=0.5   E   0*FLOAT(I-1)*DEL
      TRIGS(I)=COS(ANGLE)
      TRIGS(I+1)=SIN(ANGLE)
   10 CONTINUE
      IF (IMODE.EQ.1) RETURN
      IF (IMODE.EQ.8) RETURN
      DEL=0.5  E  0*DEL
      NH=(NN+1)/2
      L=NH+NH
      LA=NN+NN
      DO 20 I=1,L,2
      ANGLE=0.5  E  0*FLOAT(I-1)*DEL
      TRIGS(LA+I)=COS(ANGLE)
      TRIGS(LA+I+1)=SIN(ANGLE)
   20 CONTINUE
      IF (IMODE.LE.3) RETURN
      DEL=0.5  E  0*DEL
      LA=LA+NN
      IF (MODE.EQ.5) GO TO 40
      DO 30 I=2,NN
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=2.0  E  0*SIN(ANGLE)
   30 CONTINUE
      RETURN
   40 CONTINUE
      DEL=0.5  E  0*DEL
      DO 50 I=2,N
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=SIN(ANGLE)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE VPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC,LA)
       SAVE
      DIMENSION A(N),B(N),C(N),D(N),TRIGS(N)
      DATA SIN36/0.587785252292473/,COS36/0.809016994374947/,
     *     SIN72/0.951056516295154/,COS72/0.309016994374947/,
     *     SIN60/0.866025403784437/
C
      M=N/IFAC
      IINK=M*INC1
      JINK=LA*INC2
      JUMP=(IFAC-1)*JINK
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.GT.4) RETURN
      GO TO (10,50,90,130),IGO
C
C     CODING FOR FACTOR 2
C
   10 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      DO 20 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 15 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      D(JB+J)=B(IA+I)-B(IB+I)
      I=I+INC3
      J=J+INC4
   15 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   20 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 40 K=LA1,M,LA
      KB=K+K-2
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      DO 30 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 25 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      D(JA+J)=B(IA+I)+B(IB+I)
      C(JB+J)=C1*(A(IA+I)-A(IB+I))-S1*(B(IA+I)-B(IB+I))
      D(JB+J)=S1*(A(IA+I)-A(IB+I))+C1*(B(IA+I)-B(IB+I))
      I=I+INC3
      J=J+INC4
   25 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   30 CONTINUE
      JBASE=JBASE+JUMP
   40 CONTINUE
      RETURN
C
C     CODING FOR FACTOR 3
C
   50 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      DO 60 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 55 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)=(A(IA+I)-0.5  E  0*(A(IB+I)+A(IC+I)))
     X                                       -(SIN60*(B(IB+I)-B(IC+I)))
      C(JC+J)=(A(IA+I)-0.5  E  0*(A(IB+I)+A(IC+I)))
     X                                       +(SIN60*(B(IB+I)-B(IC+I)))
      D(JB+J)=(B(IA+I)-0.5  E  0*(B(IB+I)+B(IC+I)))
     X                                       +(SIN60*(A(IB+I)-A(IC+I)))
      D(JC+J)=(B(IA+I)-0.5  E  0*(B(IB+I)+B(IC+I)))
     X                                       -(SIN60*(A(IB+I)-A(IC+I)))
      I=I+INC3
      J=J+INC4
   55 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   60 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 80 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      DO 70 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 65 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IC+I))
      C(JB+J)=
     *    C1*((A(IA+I)-0.5  E  0*(A(IB+I)+A(IC+I)))
     X                                      -(SIN60*(B(IB+I)-B(IC+I))))
     *   -S1*((B(IA+I)-0.5  E  0*(B(IB+I)+B(IC+I)))
     X                                      +(SIN60*(A(IB+I)-A(IC+I))))
      D(JB+J)=
     *    S1*((A(IA+I)-0.5  E  0*(A(IB+I)+A(IC+I)))
     X                                      -(SIN60*(B(IB+I)-B(IC+I))))
     *   +C1*((B(IA+I)-0.5  E  0*(B(IB+I)+B(IC+I)))
     X                                      +(SIN60*(A(IB+I)-A(IC+I))))
      C(JC+J)=
     *    C2*((A(IA+I)-0.5  E  0*(A(IB+I)+A(IC+I)))
     X                                      +(SIN60*(B(IB+I)-B(IC+I))))
     *   -S2*((B(IA+I)-0.5  E  0*(B(IB+I)+B(IC+I)))
     X                                      -(SIN60*(A(IB+I)-A(IC+I))))
      D(JC+J)=
     *    S2*((A(IA+I)-0.5  E  0*(A(IB+I)+A(IC+I)))
     X                                      +(SIN60*(B(IB+I)-B(IC+I))))
     *   +C2*((B(IA+I)-0.5  E  0*(B(IB+I)+B(IC+I)))
     X                                      -(SIN60*(A(IB+I)-A(IC+I))))
      I=I+INC3
      J=J+INC4
   65 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
   70 CONTINUE
      JBASE=JBASE+JUMP
   80 CONTINUE
      RETURN
C
C     CODING FOR FACTOR 4
C
   90 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      DO 100 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 95 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      D(JC+J)=(B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I))
      C(JB+J)=(A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I))
      C(JD+J)=(A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I))
      D(JB+J)=(B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I))
      D(JD+J)=(B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I))
      I=I+INC3
      J=J+INC4
   95 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  100 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 120 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      DO 110 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 105 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      D(JA+J)=(B(IA+I)+B(IC+I))+(B(IB+I)+B(ID+I))
      C(JC+J)=
     *    C2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
     *   -S2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      D(JC+J)=
     *    S2*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
     *   +C2*((B(IA+I)+B(IC+I))-(B(IB+I)+B(ID+I)))
      C(JB+J)=
     *    C1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I)))
     *   -S1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      D(JB+J)=
     *    S1*((A(IA+I)-A(IC+I))-(B(IB+I)-B(ID+I)))
     *   +C1*((B(IA+I)-B(IC+I))+(A(IB+I)-A(ID+I)))
      C(JD+J)=
     *    C3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I)))
     *   -S3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      D(JD+J)=
     *    S3*((A(IA+I)-A(IC+I))+(B(IB+I)-B(ID+I)))
     *   +C3*((B(IA+I)-B(IC+I))-(A(IB+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  105 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  110 CONTINUE
      JBASE=JBASE+JUMP
  120 CONTINUE
      RETURN
C
C     CODING FOR FACTOR 5
C
  130 IA=1
      JA=1
      IB=IA+IINK
      JB=JA+JINK
      IC=IB+IINK
      JC=JB+JINK
      ID=IC+IINK
      JD=JC+JINK
      IE=ID+IINK
      JE=JD+JINK
      DO 140 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 135 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I)))
     *  -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      C(JE+J)=(A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I)))
     *  +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I)))
      D(JB+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I)))
     *  +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      D(JE+J)=(B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I)))
     *  -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I)))
      C(JC+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I)))
     *  -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      C(JD+J)=(A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I)))
     *  +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I)))
      D(JC+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I)))
     *  +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      D(JD+J)=(B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I)))
     *  -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I)))
      I=I+INC3
      J=J+INC4
  135 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  140 CONTINUE
      IF (LA.EQ.M) RETURN
      LA1=LA+1
      JBASE=JBASE+JUMP
      DO 160 K=LA1,M,LA
      KB=K+K-2
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      DO 150 L=1,LA
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 145 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IE+I))+(A(IC+I)+A(ID+I))
      D(JA+J)=B(IA+I)+(B(IB+I)+B(IE+I))+(B(IC+I)+B(ID+I))
      C(JB+J)=
     *    C1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I)))
     *      -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I))))
     *   -S1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I)))
     *      +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JB+J)=
     *    S1*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I)))
     *      -(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I))))
     *   +C1*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I)))
     *      +(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JE+J)=
     *    C4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I)))
     *      +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I))))
     *   -S4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I)))
     *      -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      D(JE+J)=
     *    S4*((A(IA+I)+COS72*(A(IB+I)+A(IE+I))-COS36*(A(IC+I)+A(ID+I)))
     *      +(SIN72*(B(IB+I)-B(IE+I))+SIN36*(B(IC+I)-B(ID+I))))
     *   +C4*((B(IA+I)+COS72*(B(IB+I)+B(IE+I))-COS36*(B(IC+I)+B(ID+I)))
     *      -(SIN72*(A(IB+I)-A(IE+I))+SIN36*(A(IC+I)-A(ID+I))))
      C(JC+J)=
     *    C2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I)))
     *      -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I))))
     *   -S2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I)))
     *      +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JC+J)=
     *    S2*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I)))
     *      -(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I))))
     *   +C2*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I)))
     *      +(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      C(JD+J)=
     *    C3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I)))
     *      +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I))))
     *   -S3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I)))
     *      -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      D(JD+J)=
     *    S3*((A(IA+I)-COS36*(A(IB+I)+A(IE+I))+COS72*(A(IC+I)+A(ID+I)))
     *      +(SIN36*(B(IB+I)-B(IE+I))-SIN72*(B(IC+I)-B(ID+I))))
     *   +C3*((B(IA+I)-COS36*(B(IB+I)+B(IE+I))+COS72*(B(IC+I)+B(ID+I)))
     *      -(SIN36*(A(IB+I)-A(IE+I))-SIN72*(A(IC+I)-A(ID+I))))
      I=I+INC3
      J=J+INC4
  145 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  150 CONTINUE
      JBASE=JBASE+JUMP
  160 CONTINUE
      RETURN
      END
      SUBROUTINE FFT99M(A,WORK,TRIGS,IFAX,INC,JUMP,N,LOT,ISIGN)
       SAVE
      DIMENSION A(N),WORK(N),TRIGS(N),IFAX(1)
C
      NFAX=IFAX(1)
      NX=N
      NH=N/2
      INK=INC+INC
      IF (ISIGN.EQ.+1) GO TO 30
C
C     IF NECESSARY, TRANSFER DATA TO WORK AREA
      IGO=50
      IF (MOD(NFAX,2).EQ.1) GOTO 40
      IBASE=1
      JBASE=1
      DO 20 L=1,LOT
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 10 M=1,N
      WORK(J)=A(I)
      I=I+INC
      J=J+1
   10 CONTINUE
      IBASE=IBASE+JUMP
      JBASE=JBASE+NX
   20 CONTINUE
C
      IGO=60
      GO TO 40
C
C     PREPROCESSING (ISIGN=+1)
C     ------------------------
C
   30 CONTINUE
      CALL FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
      IGO=60
C
C     COMPLEX TRANSFORM
C     -----------------
C
   40 CONTINUE
      IA=1
      LA=1
      DO 80 K=1,NFAX
      IF (IGO.EQ.60) GO TO 60
   50 CONTINUE
      CALL VPASSM(A(IA),A(IA+INC),WORK(1),WORK(2),TRIGS,
     *   INK,2,JUMP,NX,LOT,NH,IFAX(K+1),LA)
      IGO=60
      GO TO 70
   60 CONTINUE
      CALL VPASSM(WORK(1),WORK(2),A(IA),A(IA+INC),TRIGS,
     *    2,INK,NX,JUMP,LOT,NH,IFAX(K+1),LA)
      IGO=50
   70 CONTINUE
      LA=LA*IFAX(K+1)
   80 CONTINUE
C
      IF (ISIGN.EQ.-1) GO TO 130
C
C     IF NECESSARY, TRANSFER DATA FROM WORK AREA
      IF (MOD(NFAX,2).EQ.1) GO TO 110
      IBASE=1
      JBASE=1
      DO 100 L=1,LOT
      I=IBASE
      J=JBASE
CDIR$ IVDEP
      DO 90 M=1,N
      A(J)=WORK(I)
      I=I+1
      J=J+INC
   90 CONTINUE
      IBASE=IBASE+NX
      JBASE=JBASE+JUMP
  100 CONTINUE
C
C     FILL IN ZEROS AT END
  110 CONTINUE
      GO TO 140
C
C     POSTPROCESSING (ISIGN=-1):
C     --------------------------
C
  130 CONTINUE
      CALL FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)
C
  140 CONTINUE
      RETURN
      END
      SUBROUTINE FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
       SAVE
C     SUBROUTINE FFT99A - PREPROCESSING STEP FOR FFT99, ISIGN=+1
C     (SPECTRAL TO GRIDPOINT TRANSFORM)
C
      DIMENSION A(N),WORK(N),TRIGS(N)
C
      NH=N/2
      NX=N
      INK=INC+INC
C
C     A(0)   A(N/2)
      IA=1
      IB=N*INC+1
      JA=1
      JB=2
CDIR$ IVDEP
      DO 10 L=1,LOT
      WORK(JA)=A(IA)
      WORK(JB)=A(IA)
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   10 CONTINUE
C
C     REMAINING WAVENUMBERS
      IABASE=2*INC+1
      IBBASE=(N-2)*INC+1
      JABASE=3
      JBBASE=N-1
C
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
CDIR$ IVDEP
      DO 20 L=1,LOT
      WORK(JA)=(A(IA)+A(IB))-
     *    (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JB)=(A(IA)+A(IB))+
     *    (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JA+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))+
     *    (A(IA+INC)-A(IB+INC))
      WORK(JB+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))-
     *    (A(IA+INC)-A(IB+INC))
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
   20 CONTINUE
      IABASE=IABASE+INK
      IBBASE=IBBASE-INK
      JABASE=JABASE+2
      JBBASE=JBBASE-2
   30 CONTINUE
C
      IF (IABASE.NE.IBBASE) GO TO 50
C     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE
CDIR$ IVDEP
      DO 40 L=1,LOT
      WORK(JA)=2.0  E  0*A(IA)
      WORK(JA+1)=-2.0  E  0*A(IA+INC)
      IA=IA+JUMP
      JA=JA+NX
   40 CONTINUE
C
   50 CONTINUE
      RETURN
      END
      SUBROUTINE FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)
       SAVE
C     SUBROUTINE FFT99B - POSTPROCESSING STEP FOR FFT99, ISIGN=-1
C     (GRIDPOINT TO SPECTRAL TRANSFORM)
C
      DIMENSION WORK(N),A(N),TRIGS(N)
C
      NH=N/2
      NX=N
      INK=INC+INC
C
C     A(0)   A(N/2)
      SCALE=1.0  E  0/FLOAT(N)
      IA=1
      IB=2
      JA=1
      JB=N*INC+1
CDIR$ IVDEP
      DO 10 L=1,LOT
      A(JA)=SCALE*(WORK(IA)+WORK(IB))
      A(JA+INC)=0.0  E  0
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   10 CONTINUE
C
C     REMAINING WAVENUMBERS
      SCALE=0.5  E  0*SCALE
      IABASE=3
      IBBASE=N-1
      JABASE=2*INC+1
      JBBASE=(N-2)*INC+1
C
      DO 30 K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
CDIR$ IVDEP
      DO 20 L=1,LOT
      A(JA)=SCALE*((WORK(IA)+WORK(IB))
     *   +(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JB)=SCALE*((WORK(IA)+WORK(IB))
     *   -(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JA+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *    +(WORK(IB+1)-WORK(IA+1)))
      A(JB+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *    -(WORK(IB+1)-WORK(IA+1)))
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
   20 CONTINUE
      IABASE=IABASE+2
      IBBASE=IBBASE-2
      JABASE=JABASE+INK
      JBBASE=JBBASE-INK
   30 CONTINUE
C
      IF (IABASE.NE.IBBASE) GO TO 50
C     WAVENUMBER N/4 (IF IT EXISTS)
      IA=IABASE
      JA=JABASE
      SCALE=2.0  E  0*SCALE
CDIR$ IVDEP
      DO 40 L=1,LOT
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=-SCALE*WORK(IA+1)
      IA=IA+NX
      JA=JA+JUMP
   40 CONTINUE
C
   50 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   .===================================================================
C   | DESCRIPTION | NAME   | BITS | MAX VALUE  | DIMENSIONS
C   |=============*========*======*============*========================
C   | ZS          | ZS     | WORD | REAL       |  256 , 129
C   |-------------+--------+------+------------+------------------------
C   | PS          | PS     | WORD | REAL       |  256 , 129
C   |-------------+--------+------+------------+------------------------
C   | T           | T      | WORD | REAL       |  256 , 129 , 28
C   |-------------+--------+------+------------+------------------------
C   | U           | U      | WORD | REAL       |  256 , 129 , 28
C   |-------------+--------+------+------------+------------------------
C   | V           | V      | WORD | REAL       |  256 , 129 , 28
C   |-------------+--------+------+------------+------------------------
C   | Q           | Q      | WORD | REAL       |  256 , 129 , 28
C   `=============^========^======^============^========================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      FUNCTION ZS(I,J)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      COMMON /STOR1/IAR(IM,JM)
      REAL IAR,ZS
      ZS = IAR(I,J)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE ZSP(I,J,V)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      COMMON /STOR1/IAR(IM,JM)
      REAL IAR,V
      IAR(I,J) = V
      RETURN
      END
C-----------------------------------------------------------------------
      FUNCTION PS(I,J)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      COMMON /STOR2/IAR(IM,JM)
      REAL IAR,PS
      PS = IAR(I,J)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE PSP(I,J,V)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      COMMON /STOR2/IAR(IM,JM)
      REAL IAR,V
      IAR(I,J) = V
      RETURN
      END
C-----------------------------------------------------------------------
      FUNCTION T(I,J,K)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      PARAMETER (KM =  28 )
      COMMON /STOR3/IAR(IM,JM,KM)
      REAL IAR,T
      T = IAR(I,J,K)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE TP(I,J,K,V)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      PARAMETER (KM =  28 )
      COMMON /STOR3/IAR(IM,JM,KM)
      REAL IAR,V
      IAR(I,J,K) = V
      RETURN
      END
C-----------------------------------------------------------------------
      FUNCTION U(I,J,K)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      PARAMETER (KM =  28 )
      COMMON /STOR4/IAR(IM,JM,KM)
      REAL IAR,U
      U = IAR(I,J,K)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE UP(I,J,K,V)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      PARAMETER (KM =  28 )
      COMMON /STOR4/IAR(IM,JM,KM)
      REAL IAR,V
      IAR(I,J,K) = V
      RETURN
      END
C-----------------------------------------------------------------------
      FUNCTION V(I,J,K)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      PARAMETER (KM =  28 )
      COMMON /STOR5/IAR(IM,JM,KM)
      REAL IAR,V
      V = IAR(I,J,K)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE VP(I,J,K,V)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      PARAMETER (KM =  28 )
      COMMON /STOR5/IAR(IM,JM,KM)
      REAL IAR,V
      IAR(I,J,K) = V
      RETURN
      END
C-----------------------------------------------------------------------
      FUNCTION Q(I,J,K)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      PARAMETER (KM =  28 )
      COMMON /STOR6/IAR(IM,JM,KM)
      REAL IAR,Q
      Q = IAR(I,J,K)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE QP(I,J,K,V)
      PARAMETER (IM =  256 )
      PARAMETER (JM =  129 )
      PARAMETER (KM =  28 )
      COMMON /STOR6/IAR(IM,JM,KM)
      REAL IAR,V
      IAR(I,J,K) = V
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      SUBROUTINE ALLFAC(PLN,DEP,INX,FAC,SNL,CSL,RSC)
 
      COMMON /SIGMAS/ IMAX,JMAX,KMAX,LMAX,DLAT,DLON,SL(100),SI(101)
      COMMON /GESPRM/ JCAP,JCAP1,JCAP2,JCAP1X2,MDIMA,MDIMB,MDIMC
 
C-CRA DIMENSION PLN(JCAP1,2,JMAX),DEP(MDIMC,2),INX(MDIMC,2)
C-CRA DIMENSION FAC(0:JCAP1,0:JCAP,3),SNL(IMAX),CSL(IMAX),RSC(JMAX)
 
      DIMENSION PLN( 63 ,2, 129 )
      DIMENSION DEP( 4158 ,2),INX( 4158 ,2)
      DIMENSION FAC(0: 63 ,0: 62 ,3)
      DIMENSION SNL( 256 ),CSL( 256 ),RSC( 129 )
 
      DATA PI180/.0174532/
 
C----------------------------------------------------------------------
      FA(X,Y)   = 6.3712E6 * SQRT((X**2-Y**2)/(4*X**2-1))/X
      FB(X,Y)   = 6.3712E6 * Y/(X**2+X)
      XLAT(J)   = (J-1)*DLAT-90.
      COLR(J)   = (90.0-ABS(XLAT(J)))*.0174532
      DEPS(X,Y) = SQRT((X**2-Y**2)/(4.0*X**2-1.0))
C-----------------------------------------------------------------------
 
C  CLEAR THE ARRAYS
C  ----------------
 
      DO I=1,JCAP1*2*JMAX
      PLN(I,1,1) = 0
      ENDDO
      DO I=1,MDIMC*2
      DEP(I,1) = 0
      INX(I,1) = 0
      ENDDO
      DO K=1,3
        DO J=0,JCAP
          DO I=0,JCAP1
            FAC(I,J,K) = 0
          ENDDO
        ENDDO
      ENDDO
      DO I=1,IMAX
        SNL(I) = 0
        CSL(I) = 0
      ENDDO
      DO I=1,JMAX
        RSC(I) = 0
      ENDDO
 
C  COMPUTE THE TRANSPOSE INDEXES FOR MDIMA AND MDIMC
C  -------------------------------------------------
 
      L = 1
      DO M=1,JCAP1
      DO N=0,JCAP1-M
      IND = N*(JCAP1X2-N+1) + 2*M - 1
      INX(L  ,1) = IND
      INX(L+1,1) = IND+1
      L=L+2
      ENDDO
      ENDDO
 
      L = 1
      DO M=1,JCAP1
      DO N=0,JCAP1-M+1
      IF(N.EQ.0) IND = 2*M-1
      IF(N.EQ.1) IND = 2*(JCAP1+M)-1
      IF(N.GT.1) IND = N*(JCAP1X2-N+3)+2*M-3
      INX(L  ,2) = IND
      INX(L+1,2) = IND+1
      L=L+2
      ENDDO
      ENDDO
 
C  COMPUTE THE PLN FACTORS FOR EACH LATITUDE EXCEPT THE POLES
C  ----------------------------------------------------------
 
      DO J=2,JMAX-1
      SINLAT = COS(COLR(J))
      COS2   = 1.0-SINLAT**2
      PROD   = 1.0
      DO N=1,JCAP1
      X = 2*N+1
      SRHP = SQRT(PROD*.5)
      PLN(N,1,J) = SRHP
      PLN(N,2,J) = SRHP*SINLAT*SQRT(X)
      PROD  = PROD*COS2*(X/(X-1.))
      ENDDO
      ENDDO
 
C  COMPUTE DEPS FOR MDIMA AND MDIMC
C  --------------------------------
 
      IATA = 1
      IATC = 1
      LEN  = JCAP
      DO N=0,JCAP1
      DO M=0,LEN
      DEPX = DEPS(FLOAT(N+M),FLOAT(M))
      DEP(IATA+2*M  ,1) = DEPX
      DEP(IATC+2*M  ,2) = DEPX
      DEP(IATA+2*M+1,1) = DEPX
      DEP(IATC+2*M+1,2) = DEPX
      ENDDO
      IATA = IATA+2*(JCAP1-N)
      IATC = IATC+2*(LEN+1)
      LEN  = LEN-MIN(N,1)
      ENDDO
 
C  THE DZTOUV FACTORS
C  ------------------
 
      DO M=0,JCAP
      DO N=M,JCAP1
      X = N
      Y = M
      IF(N.LT.JCAP              ) FAC(N,M,1) = FA(X+1,Y)
      IF(N.GT.0                 ) FAC(N,M,2) = FA(X,Y)
      IF(N.GT.0 .AND. N.LT.JCAP1) FAC(N,M,3) = FB(X,Y)
      ENDDO
      ENDDO
 
C  POLAR WIND FACTORS
C  ------------------
 
      DO I=1,IMAX
      ANG = (I-1)*DLON*PI180
      SNL(I) = SIN(ANG)
      CSL(I) = COS(ANG)
      ENDDO
 
C  RECIPROCALS OF SINES OF COLATITUDES
C  -----------------------------------
 
      DO J=2,JMAX-1
      RSC(J) = 1./SIN(COLR(J))
      ENDDO
 
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE COF2GRD(LUN,IRET)
 
      COMMON /SIGMAS/ IMAX,JMAX,KMAX,LMAX,DLAT,DLON,SL(100),SI(101)
      COMMON /GESPRM/ JCAP,JCAP1,JCAP2,JCAP1X2,MDIMA,MDIMB,MDIMC
 
C-CRA DIMENSION COF(MDIMC,2),GRI(MDIMC,2),GRD(IMAX,JMAX,2)
C-CRA DIMENSION FAC(0:JCAP1,0:JCAP,3),SNL(IMAX),CSL(IMAX),RSC(JMAX)
C-CRA DIMENSION INX(MDIMC,2),PLN(JCAP1,2,JMAX),DEP(MDIMC,2)
C-CRA DIMENSION IFAX(20),TRIGS(IMAX,2),WORK(IMAX,4)
 
      DIMENSION COF( 4158 ,2),GRI( 4158 ,2)
      DIMENSION GRD( 256 , 129 ,2)
      DIMENSION FAC(0: 63 ,0: 62 ,3)
      DIMENSION SNL( 256 ),CSL( 256 ),RSC( 129 )
      DIMENSION INX( 4158 ,2),PLN( 63 ,2, 129 ),DEP( 4158 ,2)
      DIMENSION IFAX(20),TRIGS( 256 ,2),WORK( 256 ,4)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      IRET = 0
 
C  INITIALIZE SOME STUFF
C  ---------------------
 
      CALL ALLFAC(PLN,DEP,INX,FAC,SNL,CSL,RSC)
C-CRA CALL FFTFAX(IMAX,IFAX,TRIGS)
      CALL    FAX(IFAX, IMAX,3)
      CALL FFTRIG(TRIGS,IMAX,3)
      IF(IFAX(1).EQ.0 .OR. IFAX(1).EQ.-99) GOTO 902
 
C  READ AND TRANSFORM FIELDS IN THE SPECTRAL FILE
C  ----------------------------------------------
 
      DO I=1,5
      NRD = 1
      LEV = KMAX
      NCF = MDIMA
      IF(I.LE.2) LEV = 1
      IF(I.EQ.4) NRD = 2
      IF(I.EQ.4) NCF = MDIMC
      DO L=1,LEV
      DO K=1,NRD
      READ(LUN,END=900,ERR=901) (COF(II,K),II=1,MDIMA)
      ENDDO
 
      IF(I.EQ.4) CALL DZTOUV(COF,FAC)
 
      DO J=2,JMAX-1
      CALL PLNSUM(COF,NCF,J,INX(1,NRD),PLN(1,1,J),DEP(1,NRD),GRI)
      DO K=1,NRD
C-CRA CALL RFFTMLT(GRI(1,K),WORK,TRIGS,IFAX,1,IMAX,IMAX,1,1)
      CALL FFT99M (GRI(1,K),WORK,TRIGS,IFAX,1,IMAX,IMAX,1,1)
      DO IM=1,IMAX
      GRD(IM,J,K) = GRI(IM,K)
      ENDDO
      ENDDO
      ENDDO
      CALL POLES(GRD,I,SNL,CSL,RSC)
      CALL GUSER(GRD,I,L)
      ENDDO
      ENDDO
 
      RETURN
900   PRINT*,'COF2GRD - EOF READING GUESS  '
      IRET = -1
      RETURN
901   PRINT*,'COF2GRD - ERROR READING GUESS'
      IRET = -1
      RETURN
902   PRINT*,'COF2GRD - IDIM NOT FACTORABLE'
      IRET = -1
      RETURN
      END
C----------------------------------------------------------------------
C  THIS ROUTINE PERFORMS CONVERSION OF TRUE SCALAR RELATIVE
C  VORTICITY (Z), DIVERGENCE (D) TO PSEUDO SCALAR U AND V
C  IN SPECTRAL SPACE.
C
C  CALL DZTOUV(NLV,U,V)
C
C  NLV ......... NUMBER OF LEVELS TO CONVERT
C  U (INPUT) ... DIVERGENCE
C  V (INPUT) ... RELATIVE VORTICITY
C  U (OUTPUT) .. OUTPUT PSEUDO SCALAR ZONAL WIND (=UCOS(PHI))
C  V (OUTPUT) .. OUTPUT PSEUDO SCALAR MERID WIND (=VCOS(PHI))
C
C  HUA-LU PAN   27 FEBRUARY 1989
C  J WOOLLEN    14 MARCH    1993
C
C   MODIFIED TO RUN ON THE CRAY
C----------------------------------------------------------------------
      SUBROUTINE DZTOUV(COF,F)
 
      COMMON /GESPRM/ JCAP,JCAP1,JCAP2,JCAP1X2,MDIMA,MDIMB,MDIMC
 
C-CRA DIMENSION COF(MDIMC,2),F(0:JCAP1,0:JCAP,3)
C-CRA DIMENSION D(-1:MDIMA+4),Z(-1:MDIMA+4)
C-CRA DIMENSION U(MDIMC),V(MDIMC)
 
      DIMENSION COF( 4158 ,2),F(0: 63 ,0: 62 ,3)
      DIMENSION D(-1: 4032 +4),Z(-1: 4032 +4)
      DIMENSION U( 4158 ),V( 4158 )
 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
 
C  CONVERT D AND Z TO SCALED U AND V
C  ---------------------------------
 
      DO I=-1,MDIMA+4
      D(I) = 0
      Z(I) = 0
      ENDDO
 
      DO I=1,MDIMA
      D(I) = COF(I,1)
      Z(I) = COF(I,2)
      ENDDO
 
      MDZ = 0
      MUV = 0
 
      DO M=0,JCAP
      DO N=M,JCAP1
      MUV = MUV+2
      MDZ = MDZ+2
      U(MUV-1) =  Z(MDZ+1)*F(N,M,1)-Z(MDZ-3)*F(N,M,2)+D(MDZ  )*F(N,M,3)
      U(MUV  ) =  Z(MDZ+2)*F(N,M,1)-Z(MDZ-2)*F(N,M,2)-D(MDZ-1)*F(N,M,3)
      V(MUV-1) = -D(MDZ+1)*F(N,M,1)+D(MDZ-3)*F(N,M,2)+Z(MDZ  )*F(N,M,3)
      V(MUV  ) = -D(MDZ+2)*F(N,M,1)+D(MDZ-2)*F(N,M,2)-Z(MDZ-1)*F(N,M,3)
      ENDDO
      MDZ = MDZ-2
      ENDDO
 
      DO I=1,MDIMC
      COF(I,1) = U(I)
      COF(I,2) = V(I)
      ENDDO
 
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GESRES(LUN,IRET)
 
      COMMON /SIGMAS/ IMAX,JMAX,KMAX,LMAX,DLAT,DLON,SL(100),SI(101)
      COMMON /GESPRM/ JCAP,JCAP1,JCAP2,JCAP1X2,MDIMA,MDIMB,MDIMC
      COMMON /GESDAT/ FCLABL(4),IDATE,VDATE
      CHARACTER*8 FCLABL,IDATE,VDATE
 
      DIMENSION HEADR2(207)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      IRET = 0
 
C  READ SIGGES HEADERS
C  -------------------
 
      JCAP  =  62
      KMAX  =  28
 
C-MK  REWIND LUN
C-MK  READ(LUN,END=900,ERR=901) FCLABL
C-MK  READ(LUN,END=900,ERR=5  ) FHR,IH,IM,ID,IY,HEADR2
C-MK  GOTO 6
 
C-MK5     CONTINUE
C-MK  PRINT*,' ATTEMPTING TO READ GES RESOLUTION FROM UNIT 5'
C-MK  READ(5,*,END=901,ERR=901) JCAP,KMAX
 
      REWIND LUN
      READ(LUN,END=900,ERR=901) FCLABL
      READ(LUN,END=900,ERR=901) FHR,IH,IM,ID,IY,(HEADR2(I),I=1,2*KMAX+1)
 
C-MK  HEADR2(202) = JCAP
C-MK  HEADR2(203) = KMAX
C-MK 6     CONTINUE
 
C  EXTRACT HEADER INFO
C  -------------------
 
C-MK  JCAP  = HEADR2(202)
C-MK  KMAX  = HEADR2(203)
 
      IF(KMAX.GT.100) GOTO 902
 
      DO L=1,KMAX
      SI(L) = HEADR2(L)
      SL(L) = HEADR2(KMAX+1+L)
      ENDDO
 
      SI(KMAX+1) = HEADR2(KMAX+1)
 
      JHR = FHR
      CALL W3FS03(IDI,IH,IY,IM,ID,0)
      CALL W3FS15(IDI,JHR,IDV)
      CALL W3FS03(IDV,JH,JY,JM,JD,1)
      WRITE(IDATE,'(4I2)') IY,IM,ID,IH
      WRITE(VDATE,'(4I2)') JY,JM,JD,JH
      DO I=1,8
      IF(IDATE(I:I).EQ.' ') IDATE(I:I) = '0'
      IF(VDATE(I:I).EQ.' ') VDATE(I:I) = '0'
      ENDDO
      WRITE(6,1) JHR,IDATE,VDATE
1     FORMAT(1X,'A ',I3,' HOUR FORECAST FROM ',A8,' VALID AT ',A8)
 
C  DEFINE THE OTHER RESOLUTION PARAMETERS
C  --------------------------------------
 
      JCAP1   = JCAP+1
      JCAP2   = JCAP+2
      JCAP1X2 = JCAP1*2
      MDIMA   = JCAP1*JCAP2
      MDIMB   = MDIMA/2+JCAP1
      MDIMC   = MDIMB*2
 
C-MK  IMAX    = 256
 
      IMAX    =  256
      JMAX    = IMAX/2+1
 
      IF(IMAX.LT.JCAP1X2) GOTO 903
 
      DLAT  = 180./(JMAX-1)
      DLON  = 360./IMAX
 
      WRITE(6,2) JCAP,KMAX,DLAT,DLON
2     FORMAT(1X,'T',I3,' ',I2,' LEVELS -------> ',F3.1,' X ',F3.1)
 
      CALL COF2GRD(LUN,IRET)
 
      RETURN
900   PRINT*,'GESRES - EOF   READING GUESS ON LUN ',LUN
      IRET = -1
      RETURN
901   PRINT*,'GESRES - ERROR READING GUESS ON LUN ',LUN
      IRET = -1
      RETURN
902   PRINT*,'GESRES - KMAX TOO BIG = ',KMAX
      IRET = -1
      RETURN
903   PRINT*,'GESRES - IMAX TOO SMALL = ',IMAX
      IRET = -1
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE PLNSUM(COF,NCF,J,INX,PLN,DEP,GRD)
 
      COMMON /GESPRM/ JCAP,JCAP1,JCAP2,JCAP1X2,MDIMA,MDIMB,MDIMC
      COMMON /SIGMAS/ IMAX,JMAX,KMAX,LMAX,DLAT,DLON,SL(100),SI(101)
 
C-CRA DIMENSION COF(MDIMC,2),GRD(MDIMC,2)
C-CRA DIMENSION INX(MDIMC),PLN(JCAP1,2),DEP(MDIMC)
C-CRA DIMENSION QLN(MDIMC)
 
      DIMENSION COF( 4158 ,2),GRD( 4158 ,2)
      DIMENSION INX( 4158 ),PLN( 63 ,2),DEP( 4158 )
      DIMENSION QLN( 4158 )
 
C-----------------------------------------------------------------------
      XLAT(J)   = (J-1)*DLAT-90.
      COLR(J)   = (90.0-ABS(XLAT(J)))*.0174532
C-----------------------------------------------------------------------
 
      DO I=1,MDIMC*2
      GRD(I,1) = 0
      ENDDO
 
C  SET THE FIELD RELATED PARAMETERS
C  --------------------------------
 
      IF(NCF.EQ.MDIMA) THEN
         NCR = 1
         N0  = 1
      ELSE IF(NCF.EQ.MDIMC) THEN
         NCR = 2
         N0  = 0
      ELSE
         CALL SABORT('PLNSUM - UNKNOWN VALUE OF NCF')
      ENDIF
 
C  TRANSPOSE THE INPUT COEFFICIENTS
C  --------------------------------
 
      IF(J.EQ.2) THEN
         DO K=1,NCR
         DO I=1,NCF
         QLN(INX(I)) = COF(I,K)
         ENDDO
         DO I=1,NCF
         COF(I,K) = QLN(I)
         ENDDO
         ENDDO
      ENDIF
 
C  MAKE QLN FOR A SPECIFIC LATITUDE
C  --------------------------------
 
      SINLAT = COS(COLR(J))
 
      DO N=1,JCAP1
      N1 = 2*N-1
      N2 = N1+JCAP1X2
      QLN(N1  ) = PLN(N,1)
      QLN(N1+1) = PLN(N,1)
      QLN(N2  ) = PLN(N,2)
      QLN(N2+1) = PLN(N,2)
      ENDDO
 
      LP0 = 2*JCAP1X2-2*N0
      LP1 = JCAP1X2
      LP2 = 0
 
      DO N=1,JCAP
      LEN = JCAP1X2-2*(N+N0)
      DO L=1,LEN
      QLN(LP0+L) = (SINLAT*QLN(LP1+L)-DEP(LP1+L)*QLN(LP2+L))/DEP(LP0+L)
      ENDDO
      LP2 = LP1
      LP1 = LP0
      LP0 = LP0 + LEN
      ENDDO
 
 
C  SUM THE COEFFICIENTS FOR THIS LATITUDE
C  --------------------------------------
 
      LL   = 0
      FST  = 0
      HEM  = 1
      LEN  = JCAP1X2
      AHEM = SIGN(1.,XLAT(J))
 
      DO N=N0,JCAP1
      DO K=1,NCR
      DO L=1,LEN
      GRD(L,K) = COF(L+LL,K)*QLN(L+LL)*HEM + GRD(L,K)
      ENDDO
      ENDDO
 
      LL  = LL+LEN
      LEN = JCAP1X2-2*N
      HEM = HEM*AHEM
      FST = 1
      ENDDO
 
C     IF(NCF.EQ.MDIMC) THEN
C     WRITE(50)(COF(I,1),I=1,NCF)
C     WRITE(50)(QLN(I),I=1,NCF)
C     WRITE(50)(GRD(I,1),I=1,JCAP1X2)
C     ENDIF
 
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE POLES(GRD,IQ,SNL,CSL,RSC)
 
      COMMON /SIGMAS/ IMAX,JMAX,KMAX,LMAX,DLAT,DLON,SL(100),SI(101)
 
C-CRA DIMENSION GRD(IMAX,JMAX,2),SNL(IMAX),CSL(IMAX),RSC(JMAX)
      DIMENSION GRD( 256 , 129 ,2)
      DIMENSION SNL( 256 ),CSL( 256 ),RSC( 129 )
 
      DATA PI180/.0174532/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      RIMAX = 1./IMAX
 
C  SURFACE PRESSURE AND WINDS GET SPECIAL TREATMENT
C  ------------------------------------------------
 
      IF(IQ.EQ.2) THEN
         DO J=2,JMAX-1
         DO I=1,IMAX
         GRD(I,J,1) = 10.*EXP(GRD(I,J,1))
         ENDDO
         ENDDO
      ELSEIF(IQ.EQ.4) THEN
         DO J=2,JMAX-1
         DO I=1,IMAX
         GRD(I,J,1) = GRD(I,J,1)*RSC(J)
         GRD(I,J,2) = GRD(I,J,2)*RSC(J)
         ENDDO
         ENDDO
         GOTO 20
      ENDIF
 
C  AVERAGE THE NEAREST ZONE FOR SCALAR POLES
C  -----------------------------------------
 
10    DO J=1,JMAX,JMAX-1
      IF(J.EQ.JMAX) JN = JMAX-1
      IF(J.EQ.1   ) JN = 2
      POLE = 0
      DO I=1,IMAX
      POLE = POLE + GRD(I,JN,1)*RIMAX
      ENDDO
      DO I=1,IMAX
      GRD(I,J,1) = POLE
      ENDDO
      ENDDO
 
      RETURN
 
C  AVERAGE THE NEAREST ZONE FOR VECTOR POLES
C  -----------------------------------------
 
20    DO J=1,JMAX,JMAX-1
      IF(J.EQ.JMAX) JN = JMAX-1
      IF(J.EQ.1   ) JN = 2
      UPOLE = 0
      VPOLE = 0
      DO I=1,IMAX
      SNL(I) = -SNL(I)
      UPOLE = UPOLE + (GRD(I,JN,1)*CSL(I)-GRD(I,JN,2)*SNL(I))*RIMAX
      VPOLE = VPOLE + (GRD(I,JN,1)*SNL(I)+GRD(I,JN,2)*CSL(I))*RIMAX
      ENDDO
      DO I=1,IMAX
      GRD(I,J,1) =  UPOLE*CSL(I) + VPOLE*SNL(I)
      GRD(I,J,2) = -UPOLE*SNL(I) + VPOLE*CSL(I)
      ENDDO
      ENDDO
 
      RETURN
      END
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      PROGRAM POSTVENT
 
      COMMON /SIGFAC/ MAXPRS,MXXXXX,SITERP(1200),SLTERP(1200)
      COMMON /MANFAC/ MAXP,MAND,PMAN(21),LEVP(1200)
      COMMON /WORDL/  NBITW
 
      CHARACTER*80 HEADR,OBSTR,QMSTR,GSSTR,ANSTR
      CHARACTER*8  SUBSET
 
      DIMENSION    GFIT(3,21,4,6,100)
      DIMENSION    AFIT(3,21,4,6,100)
      DIMENSION    OBS(10,255),QMS(10,255),HDR(10)
      DIMENSION    FCB(10,255),ANB(10,255)
 
      DATA HEADR /'SID XOB YOB DHR TYP ELV                   '/
      DATA OBSTR /'POB QOB TOB ZOB UOB VOB PWO CAT           '/
      DATA QMSTR /'PQM QQM TQM ZQM WQM NUL PWQ               '/
      DATA GSSTR /'PFC QFC TFC ZFC UFC VFC PWF               '/
      DATA ANSTR /'PAN QAN TAN ZAN UAN VAN PWA               '/
 
      DATA QMAX  /3    /
      DATA LUBFI /20   /
      DATA LUANA /21   /
      DATA LUBFO /50   /
      DATA LUNFG /51   /
      DATA LUNFA /52   /
      DATA LUNDM /53   /
      DATA LUBFA /54   /
      DATA BMISS /10E10/
 
C----------------------------------------------------------------------
C----------------------------------------------------------------------
 
ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib
 
      PRINT*
      PRINT*,'******  BEGINNING POSTVENT PROCESSING ******'
      PRINT*
 
C  DETERMINE THE NUMBER OF BITS IN A WORK ON THIS MACHINE
C  ------------------------------------------------------
      CALL W3FI01(LW)
      NBITW = LW*8
 
C  CHECK THE BUFR DATE WITH THE SIGMA VALID TIME
C  ---------------------------------------------
 
      CALL DATECK(LUANA,LUBFI,NMCDATE)
 
C  TRANSFORM THE SIGMA FILE AND SET INTERPOLATION FACTORS
C  ------------------------------------------------------
 
      CALL GESRES(LUANA,IGES)
      IF(IGES.NE.0) CALL SABORT('UNABLE TO PROCESS SANL FILE')
      CALL SETTERP
 
C  INITIALIZE THE FIT FILES
C  ------------------------
 
      DO I=1,3*21*4*6*100
      AFIT(I,1,1,1,1) = 0
      GFIT(I,1,1,1,1) = 0
      ENDDO
 
      DO M=1,MAND
      AFIT(1,M,1,1,1) = PMAN(M)
      GFIT(1,M,1,1,1) = PMAN(M)
      ENDDO
 
C  OPEN THE INPUT/OUTPUT BUFR FILES AND GET THE SHOW ON THE ROAD
C  -------------------------------------------------------------
 
      CALL OPENBF(LUBFI,'IN ',LUBFI)
      CALL OPENBF(LUBFO,'OUT',LUBFI)
 
C  READ THROUGH THE DATA ONE REPORT AT A TIME
C  ------------------------------------------
 
CMIC$ PARALLEL AUTOSCOPE
CMIC$.SHARED   (LUBFI,LUBFO,GFIT,AFIT,QMAX)
CMIC$.SHARED   (HEADR,OBSTR,QMSTR,GSSTR,ANSTR)
CMIC$.PRIVATE  (SUBSET,IDATE,HDR,OBS,QMS,FCB,ANB,NLEV,IRET)
 
      DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
      CALL OPENMB(LUBFO,SUBSET,IDATE)
      DO WHILE(IREADSB(LUBFI).EQ.0)
      CALL UFBCPY(LUBFI,LUBFO)
 
C  READ OUT THE HDR, OBS, QMS, AND FORECAST VALUES FROM DATA
C  ---------------------------------------------------------
 
      CALL UFBINT(LUBFI,HDR,10,  1,NLEV,HEADR)
      CALL UFBINT(LUBFI,OBS,10,255,NLEV,OBSTR)
      CALL UFBINT(LUBFI,QMS,10,255,NLEV,QMSTR)
      CALL UFBINT(LUBFI,FCB,10,255,NLEV,GSSTR)
 
C  INTERPOLATE SANL TO DATA AND WRITE BACK AS EVENTS
C  -------------------------------------------------
 
      CALL GETSIG(HDR,OBS,QMS,ANB,NLEV)
      CALL UFBINT(LUBFO,ANB,10,NLEV,IRET,ANSTR)
      CALL WRITSB(LUBFO)
 
C  SAVE OBS-ANA AND OBS-GES FITS FOR DATA WHERE QM <= QMAX
C  -------------------------------------------------------
 
CMIC$ GUARD 0
      CALL BAKFIT(AFIT,QMAX,HDR,OBS,QMS,ANB,NLEV)
      CALL BAKFIT(GFIT,QMAX,HDR,OBS,QMS,FCB,NLEV)
CMIC$ ENDGUARD 0
 
      ENDDO
      ENDDO
 
      WRITE(6,*) 'BAKFIT LOOP COMPLETED'
C-SGI CALL FLUSH(6)
C-HP   CALL FLUSH(6)
 
      CALL CLOSMG(LUBFO)
CMIC$ END PARALLEL
 
      CALL CLOSBF(LUBFI)
      CALL CLOSBF(LUBFO)
 
C  WRITE THE FIT FILES
C  -------------------
 
      CALL WRTOBO(LUNFG,LUNDM,MAND,GFIT,NMCDATE)
      CALL WRTOBO(LUNFA,LUNDM,MAND,AFIT,NMCDATE)
 
C  CREATE FINAL BUFR FILE WITH CONSOLIDATED SUBSETS
C  ------------------------------------------------
 
C-CRA CALL UFBPKS(LUBFO,LUBFA)
 
C  END OF POSTVENTION
C  ------------------
 
      STOP
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE DATECK(LUSIG,LUBFR,NMCDATE)
 
      CHARACTER*8  O85LAB(4),SUBSET,SDATE,PDATE
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  SIGMA FILE
C  ----------
 
      REWIND LUSIG
      READ(LUSIG,END=900,ERR=900) O85LAB
C     READ(LUSIG,END=900,ERR=900) FHOUR
      READ(LUSIG,END=900,ERR=900) FHOUR,IH,IM,ID,IY
	WRITE(*,*) 'LUSIG: FHOUR/IH/IM/ID/IY=',FHOUR,IH,IM,ID,IY
	IY=MOD(IY,100)
      JFHOUR = FHOUR
C     CALL W3FS11(O85LAB(2),IY,IM,ID,IH,1)
	IF (IY.EQ.0) THEN
C	    ASSUME 2000
	    IY=IY+80
            CALL W3FS03(IDI,IH,IY,IM,ID,0)
            CALL W3FS15(IDI,JFHOUR,IDV)
            CALL W3FS03(IDV,IH,IY,IM,ID,1)
	    IY=IY-80
	ELSE
            CALL W3FS03(IDI,IH,IY,IM,ID,0)
            CALL W3FS15(IDI,JFHOUR,IDV)
            CALL W3FS03(IDV,IH,IY,IM,ID,1)
	ENDIF
      WRITE(SDATE,'(4I2.2)') IY,IM,ID,IH
      PRINT'(''GUESS VALID AT  '',A8)',SDATE
      CLOSE (LUSIG)
 
C  PREPDA FILE
C  -----------
 
      CALL DATEBF(LUBFR,IY,IM,ID,IH,IDATE)
      WRITE(PDATE,'(I8.8)') IDATE
	WRITE(*,*) 'BUFFER DATE'
      PRINT'(''DATA  VALID AT  '',A8)',PDATE
 
C  VALID DATES MUST MATCH
C  ----------------------
 
      IF(SDATE.NE.PDATE) GOTO 902
      NMCDATE = IDATE
 
      RETURN
900   CALL SABORT('DATECK - BAD OR MISSING SIGMA FILE     ')
901   CALL SABORT('DATECK - BAD OR MISSING PREPD FILE     ')
902   CALL SABORT('DATECK - DATES DONT MATCH              ')
      END
C-----------------------------------------------------------------------
C  GETFC - INTERPOLATE SIGMA VARS TO OB LOCATIONS
C-----------------------------------------------------------------------
      SUBROUTINE GETSIG(HDR,OBS,QMS,BAK,NLEV)
 
C-CRA TASKCOMMON /GUESS/  PS,ZS,T(100),U(100),V(100),Q(100)
          COMMON /GUESS/  PS,ZS,T(100),U(100),V(100),Q(100)
 
      COMMON /SIGFAC/ MAXPRS,MXXXXX,SITERP(1200),SLTERP(1200)
      COMMON /SIGMAS/ IMAX,JMAX,KMAX,LMAX,DLAT,DLON,SL(100),SI(101)
 
      DIMENSION HDR(10),OBS(10,NLEV),QMS(10,NLEV),BAK(10,NLEV)
 
C-CRA DIMENSION PINT(KMAX),ZINT(KMAX)
      DIMENSION PINT( 28 ),ZINT( 28 )
 
      DATA BMISS / 10E10  /
      DATA TZERO / 273.15 /
      DATA BETAP / .0552  /
      DATA BETA  / .00650 /
      DATA ROG   / 29.261 /
      DATA G     / 9.81   /
      DATA R     / 287.05 /
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  CLEAR THE BACKGROUND EVENT ARRAY
C  --------------------------------
 
      DO I=1,10*NLEV
      BAK(I,1) = BMISS
      ENDDO
 
C  GET SIGMA PROFILE AT OB LOCATION
C  --------------------------------
 
      SID = HDR(1)
      XOB = HDR(2)
      YOB = HDR(3)
      DHR = HDR(4)
      TYP = HDR(5)
 
      CALL HTERP(XOB,YOB)
      PS000 = 1000./PS
      PSIG  = PS*SL(1)
      PINT(1) = PS
      ZINT(1) = ZS
      DO K=2,KMAX
      K0 = K-1
      PINT(K) = PS*SI(K)
      ZINT(K) = ZINT(K0) - ROG*T(K0)*LOG(PINT(K)/PINT(K0))
      ENDDO
 
C  INTERPOLATE GUESS PROFILES TO OB PRESSURES
C  ------------------------------------------
 
      DO 10 L=1,NLEV
 
      POB = OBS(1,L)
      QOB = OBS(2,L)
      TOB = OBS(3,L)
      ZOB = OBS(4,L)
      UOB = OBS(5,L)
      VOB = OBS(6,L)
      PWO = OBS(7,L)
      CAT = OBS(8,L)
      IF(POB.LE.0 .OR. POB.GE.BMISS) GOTO 10
 
      IP  = POB*PS000
      IP  = MIN(IP,MAXPRS)
      IP  = MAX(IP,1)
 
C  SURFACE PRESSURE
C  ----------------
 
      IF(CAT.EQ.0 .AND. ZOB.LT.BMISS) THEN
         TS = T(1) + (PS-PSIG)*BETAP
         DZ  = ZOB-ZS
         TM  = TS - DZ*BETA*.5
         PFC = PS*EXP(-DZ/(TM*ROG))
      ELSE
         PFC = BMISS
      ENDIF
 
C  SPECIFIC HUMIDITY
C  -----------------
 
      IF(QOB.LT.BMISS) THEN
         LB = SLTERP(IP)
         WT = SLTERP(IP)-LB
         LA = MIN(LB+1,KMAX)
         QOB = Q(LB) + (Q(LA)-Q(LB))*WT
      ENDIF
 
C  TEMPERATURE
C  -----------
 
      IF(TOB.LT.BMISS) THEN
         IF(POB.GT.PSIG) THEN
            TOB = T(1) + (POB-PSIG)*BETAP
         ELSE
            LB = SLTERP(IP)
            WT = SLTERP(IP)-LB
            LA = MIN(LB+1,KMAX)
            TOB = T(LB) + (T(LA)-T(LB))*WT
         ENDIF
         TOB = TOB - TZERO
      ENDIF
 
C  HEIGHT
C  ------
 
      IF(ZOB.LT.BMISS) THEN
         IF(POB.GT.PSIG) THEN
            TM = T(1) + (.5*(PINT(1)+POB)-PSIG)*BETAP
            ZOB = ZINT(1) - ROG*TM*LOG(POB/PINT(1))
         ELSE
            LI = SITERP(IP)
            MP = (POB+PINT(LI))*PS000*.5
            MP = MAX(MIN(MP,MAXPRS),1)
            LB = SLTERP(MP)
            WT = SLTERP(MP)-LB
            LA = MAX(MIN(LB+1,KMAX),1)
            TM = T(LB) + (T(LA)-T(LB))*WT
            ZOB = ZINT(LI) - ROG*TM*LOG(POB/PINT(LI))
         ENDIF
      ENDIF
 
C  U AND V COMPONENTS
C  ------------------
 
      IF(UOB.LT.BMISS .OR. VOB.LT.BMISS) THEN
         LB = SLTERP(IP)
         WT = SLTERP(IP)-LB
         LA = MIN(LB+1,KMAX)
         UOB = U(LB) + (U(LA)-U(LB))*WT
         VOB = V(LB) + (V(LA)-V(LB))*WT
      ENDIF
 
C  PRECIPITABLE WATER
C  ------------------
 
      PWO = BMISS
 
C  RELATIVE HUMIDITY
C  -----------------
 
      RHO = BMISS
 
C  SCATTER THE PROPER FORECAST VALUES
C  ----------------------------------
 
      BAK(1,L) = PFC
      BAK(2,L) = QOB
      BAK(3,L) = TOB
      BAK(4,L) = ZOB
      BAK(5,L) = UOB
      BAK(6,L) = VOB
      BAK(7,L) = PWO
      BAK(8,L) = RHO
 
10    ENDDO
 
      RETURN
      END
C-----------------------------------------------------------------------
C  SUBROUTINE GUSER - GESRES USER INTERFACE FOR PREPFIT (PS,ZS,T,U,V)
C-----------------------------------------------------------------------
      SUBROUTINE GUSER(GRD,IQ,LEV)
 
      COMMON /SIGMAS/ IMAX,JMAX,KMAX,LMAX,DLAT,DLON,SL(100),SI(101)
 
C-CRA DIMENSION   GRD(IMAX,JMAX,2)
      DIMENSION   GRD( 256 , 129 ,2)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  PACK 2D GUESS FIELD INTO BIT PACKED ARRAYS
C  ------------------------------------------
 
      IF(IQ.EQ.1) THEN
         DO J=1,JMAX
         DO I=1,IMAX
         CALL ZSP(I,J,GRD(I,J,1))
         ENDDO
         ENDDO
      ELSEIF(IQ.EQ.2) THEN
         DO J=1,JMAX
         DO I=1,IMAX
         CALL PSP(I,J,GRD(I,J,1))
         ENDDO
         ENDDO
      ELSEIF(IQ.EQ.3) THEN
         DO J=1,JMAX
         DO I=1,IMAX
         CALL TP(I,J,LEV,GRD(I,J,1))
         ENDDO
         ENDDO
      ELSEIF(IQ.EQ.4) THEN
         DO J=1,JMAX
         DO I=1,IMAX
         CALL UP(I,J,LEV,GRD(I,J,1))
         CALL VP(I,J,LEV,GRD(I,J,2))
         ENDDO
         ENDDO
      ELSEIF(IQ.EQ.5) THEN
         DO J=1,JMAX
         DO I=1,IMAX
         CALL QP(I,J,LEV,MAX(0.,GRD(I,J,1))*1E6)
         ENDDO
         ENDDO
      ENDIF
 
      RETURN
      END
C-----------------------------------------------------------------------
C  SUBROUTINE HTERP - 2D LINEAR HORIZONTAL INTERPOLATION
C-----------------------------------------------------------------------
      SUBROUTINE HTERP(XOB,YOB)
 
C-CRA TASKCOMMON /GUESS/ PSI,ZSI,TI(100),UI(100),VI(100),QI(100)
          COMMON /GUESS/ PSI,ZSI,TI(100),UI(100),VI(100),QI(100)
      COMMON /SIGMAS/ IMAX,JMAX,KMAX,LMAX,DLAT,DLON,SL(100),SI(101)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  CALCULATE HORIZONTAL WEIGHTS AND INTERPOLATE
C  --------------------------------------------
 
      WX = XOB/DLON + 1.0
      I0 = WX
      I1 = MOD(I0,IMAX) + 1
      WX = WX-I0
 
      WY = (YOB+90.)/DLAT + 1.0
      J0 = WY
      J1 = MIN(J0+1,JMAX)
      WY = WY-J0
 
C  HTERP FOR SURFACE HEIGHT
C  ------------------------
 
      P1  = ZS(I0,J0)
      P2  = ZS(I0,J1)
      P3  = ZS(I1,J0)
      P4  = ZS(I1,J1)
      P5  = P1+(P2-P1)*WY
      P6  = P3+(P4-P3)*WY
      ZSI = P5+(P6-P5)*WX
 
C  HTERP FOR SURFACE PRESSURE
C  --------------------------
 
      P1  = PS(I0,J0)
      P2  = PS(I0,J1)
      P3  = PS(I1,J0)
      P4  = PS(I1,J1)
      P5  = P1+(P2-P1)*WY
      P6  = P3+(P4-P3)*WY
      PSI = P5+(P6-P5)*WX
 
C  HTERP FOR UPA T,U,V,Q
C  ---------------------
 
      DO K=1,KMAX
 
      P1 = T(I0,J0,K)
      P2 = T(I0,J1,K)
      P3 = T(I1,J0,K)
      P4 = T(I1,J1,K)
      P5 = P1+(P2-P1)*WY
      P6 = P3+(P4-P3)*WY
      TI(K) = P5+(P6-P5)*WX
 
      P1 = U(I0,J0,K)
      P2 = U(I0,J1,K)
      P3 = U(I1,J0,K)
      P4 = U(I1,J1,K)
      P5 = P1+(P2-P1)*WY
      P6 = P3+(P4-P3)*WY
      UI(K) = P5+(P6-P5)*WX
 
      P1 = V(I0,J0,K)
      P2 = V(I0,J1,K)
      P3 = V(I1,J0,K)
      P4 = V(I1,J1,K)
      P5 = P1+(P2-P1)*WY
      P6 = P3+(P4-P3)*WY
      VI(K) = P5+(P6-P5)*WX
 
      P1 = Q(I0,J0,K)
      P2 = Q(I0,J1,K)
      P3 = Q(I1,J0,K)
      P4 = Q(I1,J1,K)
      P5 = P1+(P2-P1)*WY
      P6 = P3+(P4-P3)*WY
      QI(K) = P5+(P6-P5)*WX
 
      ENDDO
 
      RETURN
      END
C-----------------------------------------------------------------------
C  SUBROUTINE SETTERP
C-----------------------------------------------------------------------
      SUBROUTINE SETTERP
 
      COMMON /SIGFAC/ MAXPRS,MXXXXX,SITERP(1200),SLTERP(1200)
      COMMON /MANFAC/ MAXP,MAND,PMAN(21),LEVP(1200)
      COMMON /SIGMAS/ IMAX,JMAX,KMAX,LMAX,DLAT,DLON,SL(100),SI(101)
 
      PARAMETER (MANDIM=21)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      MAXPRS = 1200
 
C  COMPUTE TABLE FOR THE SIGMA MIDPOINTS RELATIVE TO 1000 MB
C  ---------------------------------------------------------
 
      DO 20 I=1,MAXPRS
      P = I
      DO 10 LA=1,KMAX
      IF(P.GE.SL(LA)*1000.) GOTO 15
10    CONTINUE
15    IF(LA.GT.KMAX) THEN
         LA = KMAX
         LB = KMAX
      ELSE IF(LA.EQ.1) THEN
         LA = 1
         LB = 1
      ELSE
         LB = LA-1
      ENDIF
      IF(LA.EQ.LB) THEN
         WP = 0.
      ELSE
         PA = SL(LA)*1000.
         PB = SL(LB)*1000.
         WP = LOG(P/PB)/LOG(PA/PB)
      ENDIF
      SLTERP(I) = LB + WP
20    CONTINUE
 
C  COMPUTE TABLE FOR THE SIGMA INTERFACES RELATIVE TO 1000 MB
C  ----------------------------------------------------------
 
      DO 40 I=1,MAXPRS
      P = I
      DO 30 LA=1,KMAX
      IF(P.GE.SI(LA)*1000.) GOTO 35
30    CONTINUE
35    SITERP(I) = MAX(LA-1,1)
40    CONTINUE
 
C  COMPUTE TABLE FOR MANDATORY LEVEL INDEX LOOKUP
C  ----------------------------------------------
 
      MAXP = 1200
      MAND = MANDIM
 
      PMAN( 1) = 1000
      PMAN( 2) = 925
      PMAN( 3) = 850
      PMAN( 4) = 700
      PMAN( 5) = 500
      PMAN( 6) = 400
      PMAN( 7) = 300
      PMAN( 8) = 250
      PMAN( 9) = 200
      PMAN(10) = 150
      PMAN(11) = 100
      PMAN(12) = 70
      PMAN(13) = 50
      PMAN(14) = 30
      PMAN(15) = 20
      PMAN(16) = 10
      PMAN(17) = 7
      PMAN(18) = 5
      PMAN(19) = 3
      PMAN(20) = 2
      PMAN(21) = 1
 
      DO I=1,MAXP
      DMIN = 10E10
      DO M=1,MAND
      PMIN = ABS(I-PMAN(M))
      IF(PMIN.LT.DMIN) THEN
         DMIN = PMIN
         MMIN = M
      ENDIF
      ENDDO
      LEVP(I) = MMIN
      ENDDO
 
      RETURN
      END
      SUBROUTINE W3FS03(IDATE,IHOUR,IYEAR,MONTH,IDAY,NN)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FS03         NMC DATE WORD PACKER AND UNPACKER
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 87-03-24
C
C ABSTRACT: OBTAINS THE COMPONENTS OF THE NMC DATE WORD (SEE NMC
C   O.N. 84 AND 85) OR GIVEN ITS COMPONENTS, FORMS AN NMC TYPE
C   DATE WORD. W3FS03 IS THE SAME AS W3FS11 EXCEPT FOR THE ORDER OF
C   THE PARAMETERS.
C
C PROGRAM HISTORY LOG:
C 87-03-24  R.E.JONES   CONVERT TO CYBER 205 FORTRAN 200
C 89-10-13  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C
C USAGE:  CALL W3FS03 (IDATE,IHOUR,IYEAR,MONTH,IDAY,NN)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IDATE  ARG LIST  LEFT 4 BYTES OF INTEGER WORD
C                      WORD. (7TH WORD OF DATA FIELD OR 3RD WORD OF
C                      THE ID TABLE OF A BINARY FILE IF IN HALF
C                      PRECISION ARRAY OR THE 4TH WORD OR 2ND WORD IF
C                      IN INTEGER ARRAY).
C     IHOUR  ARG LIST  HOUR
C     IYEAR  ARG LIST  YEAR (2 DIGITS)
C     MONTH  ARG LIST  MONTH
C     IDAY   ARG LIST  DAY
C     NN     ARG LIST  CODE:
C                       = 0  PACK IHOUR, IYEAR, MONTH, IDAY, INTO IDATE
C                      <> 0  UNPACK IDATE INTO IHOUR, IYEAR, MONTH, IDAY
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IDATE  ARG LIST  LEFT 4 BYTES OF INTEGER WORD
C                      WORD. (7TH WORD OF DATA FIELD OR 3RD WORD OF
C                      THE ID TABLE OF A BINARY FILE IF IN HALF
C                      PRECISION ARRAY OR THE 4TH WORD OR 2ND WORD IF
C                      IN INTEGER ARRAY).
C     IHOUR  ARG LIST  HOUR
C     IYEAR  ARG LIST  YEAR (2 DIGITS)
C     MONTH  ARG LIST  MONTH
C     IDAY   ARG LIST  DAY
C
C   SUBPRGRAMS CALLED:
C     NAMES   LIBRARY
C     ------------------------------------------------------- --------
C     CHAR MOVA2I                                              SYSTEM
C
C REMARKS:    WHEN NN.NE.0, THE INFORMATION IN IDATE MUST BE
C     FORMATTED AS DIAGRAMMED IN APPENDIX C OF NMC O.N. 84.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CYAY Y-MP8/832
C
C$$$
C
      CHARACTER*1 IDATE(4)
C
      IF (NN.NE.0) THEN
C
        IYEAR = MOVA2I(IDATE(1))
        MONTH = MOVA2I(IDATE(2))
        IDAY  = MOVA2I(IDATE(3))
        IHOUR = MOVA2I(IDATE(4))
C
      ELSE
C
        IDATE(1) = CHAR(IYEAR)
        IDATE(2) = CHAR(MONTH)
        IDATE(3) = CHAR(IDAY)
        IDATE(4) = CHAR(IHOUR)
      ENDIF
C
      RETURN
      END
       SUBROUTINE W3FS11(IDATE,IYEAR,MONTH,IDAY,IHOUR,NN)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM: W3FS11         NMC DATE WORD UNPACKER AND PACKER
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 87-03-24
C
C ABSTRACT: OBTAINS THE COMPONENTS OF THE NMC DATE WORD (NMC OFFICE
C   NOTES 84 AND 85), OR GIVEN ITS COMPONENTS, FORMS AN NMC TYPE DATE
C   WORD. W3FS11 IS THE SAME AS W3FS03 EXCEPT FOR THE ORDER OF THE
C   PARAMETERS.
C
C PROGRAM HISTORY LOG:
C   87-03-24  R.E.JONES   CONVERT TO CYBER 205 FORTRAN 200
C   89-10-13  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C
C USAGE:  CALL W3FS11 (IDATE, IYEAR, MONTH, IDAY, IHOUR, NN)
C
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IDATE  ARG LIST  LEFT 4 BYTES OF INTEGER 64 BIT WORD, OR CAN BE
C                      CHARACTER*1 IDATE(4) OR CHARACTER*4 IDATE.
C                      IF OFFICE NOTE 85 LABEL USED AS 4 64 BIT WORDS,
C                      IDATE IS IN THE LEFT 32 BITS OF THE 2ND WORD.
C                      IF OFFICE NOTE 84 12 IDS. (6 64 BIT WORDS ON
C                      CRAY, DATE WORD IN LEFT 32 BITS OF 4TH CRAY ID
C                      WORD, OR 7TH 32 BIT ID WORD ON NAS.
C     IYEAR  ARG LIST  INTEGER   YEAR (2 DIGITS)
C     MONTH  ARG LIST  INTEGER   MONTH
C     IDAY   ARG LIST  INTEGER   DAY
C     IHOUR  ARG LIST  INTEGER   HOUR
C     NN     ARG LIST  INTEGER   CODE:
C                     .EQ. 0 PACK IYEAR, MONTH, IDAY, IHOUR INTO IDATE
C                     .NE. 0 UNPACK IDATE INTO IYEAR, MONTH, IDAY, IHOUR
C
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     IDATE  ARG LIST  LEFT 4 BYTES OF INTEGER 64 BIT WORD, OR CAN BE
C                      CHARACTER*1 IDATE(4) OR CHARACTER*4 IDATE.
C                      IF OFFICE NOTE 85 LABEL USED AS 4 64 BIT WORDS,
C                      IDATE IS IN THE LEFT 32 BITS OF THE 2ND WORD.
C                      IF OFFICE NOTE 84 12 IDS. (6 64 BIT WORDS ON
C                      CRAY, DATE WORD IN LEFT 32 BITS OF 4TH CRAY ID
C                      WORD, OR 7TH 32 BIT ID WORD ON NAS.
C     IYEAR  ARG LIST  INTEGER   YEAR (2 DIGITS)
C     MONTH  ARG LIST  INTEGER   MONTH
C     IDAY   ARG LIST  INTEGER   DAY
C     IHOUR  ARG LIST  INTEGER   HOUR
C
C   SUBROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     CHAR   MOVA2I                                            SYSTEM
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C
C$$$
C
      CHARACTER IDATE(4)
C
      IF (NN.NE.0) THEN
C
        IYEAR = MOVA2I(IDATE(1))
        MONTH = MOVA2I(IDATE(2))
        IDAY  = MOVA2I(IDATE(3))
        IHOUR = MOVA2I(IDATE(4))
C
      ELSE
C
        IDATE(1) = CHAR(IYEAR)
        IDATE(2) = CHAR(MONTH)
        IDATE(3) = CHAR(IDAY)
        IDATE(4) = CHAR(IHOUR)
      ENDIF
C
      RETURN
      END
      SUBROUTINE W3FS15(IDATE,JTAU,NDATE)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:   W3FS15       UPDATING OFFICE NOTE 85 DATE/TIME WORD
C   PRGMMR: REJONES          ORG: NMC421     DATE: 89-08-23
C
C ABSTRACT: UPDATES OR BACKDATES A FULLWORD DATE/TIME WORD (O.N. 84)
C   BY A SPECIFIED NUMBER OF HOURS.
C
C PROGRAM HISTORY LOG:
C   ??-??-??  R.ALLARD
C   87-02-19  R.E.JONES  CLEAN UP CODE
C   87-02-19  R.E.JONES  CHANGE TO MICROSOFT FORTRAN 4.10
C   89-05-12  R.E.JONES  CORRECT ORDER OF BYTES IN DATE WORD FOR PC
C   89-08-04  R.E.JONES  CLEAN UP CODE, GET RID OF ASSIGN, CORRECTION
C                        FOR MEMORY SET TO INDEFINITE.
C   89-10-25  R.E.JONES  CHANGE TO CRAY CFT77 FORTRAN
C   95-11-15  R.E.JONES  ADD SAVE STATEMENT
C
C USAGE:    CALL W3FS15 (IDATE, JTAU, NDATE)
C   INPUT ARGUMENT LIST:
C     IDATE    - PACKED BINARY DATE/TIME AS FOLLOWS:
C                BYTE 1  IS YEAR OF CENTURY  00-99
C                BYTE 2  IS MONTH            01-12
C                BYTE 3  IS DAY OF MONTH     01-31
C                BYTE 4  IS HOUR             00-23
C                SUBROUTINE TAKES ADVANTAGE OF FORTRAN ADDRESS
C                PASSING, IDATE AND NDATE MAY BE
C                A CHARACTER*1 ARRAY OF FOUR, THE LEFT 32
C                BITS OF 64 BIT INTEGER WORD. AN OFFICE NOTE 85
C                LABEL CAN BE STORED IN
C                4 INTEGER WORDS.
C                IF INTEGER THE 2ND WORD IS USED. OUTPUT
C                IS STORED IN LEFT 32 BITS. FOR A OFFICE NOTE 84
C                LABEL THE 7TH WORD IS IN THE 4TH CRAY 64 BIT
C                INTEGER, THE LEFT 32 BITS.
C     JTAU     - INTEGER  NUMBER OF HOURS TO UPDATE (IF POSITIVE)
C                OR BACKDATE (IF NEGATIVE)
C
C   OUTPUT ARGUMENT LIST:
C     NDATE    - NEW DATE/TIME WORD RETURNED IN THE
C                SAME FORMAT AS 'IDATE'. 'NDATE' AND 'IDATE' MAY
C                BE THE SAME VARIABLE.
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3LIB    - NONE
C
C   RESTRICTIONS: THIS ROUTINE IS VALID ONLY FOR THE 20TH CENTURY.
C
C   NOTES: THE FORMAT OF THE DATE/TIME WORD IS THE SAME AS THE
C     SEVENTH WORD OF THE PACKED DATA FIELD LABEL (SEE O.N. 84) AND
C     THE THIRD WORD OF A BINARY DATA SET LABEL (SEE O.N. 85).
C
C   EXIT STATES:
C     AN ERROR FOUND BY OUT OF RANGE TESTS ON THE GIVEN DATE/TIME
C     INFORMATION WILL BE INDICATED BY RETURNING A BINARY ZERO WORD
C     IN 'NDATE'.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C
C$$$
C
      INTEGER     ITABYR(13)
      INTEGER     LPTB(13)
      INTEGER     NOLPTB(13)
C
      CHARACTER*1 IDATE(4)
      CHARACTER*1 NDATE(4)
C
      SAVE
C
      DATA  LPTB  /0000,0744,1440,2184,2904,3648,4368,5112,
     &             5856,6576,7320,8040,8784/
      DATA  NOLPTB/0000,0744,1416,2160,2880,3624,4344,5088,
     &             5832,6552,7296,8016,8760/
      DATA  ICENTY/1900/
C
C     ...WHERE ICENTY IS FOR THE 20TH CENTURY ASSUMED FOR THE GIVEN
C     ...                 YEAR WITHIN THE CENTURY
C
      IYR    = MOVA2I(IDATE(1))
      IMONTH = MOVA2I(IDATE(2))
      IDAY   = MOVA2I(IDATE(3))
      IHOUR  = MOVA2I(IDATE(4))
C
      IF (IYR    .GT. 99) GO TO 1600
      IF (IMONTH .LE.  0) GO TO 1600
      IF (IMONTH .GT. 12) GO TO 1600
      IF (IDAY   .LE.  0) GO TO 1600
      IF (IDAY   .GT. 31) GO TO 1600
      IF (IHOUR  .LT.  0) GO TO 1600
      IF (IHOUR  .GT. 24) GO TO 1600
      IF (JTAU   .NE.  0) GO TO 100
C
        NDATE(1) = IDATE(1)
        NDATE(2) = IDATE(2)
        NDATE(3) = IDATE(3)
        NDATE(4) = IDATE(4)
        RETURN
C
  100 CONTINUE
        JAHR  = IYR + ICENTY
        KABUL = 1
        GO TO 900
C
C     ...WHERE 900 IS SUBROUTINE TO INITIALIZE ITABYR
C     ...AND RETURN THRU KABUL
C
  200 CONTINUE
        IHRYR  = IHOUR + 24 * (IDAY - 1) + ITABYR(IMONTH)
        IHRYR2 = IHRYR + JTAU
C
C     ...TO TEST FOR BACKDATED INTO PREVIOUS YEAR...
C
  300 CONTINUE
        IF (IHRYR2 .LT. 0) GO TO 700
C
      DO  400  M = 2,13
        IF (IHRYR2 .LT. ITABYR(M)) GO TO 600
  400 CONTINUE
C
C     ...IF IT FALLS THRU LOOP TO HERE, IT IS INTO NEXT YEAR...
C
        JAHR   = JAHR   + 1
        IHRYR2 = IHRYR2 - ITABYR(13)
        KABUL  = 2
        GO TO 900
C
  600 CONTINUE
        MONAT  = M      - 1
        IHRMO  = IHRYR2 - ITABYR(MONAT)
        NODAYS = IHRMO  / 24
        ITAG   = NODAYS + 1
        IUHR   = IHRMO  - NODAYS * 24
        GO TO 1500
C
C     ...ALL FINISHED.  RETURN TO CALLING PROGRAM.......................
C     ...COMES TO 700 IF NEG TOTAL HRS. BACK UP INTO PREVIOUS YEAR
C
  700 CONTINUE
        JAHR  = JAHR - 1
        KABUL = 3
        GO TO 900
C
C     ...WHICH IS CALL TO INITIALIZE ITABYR AND RETURN THRU KABUL
C
  800 CONTINUE
        IHRYR2 = ITABYR(13) + IHRYR2
        GO TO 300
C
C     ...SUBROUTINE INITYR...
C     ...CALLED BY GO TO 900 AFTER ASSIGNING RETURN NO. TO KABUL...
C     ...ITABYR HAS MONTHLY ACCUMULATING TOTAL HRS REL TO BEGIN OF YR.
C     ...DEPENDS ON WHETHER JAHR IS LEAP YEAR OR NOT.
C
  900 CONTINUE
        IQUOT  = JAHR / 4
        IRMNDR = JAHR - 4 * IQUOT
        IF (IRMNDR .NE. 0) GO TO 1000
C
C     ...WAS MODULO 4, SO MOST LIKELY A LEAP YEAR,
C
        IQUOT  = JAHR / 100
        IRMNDR = JAHR - 100 * IQUOT
        IF (IRMNDR .NE. 0) GO TO 1200
C
C     ...COMES THIS WAY IF A CENTURY YEAR...
C
        IQUOT  = JAHR / 400
        IRMNDR = JAHR - 400 * IQUOT
        IF (IRMNDR .EQ. 0) GO TO 1200
C
C     ...COMES TO 1000 IF NOT A LEAP YEAR...
C
 1000 CONTINUE
      DO  1100  I = 1,13
        ITABYR(I) = NOLPTB(I)
 1100 CONTINUE
      GO TO 1400
C
C     ...COMES TO 1200 IF LEAP YEAR
C
 1200 CONTINUE
      DO  1300  I = 1,13
        ITABYR(I) = LPTB(I)
 1300 CONTINUE
C
 1400 CONTINUE
        GO TO (200,300,800) KABUL
C
 1500 CONTINUE
        JAHR     = MOD(JAHR,100)
        NDATE(1) = CHAR(JAHR)
        NDATE(2) = CHAR(MONAT)
        NDATE(3) = CHAR(ITAG)
        NDATE(4) = CHAR(IUHR)
        RETURN
C
 1600 CONTINUE
        NDATE(1) = CHAR(0)
        NDATE(2) = CHAR(0)
        NDATE(3) = CHAR(0)
        NDATE(4) = CHAR(0)
C
C     ...WHICH FLAGS AN ERROR CONDITION ...
C
      RETURN
      END
 
      SUBROUTINE SABORT(STRING)
      CHARACTER*(*) STRING
      WRITE(*,*) 'ABORT:',STRING
      STOP 8
      END
