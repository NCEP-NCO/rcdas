C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE bfrini
                                                                        
      COMMON /bitbuf/ maxbyt, ibit, ibay(3000), mbyt(32), 
     +            mbay(3000,32)
      COMMON /padesc/ ibct, ipd1, ipd2, ipd3, ipd4
      COMMON /reptab/ idnr(5,2), typs(5,2), reps(5,2), lens(5)
      COMMON /stbfr/ iolun(32), iomsg(32)
      COMMON /tababd/ ntba(0:32), ntbb(0:32), ntbd(0:32), mtab(50,32), 
     +            idna(50,32,2), idnb(250,32), idnd(250,32), 
     +            taba(50,32), tabb(250,32), tabd(250,32)
      COMMON /dxtab/ maxdx, idxv, nxstr(10), ldxa(10), ldxb(10), 
     +            ldxd(10), ld30(10), dxstr(10)
      COMMON /tables/ maxtab, ntab, tag(20000), typ(20000), knt(20000),
     +            jump(20000), link(20000), jmpb(20000), ibt(20000), 
     +            irf(20000), isc(20000), itp(20000), vali(20000), 
     +            knti(20000), iseq(20000,2), jseq(20000)
      COMMON /bufrmg/ msglen, msgtxt(3000)
      COMMON /mrgcom/ nrpl, nmrg, namb, ntot
      COMMON /quiet/ iprt
                                                                        
                                                                        
      CHARACTER*600 tabd
      CHARACTER*128 tabb
      CHARACTER*128 taba
      CHARACTER*56 dxstr
      CHARACTER*10 tag
      CHARACTER*6 adsn(5,2), dndx(25,10)
      CHARACTER*3 typx(5,2), typs, typ
      CHARACTER*1 repx(5,2), reps
      DIMENSION ndndx(10), nldxa(10), nldxb(10), nldxd(10), nld30(10)
      DIMENSION lenx(5)
                                                                        
      DATA adsn /'101000', '360001', '360002', '360003', '360004', 
     +            '101255', '031002', '031001', '031001', '031000'/
      DATA typx /'REP', 'DRP', 'DRP', 'DRS', 'DRB', 'SEQ', 'RPC', 'RPC',
     +            'RPS', 'SEQ'/
      DATA repx /'"', '(', '{', '[', '<', '"', ')', '}', ']', '>'/
      DATA lenx /0, 16, 8, 8, 1/
                                                                        
      DATA (dndx(i,1),i = 1,25) /'102000', '031001', '000001', '000002',
     +            '110000', '031001', '000010', '000011', '000012', 
     +            '000013', '000015', '000016', '000017', '000018', 
     +            '000019', '000020', '107000', '031001', '000010', 
     +            '000011', '000012', '000013', '101000', '031001', 
     +            '000030'/
                                                                        
      DATA (dndx(i,2),i = 1,15) /'103000', '031001', '000001', '000002',
     +            '000003', '101000', '031001', '300004', '105000', 
     +            '031001', '300003', '205064', '101000', '031001', 
     +            '000030'/
                                                                        
      DATA ndndx /25, 15, 8 * 0/
      DATA nldxa /35, 67, 8 * 0/
      DATA nldxb /80, 112, 8 * 0/
      DATA nldxd /38, 70, 8 * 0/
      DATA nld30 /5, 6, 8 * 0/
                                                                        
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
                                                                        
C  INITIALIZE /BITBUF/                                                  
C  -------------------                                                  
                                                                        
      maxbyt = 19970
                                                                        
C     INITIALIZE /PADESC/                                                  
C     -------------------                                                  
                                                                        
      ibct = ifxy('063000')
      ipd1 = ifxy('102000')
      ipd2 = ifxy('031001')
      ipd3 = ifxy('206001')
      ipd4 = ifxy('063255')
                                                                        
C     INITIALIZE /STBFR/                                                   
C     ------------------                                                   
                                                                        
      DO i = 1, 10
        iolun(i) = 0
        iomsg(i) = 0
      END DO
                                                                        
C     INITIALIZE /REPTAB/                                                  
C     -------------------                                                  
                                                                        
      DO i = 1, 5
        lens(i) = lenx(i)
        DO j = 1, 2
          idnr(i,j) = ifxy(adsn(i,j))
          typs(i,j) = typx(i,j)
          reps(i,j) = repx(i,j)
        END DO
      END DO
                                                                        
C     INITIALIZE /TABABD/                                                  
C     -------------------                                                  
                                                                        
      ntba(0) = 50
      ntbb(0) = 250
      ntbd(0) = 250
                                                                        
C     INITIALIZE /DXTAB/                                                   
C     ------------------                                                   
                                                                        
      maxdx = maxbyt
      idxv = 1
                                                                        
      DO j = 1, 10
        ldxa(j) = nldxa(j)
        ldxb(j) = nldxb(j)
        ldxd(j) = nldxd(j)
        ld30(j) = nld30(j)
        dxstr(j) = '      '
        nxstr(j) = ndndx(j) * 2
        DO i = 1, ndndx(j)
          i1 = i * 2 - 1
          CALL ipkm(dxstr(j)(i1:i1),2,ifxy(dndx(i,j)))
        END DO
      END DO
                                                                        
C     INITIALIZE /TABLES/                                                  
C     -------------------                                                  
                                                                        
      maxtab = 20000
                                                                        
C     INITIALIZE /BUFRMG/                                                  
C     -------------------                                                  
                                                                        
      msglen = 0
                                                                        
C     INITIALIZE /MRGCOM/                                                  
C     -------------------                                                  
                                                                        
      nrpl = 0
      nmrg = 0
      namb = 0
      ntot = 0
                                                                        
C     INITIALIZE /QUIET/                                                   
C     ------------------                                                   
                                                                        
      iprt = 0
                                                                        
      RETURN
      END
