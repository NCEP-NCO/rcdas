	SUBROUTINE ST_RNAN  ( string, outstr, length, iret )
C************************************************************************
C* ST_RNAN                                     				*
C* 									*
C* This subroutine replaces non-alphanumeric characters with spaces 	*
C* and removes the extra spaces from a character string.  The		*
C* characters period (.), plus sign (+), minus sign (-) and asterisk	*
C* (*) are not removed.							*
C* 									*
C* ST_RNAN  ( STRING, OUTSTR, LENGTH, IRET )				*
C*									*
C* Input parameters:                                                   	*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:                                                  	*
C*	OUTSTR		CHAR*		Converted string          	*
C*	LENGTH		INTEGER		Length of output string		*
C*	IRET		INTEGER 	Return code  			*
C*				   	 0 = normal return 		*
C** 									*
C* I. Graffman/RDS	 2/84						*
C* M. desJardins/GSFC	 4/84	remove plus signs (+)			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Sager/NCEP         2/96   Increased size of strbuf                *
C* D. Kidwell/NCEP      10/96   Ported to Cray                          *
C************************************************************************
C* GEMPRM.PRM
C*
C* This include file contains parameter definitions for the GEMPAK
C* software routines in the ST_ and PR_ libraries.
C*
C* CRAY version
C**
C* Log:  
C*	Kidwell/NCEP	07/96	Adapted a subset of gemprm.prm for Cray
C************************************************************************
C!
C!	Missing data definitions
C!
 	PARAMETER	( RMISSD = -9999.0 )
C!						Missing data value
	PARAMETER	( RDIFFD =  0.1    )
C!						Missing value fuzziness
	PARAMETER	( IMISSD = -9999   )
C!						Missing integer value
	LOGICAL		  ERMISS
C!						Declare for stmt func
C!
C! 	Physical and mathematical constants
C!
	PARAMETER       ( PI = 3.14159265  )
C!                                              PI
	PARAMETER       ( DTR = PI / 180.  )
	PARAMETER       ( RTD = 180. / PI  )
C!                                              Degrees <--> Radians
	PARAMETER	( GRAVTY = 9.80616  )
C!						Acceleration of gravity
	PARAMETER	( RDGAS  = 287.04   )
	PARAMETER	( RKAP   = RDGAS / GRAVTY )
C!						Gas constant of dry air
	PARAMETER	( RKAPPA = 2. / 7. )
C!						Poisson constant
	PARAMETER	( GAMUSD = 6.5 )
C!						US std atmos lapse rate
	PARAMETER	( TMCK   = 273.15 )
C!						Centigrade -> Kelvin
C!
C!	ASCII character constants 
C!
C!	Since the Cray does not allow the use of a function (e.g.,
C!	CHAR) to define a parameter, nor does it allow a character
C!	to be defined directly as a hex (X) value, the convolutions
C!      below are necessary to define the character values for Cray.
C!
C
	CHARACTER * 1 chnull, chtab, chspac, chtlda
C
	CHARACTER * 8 c8null, c8tab, c8spac, c8tlda
C
	INTEGER       iigemc ( 4 )
C
	EQUIVALENCE ( chnull, c8null (8:8) ), ( chtab , c8tab  (8:8) ),
     +              ( chspac, c8spac (1:1) ), ( chtlda, c8tlda (8:8) ) 
C
	EQUIVALENCE ( iigemc ( 1), c8null ), ( iigemc ( 2), c8tab  ),
     +              ( iigemc ( 3), c8spac ), ( iigemc ( 4), c8tlda )
C
	DATA iigemc / Z'00',    Z'09',    Z'20',    Z'7E' /
C                     Null      Tab       Space     Tilda
C!
C*
	CHARACTER*(*)	string, outstr
C*
	CHARACTER	strbuf*160
C------------------------------------------------------------------------
	length = 0
	iret   = 0
	strbuf = string
	outstr = ' '
C
C*	Remove leading spaces and tabs.
C
	CALL ST_LDSP  ( strbuf, strbuf, isiz, iret )
	IF  ( isiz .le. 0 )  RETURN
C
C*	Check each character in string.
C
	DO  i = 1, isiz
C
C*	    Find non alphanumerics.
C
	    CALL ST_ALNM  ( strbuf (i:i), itype, iret )
	    IF  ( itype .eq. 0 )  THEN
C
C*	        Check for period, minus sign, plus sign or asterisk.
C*
	        IF  ( ( strbuf (i:i) .ne. '.' ) .and.
     +		      ( strbuf (i:i) .ne. '-' ) .and.
     +		      ( strbuf (i:i) .ne. '+' ) .and.
     +		      ( strbuf (i:i) .ne. '*' ) )   THEN
		    strbuf (i:i) = CHSPAC
		END IF
	    END IF
	END DO
C
C*	Remove extra spaces.
C
	ispac = 0
	DO  j = 1, isiz
	    IF  ( strbuf (j:j) .ne. CHSPAC )  THEN
	        length = length+1
	        strbuf (length:length) = strbuf (j:j)
	        ispac  = 0
	      ELSE
	        IF  ( ispac .eq. 0 )  THEN
	            length = length + 1
	            strbuf (length:length) = strbuf (j:j)
	            ispac  = 1
	        END IF
	    END IF
	END DO
C
C*	Transfer to output string.
C
	IF  ( length .gt. 0 )  outstr = strbuf (1:length)
C*
	RETURN
	END
