	SUBROUTINE ST_INTG  ( string, intg, iret )
C************************************************************************
C* ST_INTG								*
C*									*
C* This subroutine decodes a character string into an integer.  If 	*
C* the string cannot be decoded, INTG is set to IMISSD.  Note that	*
C* only the substring containing the digits to be decoded should be	*
C* sent to this subroutine, rather than a string with trailing blanks.	*
C*									*
C* ST_INTG  ( STRING, INTG, IRET )					*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Input string			*
C*									*
C* Output parameters:							*
C*	INTG		INTEGER		Decoded integer			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = conversion error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/86						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* J. Whistler/SSAI	 7/91	Modified internal read			*
C* K. Brill/NMC		 9/91	Added call to ST_CKNI			*
C* M. desJardins/NMC	 3/92	Recode to eliminate special cases	*
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
	CHARACTER*(*)	string
C*
	CHARACTER	sss*12, c*1
	LOGICAL		good, plus
C------------------------------------------------------------------------
	iret = -2
	intg = IMISSD
C
C*	Assume that string does not have embedded or trailing blanks.
C
	lens = LEN ( string )
	IF  ( lens .gt. 12 ) lens = 12
	sss  = string
C
C*	Check for empty string.
C
	IF  ( lens .eq. 0 )  RETURN
C
C*	Check for + or - in first character.
C
	IF  ( sss (1:1) .eq. '+' )  THEN
	    ibeg = 2
	    plus = .true.
	  ELSE IF  ( sss (1:1) .eq. '-' )  THEN
	    ibeg = 2
	    plus = .false.
	  ELSE
	    ibeg = 1
	    plus = .true.
	END IF
	IF  ( ibeg .gt. lens )  RETURN
C
C*	Now loop through all characters and turn into integer.
C
	ival0 = ICHAR ( '0' )
	intg  = 0
	good  = .true.
	i     = ibeg
	DO  WHILE  ( good .and. ( i .le. lens ) )
	    c     = sss (i:i)
	    ivalc = ICHAR ( c ) - ival0
	    IF  ( ( ivalc .ge. 0 ) .and. ( ivalc .le. 9 ) )  THEN
		intg  = intg * 10 + ivalc
		i     = i + 1
	      ELSE
		good  = .false.
	    END IF
	END DO
C
C*	Check for good value and add sign.
C
	IF  ( .not. good )  THEN
	    intg = IMISSD
	  ELSE IF  ( plus )  THEN
	    iret = 0
	  ELSE
	    iret = 0
	    intg = -intg
	END IF
C*
	RETURN
	END
