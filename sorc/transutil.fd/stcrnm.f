	SUBROUTINE  ST_CRNM  ( string, value, iret )
C************************************************************************
C* ST_CRNM								*
C*									*
C* This subroutine converts a character string to a real number.  If	*
C* the conversion fails, RMISSD is returned.				*
C*									*
C* ST_CRNM  ( STRING, VALUE, IRET ) 					*
C*									*
C* Input parameters: 							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	VALUE		REAL		Real number			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = conversion error 		*
C**									*
C* Log:									*
C* I. Graffman/CSC	12/82	STR_CHRL				*
C* M. desJardins/GSFC	 4/84	Modified to use READ to get number	*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* J. Whistler/SSAI	 5/91	Modified to use internal string		*
C* J. Whistler/SSAI	 6/91	Modified internal read format using BN	*
C* K. Brill/NMC		 9/91	Check for number on IBM			*
C* M. desJardins/NMC	 3/92	Recoded to eliminate exceptions		*
C* S. Jacobs/NMC	 6/94	Increased sss*12 to sss*24		*
C* K. Tyle/GSC		12/95	Added check for single character 'E'	*
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
	CHARACTER	sss*24, ttt*4, c*1
	LOGICAL		good, plus, before
C------------------------------------------------------------------------
	iret = -2
	value = RMISSD
C
C*	Remove blanks from string.
C
	CALL ST_RMBL  ( string, sss, lens, ier )
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
C*	Now loop through all characters and turn into integers corresponding
C*	to values before and after decimal point and for exponent.
C
	ival0  = ICHAR ( '0' )
	ibefor = 0
	iafter = 0
	nafter = 0
	iexp   = 0
	good   = .true.
	i      = ibeg
	before = .true.
	DO  WHILE  ( good .and. ( i .le. lens ) )
	    c     = sss (i:i)
	    IF  ( ( c .ge. '0' ) .and. ( c .le. '9' ) )  THEN
		ivalc = ICHAR (c) - ival0
		IF  ( before )  THEN
		    ibefor = ibefor * 10 + ivalc
		  ELSE
		    iafter = iafter * 10 + ivalc
		    nafter = nafter + 1
		END IF
	      ELSE IF  ( c .eq. '.' )  THEN
		IF  ( before )  THEN
		    before = .false.
		  ELSE
		    good   = .false.
		END IF
	      ELSE IF  ( ( ( c .eq. 'E' ) .or. ( c .eq. 'e' ) )
     +                 .and. lens .ne. 1 )  THEN
		IF  ( i .lt. lens )  THEN
		    ttt = sss ( i+1 : )
		    CALL ST_NUMB  ( ttt, iexp, ier )
		    IF  ( ier .ne. 0 )  good = .false.
		END IF
		i = lens
	      ELSE
		good = .false.
	    END IF
	    i = i + 1
	END DO
C
C*	Compute the value using the three parts.
C
	IF  ( good )  THEN
	    iret = 0
	    value = FLOAT ( ibefor )
	    IF  ( nafter .gt. 0 )  THEN
		value = value + ( FLOAT ( iafter ) / 10. ** nafter )
	    END IF
	    IF  ( iexp .ne. 0 )  THEN
		value = value * 10. ** iexp
	    END IF
	    IF  ( .not. plus )  value = -value
	END IF
C*
	RETURN
	END
