	SUBROUTINE ST_UNPR  ( string, lenin, outstr, lenout, iret )
C************************************************************************
C* ST_UNPR								*
C*									*
C* This subroutine eliminates substrings of unprintable characters.	*
C* Substrings of control characters, i.e., characters less than a 	*
C* blank, are replaced by a single blank.  Characters greater than	*
C* '}' (CHAR (126)) are replaced by '~' (CHAR (127)).  This subroutine	*
C* can be used to replace control characters such as CR and LF with	*
C* a single blank.  Invalid characters in the ASCII character set	*
C* are replaced by '~' so that the lengths of fields in the record	*
C* will remain unchanged.  The input and output strings may be the	*
C* same variable.							*
C*									*
C* ST_UNPR  ( STRING, LENIN, OUTSTR, LENOUT, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Input string			*
C*	LENIN		INTEGER		Length of input string		*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		Output string			*
C*	LENOUT		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* J. Whistler/SSAI	 5/91	Put tilda definition in GEMPRM.PRM	*
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
	CHARACTER*(*)	string, outstr
C*
	LOGICAL		bl
	CHARACTER	c*1
C------------------------------------------------------------------------
	iret = 0
	bl   = .true.
	ip   = 0
C
C*	Check each character to see if it is a control character,
C*	a blank or out of range.
C
	DO  i = 1, lenin
	    c = string (i:i)
	    IF  ( ( c .ge. '!' ) .and. ( c .le. '}' ) )  THEN
C
C*		Add non blanks to the string.
C
		ip = ip + 1
		outstr ( ip: ip ) = c
		bl = .false.
	      ELSE IF  ( c .gt. '}' )  THEN
C
C*		Replace out of range characters with a tilda.
C
		ip = ip + 1
		outstr ( ip : ip ) = CHTLDA 
		bl = .false.

C
C*		Add a blank to the list only if the last character is not
C*		already a blank.
C
	      ELSE IF ( .not. bl ) THEN
		bl = .true.
		ip = ip + 1
		outstr ( ip : ip ) = ' '
	    END IF
	END DO
C
C*	Blank out remainder of string.
C
	IF ( ip .lt. lenin ) outstr ( ip+1 : lenin ) = ' '
C
C*	Set output length.  If last character was a blank, eliminate it.
C
	IF  ( ( ip .gt. 0 ) .and. ( outstr (ip:ip) .eq. ' ' ) ) THEN
	    lenout = ip - 1
	  ELSE
	    lenout = ip
	END IF
C*
	RETURN
	END
