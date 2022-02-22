	FUNCTION PR_WIND  ( drct, sped ) 
C************************************************************************
C* PR_WIND								*
C*									*
C* This function computes WIND from DRCT and SPED.  WIND is in the	*
C* form SSSDDD, where SSS is the speed and DDD is the direction.	*
C* SPED may be entered in meters/sec or knots; DRCT is in degrees	*
C* The following equation is used:					*
C*									*
C*             WIND = NINT ( SPED ) * 1000 + NINT ( DRCT )		*
C*									*
C* PR_WIND  ( DRCT, SPED )						*
C*									*
C* Input parameters:							*
C*	DRCT		REAL		Wind direction in degrees	*
C*	SPED		REAL		Wind speed 			*
C*									*
C* Output parameters:							*
C*	PR_WIND		REAL		Packed speed and direction	*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91						*
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
C
       ERMISS (xxxx) = ( ABS ( xxxx - RMISSD ) .lt. RDIFFD )
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ( .not. ERMISS ( drct ) ) .and. 
     +	      ( .not. ERMISS ( sped ) ) )  THEN
	    jdrct   = NINT ( drct )
	    jsped   = NINT ( sped )
	    PR_WIND = jdrct + jsped * 1000
	  ELSE
	    PR_WIND = RMISSD
	END IF
C*
	RETURN
	END
