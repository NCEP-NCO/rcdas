	FUNCTION PR_HGMF  ( hght )
C************************************************************************
C* PR_HGMF								*
C*									*
C* This function computes HGFT from HGHT.  The following equation is	*
C* used:  								*
C*									*
C*                HGFT  =  NINT ( HGHT * 3.28084 )			*
C*									*
C* (Note:  This function rounds to the nearest foot.)			*
C*									*
C* PR_HGMF  ( HGHT )							*
C*									*
C* Input parameters:							*
C*	HGHT		REAL		Height in meters		*
C*									*
C* Output parameters:							*
C*	PR_HGMF		REAL		Height in feet  		*
C**									*
C* Log:									*
C* G. Huffman/GSC	7/88	Build from PR_HTMT			*
C* S. Schotz/GSC	10/89   modify to round to nearest foot		*
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
C*	Check for invalid data.
C
	IF  ( ERMISS ( hght ) )  THEN
	    PR_HGMF = RMISSD
	  ELSE
C
C*	    Do the conversion.
C
	    PR_HGMF = NINT(hght * 3.28084)
	END IF
C*
	RETURN
	END
