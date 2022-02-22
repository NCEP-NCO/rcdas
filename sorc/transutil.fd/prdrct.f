	FUNCTION PR_DRCT  ( ux, vx )
C************************************************************************
C* PR_DRCT								*
C*									*
C* This function computes DRCT from Ux and Vx, both of which must	*
C* be either meters/sec or knots.  The following equation is used:	*
C*									*
C*              DRCT = ATAN2 ( -UX, -VX ) * RTD				*
C*									*
C* PR_DRCT  ( UX, VX )							*
C*									*
C* Input parameters:							*
C*	UX		REAL		U component of velocity		*
C*	VX		REAL    	V component of velocity		*
C*									*
C* Output parameters:							*
C*	PR_DRCT		REAL		Wind direction in degrees	*
C**									*
C* Log:									*
C* I. Graffman/CSC	8/83	Original source code			*
C* M. Goodman/RDS  	8/84	Renamed PR_DRCT 			*
C* M. desJardins/GSFC	1/86	Corrected uwnd=0 problem		*
C*				Added parameter statements		*
C* I. Graffman/RDS	12/87	GEMPAK4 documentation			*
C* G. Huffman/GSC       7/88    Documentation; U, V names; north=360	*
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
C*      Find bad data.
C
	IF  ( ERMISS (ux) .or. ERMISS (vx) )  THEN
	    PR_DRCT = RMISSD 
	  ELSE IF  ( ( vx .EQ. 0. ) .and. ( ux .eq. 0. ) )  THEN
	    PR_DRCT = 0.
	  ELSE
	    PR_DRCT = ATAN2 ( -ux, -vx ) * RTD
	    IF  ( PR_DRCT .le. 0. )  PR_DRCT = PR_DRCT + 360.
	END IF
C*
	RETURN
	END
