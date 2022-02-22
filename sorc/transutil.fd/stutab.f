	SUBROUTINE ST_UTAB  ( string, nchar, outstr, iret )
C************************************************************************
C* ST_UTAB								*
C*									*
C* This subroutine substitutes spaces for tabs in a string.  Spaces	*
C* are added for each tab found so that the character after the tab	*
C* appears at the next tab stop.  Tab stops are assumed to be at 	*
C* positions 9, 17, 25, ....  The input and output strings may		*
C* be the same variable.						*
C*									*
C* ST_UTAB  ( STRING, NCHAR, OUTSTR, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Input string			*
C*	NCHAR		INTEGER		Number of characters		*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		Output string			*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	11/89	Fix so STRING & OUTSTR can be the same	*
C* M. desJardins/GSFC	 3/90	Initialize output to blank		*
C* L. Sager/NCEP         2/96   Increased size of strtmp                *
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
	CHARACTER	strtmp*160
C*
	INTEGER		tabs (129)
	DATA		tabs /8*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 
     +	                      7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 
     +	                      7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1, 7*0, 1,
     +	                      7*0, 1/
C------------------------------------------------------------------------
	iret = 0
	ilen = 1
C
C*	Loop through each character checking for tabs.
C
	i = 1
	DO WHILE  ( ( i .le. nchar ) .and. ( ilen .le. 160 ) )
	    IF  ( string (i:i) .ne. CHTAB )  THEN
		strtmp (ilen:ilen) = string (i:i)
		ilen = ilen + 1
	      ELSE
		strtmp (ilen:ilen) = CHSPAC
	        ilen = ilen + 1
	        DO WHILE  ( tabs (ilen) .eq. 0 )
		    strtmp (ilen:ilen) = CHSPAC
		    ilen = ilen + 1
		END DO
	    END IF
	    i = i + 1
	END DO
C
C*	Transfer to output string.
C
	IF  ( ilen .lt. 160 )  strtmp (ilen : ) = ' '
	outstr = strtmp
C*
	RETURN
	END
