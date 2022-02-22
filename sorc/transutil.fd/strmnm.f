	SUBROUTINE ST_RMNM  ( string, outstr, nncr, lens, iret )
C************************************************************************
C* ST_RMNM                                     				*
C* 									*
C* This subroutine removes numeric characters from a string.		*
C* 									*
C* ST_RMNM  ( STRING, OUTSTR, NNCR, LENS, IRET )			*
C*									*
C* Input parameters:                                                   	*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:                                                  	*
C*	OUTSTR		CHAR*		Converted string          	*
C*	NNCR		INTEGER		Number of numeric CHARs removed	*
C*	LENS		INTEGER		Length of output string		*
C*	IRET		INTEGER 	Return code  			*
C*				   	 0 = normal return 		*
C** 									*
C* K. Brill/NMC		08/91						*
C* D. Kidwell/NCEP      10/96   Ported to Cray                          *
C************************************************************************
	CHARACTER*(*)	string, outstr
C*
C------------------------------------------------------------------------
	iret   = 0
	outstr = ' '
	lens = 0
C
C*	Find length of input character string.
C
	CALL ST_LSTR ( string, isiz, iret )
	IF  ( isiz .le. 0 )  RETURN
C
C*	Check each character in string.
C
	io = 1
	nncr = 0
	DO  i = 1, isiz
C
C*	    Find numeric characters.
C
	    IF ( string (i:i) .ne. '0' .and.
     +	 	 string (i:i) .ne. '1' .and.
     +	 	 string (i:i) .ne. '2' .and.
     +	 	 string (i:i) .ne. '3' .and.
     +	 	 string (i:i) .ne. '4' .and.
     +	 	 string (i:i) .ne. '5' .and.
     +	 	 string (i:i) .ne. '6' .and.
     +	 	 string (i:i) .ne. '7' .and.
     +	 	 string (i:i) .ne. '8' .and.
     +	 	 string (i:i) .ne. '9' )     THEN
		outstr (io:io) = string (i:i)
		io = io + 1
	    ELSE
		nncr = nncr + 1
	    END IF 
C*
	END DO
C*
	lens = io - 1
C*
	RETURN
	END
