/* PROGRAM DOCUMENTATION BLOCK

PROGRAM: fix_conv
  PRGMMR:  W. Ebisuzaki              ORG: W/NP52    DATE: 2004-05

ABSTRACT: 
  The narr generated water vapor and cloud water flux convergences.  However, the
convergences values were not scaled by 1/grid-length.  This program reads (1) a grib
table with all the grid lengths, (2) a grid 221 grib file and scales the moisture 

Program History log:
   2004-05 W. Ebisuzaki

USAGE:

   % fix_rr_conv.x [in gribfile] [out gribfile] [grib files with dx values]


INPUT FILES:
    grib files with dx values
    NARR grib file

OUTPUT FILES:
    fixed NARR grib file

SUBROUTINES CALLED:
    rd_grib_rec
    wrt_grib_rec
    PDS_tool
    set_def_power2
    set_BDSMinBits

ATTRIBUTES:
   LANGUAGE: ANSI/ISO C

*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>
#include <float.h>

#include "gribw.h"
#include "gribwlib.h"
#include "pds4.h"
#include "bds.h"

#include "nceptab_131.h"

/*
 * fix the Regional Reanalysis AWIP fields
 *   fix moisture flux convergence
 *
 *                              Wesley Ebisuzaki
 */

#define NXNY (349*277)

int main(int argc, char **argv) {

    long int len_grib, pos = 0;
    unsigned char *pds, *gds;
    FILE *input, *output, *dx_file;
    int count, cnvt, scale10, scale2;
    int ndata;
    float *array = NULL, dx[NXNY];
    int i, fld;


    /* preliminaries .. open up all files */

    if (argc != 4) {
	fprintf(stderr, "%s [in gribfile] [out gribfile] [AWIP32_dx.grib]\n", argv[0]);
	exit(8);
    }
    if ((input = fopen(argv[1],"rb")) == NULL) {
	fprintf(stderr,"could not open file: %s\n", argv[1]);
	exit(7);
    }
    if ((output = fopen(argv[2],"wb")) == NULL) {
	fprintf(stderr,"could not open file: %s\n", argv[2]);
	exit(7);
    }
    if ((dx_file = fopen(argv[3],"rb")) == NULL) {
	fprintf(stderr,"could not open file: %s\n", argv[3]);
	exit(7);
    }

    /* read the scaling factors */

    pos = 0;
    len_grib = rd_grib_rec(dx_file, pos, &pds, &gds, &array, &ndata);
    if (len_grib <= 0 || ndata != NXNY) {
        fprintf(stderr,"bad dx file\n");
        exit(9);
    }
    for (i = 0; i < NXNY; i++) {
	if (UNDEFINED_VAL(array[i])) {
	    dx[i] = 0.0;
	}
	else {
            dx[i] = 1.0 / array[i];
	}
    }
    fclose(dx_file);


    set_BDSMinBits(-1);	/* ETA-style mode */

    pos = count = cnvt = 0;
    for(;;) {

	/* read the grib file */
        len_grib = rd_grib_rec2(input, pos, &pds, &gds, &array, &ndata, 
          &scale10, &scale2);
        set_def_power2(-scale2);

	if (len_grib <= 0) break;
	if (ndata != NXNY) {
	    fprintf(stderr,"error in grid size\n");
	    exit(8);
	}
        fld = PDS_PARAM(pds);
	if (fld == WVCONV || fld == WCCONV) {
	    for (i = 0; i < NXNY; i++) {
		if (UNDEFINED_VAL(array[i]) || dx[i] == 0.0) {
		    array[i] = UNDEFINED;
		}
		else {
		    array[i] *= dx[i];
		}
	    }
	    pds = PDStool(pds, P_dec_scale(4), P_end);
	    cnvt++;
	}        
	wrt_grib_rec(pds, gds, array, ndata, output);

	pos += len_grib;
	count++;
    }
    fclose(output);
    fclose(input);
    fprintf(stderr,"fixed %d records out of %d\n", cnvt, count);
    return 0;
}

