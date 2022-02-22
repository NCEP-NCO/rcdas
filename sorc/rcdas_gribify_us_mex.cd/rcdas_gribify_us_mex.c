#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "gribwlib.h"
#include "ncepopn.h"

#define NXNY		321*201
#define GDS_GRID	255
#define LOCAL_UNDEF	-9999.0

#define SWAP

/* PROGRAM DOCUMENTATION BLOCK

PROGRAM: narr_gribify_us_mex
  PRGMMR:  E.  Yorash              ORG: W/NP52    DATE: ????-??

ABSTRACT: 
  Takes a 1 hour us-mex precip field and converts it into grib

Program History log:
            Evegeny Yorash
    ????-?? Marco Carrera
    2013-01 W. Ebisuzaki swap_flt4 for binary input that is in wrong endian
USAGE:

   % narr_gribify_us_mex.x [in bin-file] [out gribfile] [YYYYMMDDHH]

INPUT FILES:
    binary 60 minute US-MEX precip data

OUTPUT FILES:
    grib 60 minute US-MEX precip data

SUBROUTINES CALLED:
    rd_grib_rec
    wrt_grib_rec
    PDS_tool
    new_LatLon_GDS

ATTRIBUTES:
   LANGUAGE: ANSI/ISO C

*/
void swap_flt4(unsigned char *, int);

int main(int argc, char **argv) {

    unsigned char *pds, *gds;
    FILE *input, *output;
    int count = 0, yyyymmddhh, i, hour, date;
    float data[NXNY], factor;

    /* preliminaries .. open up all files */

    if (argc != 4) {
	fprintf(stderr, "%s [in bin-file] [out gribfile] [YYYYMMDDHH]\n", argv[0]);
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
    yyyymmddhh = atoi(argv[3]);
    printf("initial YYYYMMDDHH is %d\n", yyyymmddhh);
    hour=yyyymmddhh%100;
    date=yyyymmddhh/100;

    /* generate a PDS */

    pds = PDStool(New_PDS, 		/* new PDS */
	NCEP_opn, 			/* center, subcenter */
        P_param_table(2), 		/* parameter table */
	P_process(0),			/* process that made data */

	P_param(PRATE), P_sfc,	        /* variable and level */
	P_date(date),	                /* initial date yyyymmdd */
	P_hour(hour),

	P_ave_hr(0,1),			/* averaged from hour 0 to 1 */

	P_dec_scale(7),		        /* scale numbers by 10**7 */
	P_end);				/* end of arguments */

    /* generate a GDS */

    gds = new_LatLon_GDS(pds,321,201,-140.0,10.0,-60.0,60.0,0.25,0.25);

    /* read and scale the data */
    factor = 1.0 / 3600.0;
    if (i=fread(&(data[0]), sizeof(float), NXNY, input) == NXNY) {
#ifdef SWAP
        swap_flt4((unsigned char *)data, NXNY);
#endif
	for (i = 0; i < NXNY; i++) {
		if (data[i] == LOCAL_UNDEF) {
                    data[i] = UNDEFINED;
                }	
                else {
                    data[i] *= factor;
                }
        }
	wrt_grib_rec(pds, gds, data, NXNY, output);

	count++;
    }
    free(pds);
    free(gds);

    printf("%d records converted\n", count);

    fclose(input);
    fclose(output);
    return 0;
}

/* byte swaps n 4byte floats */
void swap_flt4(unsigned char *a, int n) {
    unsigned char tmp1, tmp2, tmp3, tmp4;
    while (n-- > 0) {
	tmp1 = a[0];
	tmp2 = a[1];
	tmp3 = a[2];
	tmp4 = a[3];

	a[0] = tmp4;
	a[1] = tmp3;
	a[2] = tmp2;
	a[3] = tmp1;

	a += 4;
    }
    return;
}
