#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "gribwlib.h"
#include "ncepopn.h"


/* PROGRAM DOCUMENTATION BLOCK

PROGRAM: narr_gribify_conus_gauge
  PRGMMR:  Wesley Ebisuzaki              ORG: W/NP52    DATE: 2009-04

ABSTRACT: 
  Takes CPC daily conus precipitation analysis (binary file)
  and converts to grib

Program History log:
    2009-04 W Ebisuzaki

USAGE:

   % narr_gribify_conus_gauge.x [in bin-file] [out gribfile] [YYYYMMDDHH]

INPUT FILES:
    binary daily cpc daily precipitation

OUTPUT FILES:
    grib file

SUBROUTINES CALLED:
    rd_grib_rec
    wrt_grib_rec
    PDS_tool
    new_LatLon_GDS
    swap (local)

ATTRIBUTES:
   LANGUAGE: ANSI/ISO C

*/

#define NXNY		300*120
#define LOCAL_UNDEF	-999.0
/* AIX: define SWAP */
/* #define SWAP */

void swap(char *buffer, int n);


/*
 * gribify an cpc conus 0.25 precip
 *  
 * uses PDStool(), NCEP_GDS()
 * v1.1 convert from .1 mm/day to MKS
 * v1.2 convert from .1 mm/hour to MKS
 */

int main(int argc, char **argv) {

    unsigned char *pds, *pds_debug, *gds;
    FILE *input, *output;
    int count = 0, yyyymmdd, hh, i, header;
    float data[NXNY];

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
    if ((count=sscanf(argv[3], "%8d%2d", &yyyymmdd, &hh)) != 2) {
        fprintf(stderr,"time?: %s %d %d err=%d \n", argv[3], yyyymmdd, hh, count);
        exit(7);
    }

    printf("initial YYYYMMDD is %d-%d\n", yyyymmdd, hh);

    /* generate a PDS */

    pds = PDStool(New_PDS, 		/* new PDS */
	NCEP_opn, 			/* center, subcenter */
        P_param_table(2), 		/* parameter table */
	P_process(0),			/* process that made data */
	P_param(PRATE), P_sfc,		/* variable and level */
	P_date(yyyymmdd),		/* initial date yyyymmdd */
	P_hour(hh),
	P_ave_dy(0,1),			/* 1 day average */
	P_dec_scale(7),			/* scale numbers by 10**5 */
	P_end);				/* end of arguments */

    /* generate a GDS */

    /* use NCEP grid number 2 */

    /*  gds = NCEP_GDS(pds,GDS_GRID);	*/ /* use 144x73 grid */

    /* or make a user defined GDS for a 2.5 x 2.5 grid */

    gds = new_LatLon_GDS(pds,300,120, 230.125,20.125, 304.875,49.875,0.25,0.25);

    if (fread(&(data[0]), sizeof(float), NXNY, input) == NXNY) {

#ifdef SWAP
        swap( (char *) data, NXNY );
#endif

	for (i = 0; i < NXNY; i++) {
		if (data[i] == LOCAL_UNDEF) data[i] = UNDEFINED;
		else data[i] = data[i] * 0.1 / 86400.0;	/* .1mm/day -> mm/sec */
	}
	wrt_grib_rec(pds, gds, data, NXNY, output);
	count=1;
    }

    free(pds);
    free(gds);

    printf("%d records converted\n", count);

    fclose(input);
    fclose(output);
}


/* big <=> little endian swap */

void swap(char *buffer, int n) {
    char c1, c2;
    int j;
    n = n*4;

    for (j = 0; j < n; j += 4) {
         c1 = buffer[j];
         buffer[j] = buffer[j+3];
         buffer[j+3] = c1;
         c2 = buffer[j+1];
         buffer[j+1] = buffer[j+2];
         buffer[j+2] = c2;
     }
}


