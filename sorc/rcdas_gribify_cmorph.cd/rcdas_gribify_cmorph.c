#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "gribwlib.h"
#include "ncepopn.h"

#define NX		1440
#define NY		480
#define NXNY		(NX*NY)
#define GDS_GRID	255
#define LOCAL_UNDEF	-9999.0

#define SWAP_IN
#define SWAP_OUT

/*
 * gribify Global Hourly CMORPH precip field 1440x480 grid
 */


/* PROGRAM DOCUMENTATION BLOCK

PROGRAM: narr_gribify_cmorph
  PRGMMR:  E. Yarosh                      ORG: W/NP52    DATE: ????-??

ABSTRACT: 
  Takes two 30 minute CMORPH fields and average them into 1 hourly field and write in GRIB

Program History log:
           Evegeny Yorash
           Marco Carrera
   2004-05 W. Ebisuzaki added averaging
   2013-01 W. Ebisuzaki fixed for little endian

USAGE:

   % rcdas_gribify_cmorph [out gribfile] [out binfile] [YYYYMMDDHH] (list of 30 minute CMORPH files)

INPUT FILES:
    binary 30 minute CMORPH data

OUTPUT FILES:
    hourly 60 minute CMORPH data, grib format
    hourly 60 minute CMORPH data, binary format  (same endian as original)

SUBROUTINES CALLED:
    rd_grib_rec
    wrt_grib_rec
    PDS_tool
    new_LatLon_GDS
    set_def_power2
    set_BDSMinBits
    invert (local)
    swap_local (local)

ATTRIBUTES:
   LANGUAGE: ANSI/ISO C

*/

void swap_local(unsigned char *buffer, int n);
void invert(float *data, int nx, int ny);

int main(int argc, char **argv) {

    unsigned char *pds, *gds;
    FILE *input, *output, *bin_out, *debug;
    int count[NXNY], yyyymmddhh, i, j, hour, date;
    float data[NXNY], factor, *sum, *sum2, debugval;
    int n;

    sum = malloc(sizeof(float)* NXNY);
    sum2 = malloc(sizeof(float)* NXNY);
    if (sum == NULL || sum2 == NULL) {
       printf("MALLOC ERROR\n");
       exit(88);
    }

    /* preliminaries .. open up all files */

    if (argc <= 4) {
	fprintf(stderr, "%s [out gribfile] [out binfile] [YYYYMMDDHH] (list of 30 minute CMORPH files)\n", argv[0]);
	exit(8);
    }
    n = 2*(argc - 4);
    printf("number of in ave=%d\n",n);

    /* open output grib file */

    if ((output = fopen(argv[1],"wb")) == NULL) {
        fprintf(stderr,"could not open file: %s\n", argv[1]);
        exit(7);
    }

    /* open output bin file */
    if ((bin_out = fopen(argv[2],"wb")) == NULL) {
        fprintf(stderr,"could not open file: %s\n", argv[2]);
        exit(7);
    }

    debug = fopen("gribify_cmorph.debug","w");

    /* read date code */

    yyyymmddhh = atoi(argv[3]);
    printf("initial YYYYMMDDHH is %d\n", yyyymmddhh);
    hour=yyyymmddhh%100;
    date=yyyymmddhh/100;

    for (i = 0; i < NXNY; i++) {
        count[i] = 0;
	sum[i] = 0;
	sum2[i] = 0;
    }

    /* open and read in-1..in-N */
    for (j = 0; j < n; j++) {

        if (j % 2 == 0) {
printf("open %s\n", argv[4+j/2]);
            if ((input = fopen(argv[4+j/2],"rb")) == NULL) {
                fprintf(stderr,"could not open file: %s\n", argv[4+j/2]);
                exit(7);
            }
        }
        if (fread(&(data[0]), sizeof(float), NXNY, input) != NXNY) {
            fprintf(stderr,"Too bad not enough data in %s\n", argv[3+j/2]);
            exit(8);
        }

#ifdef SWAP_IN
        swap_local( (unsigned char *) data, NXNY );
#endif

        for (i = 0; i < NXNY; i+= 100) { fprintf(debug,"grid=%d value=%f\n", i,data[i]); }
        debugval=-9999;

        for (i = 0; i < NXNY; i++) { if (debugval < data[i]) debugval = data[i]; }
        fprintf(debug,"max=%f\n", debugval);

        for (i = 0; i < NXNY; i++) { if (debugval > data[i]) debugval = data[i]; }
        fprintf(debug,"min=%f\n", debugval);

        for (i = 0; i < NXNY; i++) { if (debugval < data[i] && data[i] < 0) debugval = data[i]; }
        fprintf(debug,"max less than zero =%f\n", debugval);

	if (j % 2 == 1) fclose(input);

	for (i = 0; i < NXNY; i++) {
            if (data[i] >= 0.0) {
	        count[i] += 1;
	        sum[i] += data[i];
	    }
	    else if (data[i] >= -9998.0) {
	        fprintf(stderr,"weird data  -- val=%g i=%d\n", data[i],i);
                exit(9);
            }
	}
    }

    pds = PDStool(New_PDS, 		/* new PDS */
	NCEP_opn, 			/* center, subcenter */
        P_param_table(2), 		/* parameter table */
	P_process(0),			/* process that made data */

	P_param(PRATE), P_sfc,	        /* variable and level */
	P_date(date),	                /* initial date yyyymmdd */
	P_hour(hour),
        P_ave_hr(0,n/2),
	P_dec_scale(7),		        /* scale numbers by 10**7 */
	P_end);				/* end of arguments */

    /* generate a GDS */

    gds = new_LatLon_GDS(pds,1440,480,0.125,-59.875,-0.125,59.875,0.25,0.25);

    /*  process the data */
    factor = 1.0 / 3600.0;
    for (i = 0; i < NXNY; i++) {
	if (count[i] == 0) {
	    sum[i] = UNDEFINED;
	    sum2[i] = -9999.0;
	}
	else {
	    sum2[i] = sum[i] / count[i];
	    sum[i] = sum[i] * factor / count[i];
	}
    }
    printf("writing grib file\n");
    invert(sum, NX, NY);
    wrt_grib_rec(pds, gds, sum, NXNY, output);

    invert(sum2, NX, NY);
#ifdef SWAP_OUT
    swap_local( (unsigned char *) sum2, NXNY );
#endif

    if (fwrite(sum2, sizeof(float), NXNY, bin_out) != NXNY) {
        fprintf(stderr,"problem writing to binary output file\n");
        exit(8);
    }

    free(pds);
    free(gds);

    printf("finished\n");

    fclose(output);
    return 0;
}

/* big <=> little endian swap */

void swap_local(unsigned char *buffer, int n) {
    unsigned char c1, c2;
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

/* ns to sn grid, note data is in FORTRAN order */

void invert(float *data, int nx, int ny) {

    int i, j;
    float tmp, *p1, *p2;
    for (j = 0; j < ny/2; j++) {
        p1 = data + j*nx;
	p2 = data + (ny-1-j)*nx;
	/* swap N and S rows */
        for (i = 0; i < nx; i++) {
	    tmp = *p1;
	    *p1++ = *p2;
	    *p2++ = tmp;
        }
    }
}
