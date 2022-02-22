#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stddef.h>
#include <math.h>
#include <float.h>

#include "pds4.h"
#include "gds.h"
#include "bms.h"
#include "bds.h"
#include "grib.h"
#define VERSION "fix_afwa snow 16 mesh v1.0"


/*
 * fix afwa snow  9/2019 Public Domain Wesley Ebisuzaki
 *
 *  afwa .. air force weather agency (AF 577 weather squadron)
 *
 *  converts afwa nps/sps to be compatible with copygb
 *  not compatible with grib2ctl
 *
 * Language:
 *    ansi C
 *    big/little endian (untested on big-endian)
 *    tested gcc, icc (on x86_64 linux)
 *
 *  v1.0 9/2019             Wesley Ebisuzaki
 */

/*
 * MSEEK = I/O buffer size for seek_grib
 */

#define MSEEK 1024
#define BUFF_ALLOC0	80000


#ifndef min
   #define min(a,b)  ((a) < (b) ? (a) : (b))
   #define max(a,b)  ((a) < (b) ? (b) : (a))
#endif

 
void main(int argc, char **argv) {

    unsigned char *buffer;
    int i, nx, ny;
    long int len_grib, pos = 0, nxny, buffer_size, count = 1;
    unsigned char *msg, *pds, *gds, *bms, *bds, *pointer;
    FILE *input;
    int n_mod = 0, modified;

    if (argc == 1) {
	fprintf(stderr, VERSION);
	exit(8);
    }

    if ((input = fopen(argv[1],"r+")) == NULL) {
        fprintf(stderr,"could not open file: %s\n", argv[1]);
        exit(7);
    }

    if ((buffer = (unsigned char *) malloc(BUFF_ALLOC0)) == NULL) {
	fprintf(stderr,"not enough memory\n");
    }
    buffer_size = BUFF_ALLOC0;


    for (;;) {
	msg = seek_grib(input, &pos, &len_grib, buffer, MSEEK);
	if (msg == NULL) break;

        /* read all whole grib record */
        if (len_grib + msg - buffer > buffer_size) {
            buffer_size = len_grib + msg - buffer + 1000;
            buffer = (unsigned char *) realloc((void *) buffer, buffer_size);
            if (buffer == NULL) {
                fprintf(stderr,"ran out of memory\n");
                exit(8);
            }
        }
        read_grib(input, pos, len_grib, buffer);
	modified = 0;

	/* parse grib message */

	msg = buffer;
        pds = (msg + 8);
        pointer = pds + PDS_LEN(pds);

        if (PDS_HAS_GDS(pds)) {
            gds = pointer;
            pointer += GDS_LEN(gds);
        }
        else {
            gds = NULL;
        }

        if (PDS_HAS_BMS(pds)) {
            bms = pointer;
            pointer += BMS_LEN(bms);
        }
        else {
            bms = NULL;
        }

        bds = pointer;
        pointer += BDS_LEN(bds);

        /* end section - "7777" in ascii */
        if (pointer[0] != 0x37 || pointer[1] != 0x37 ||
            pointer[2] != 0x37 || pointer[3] != 0x37) {
            fprintf(stderr,"\n\n    missing end section\n");
            fprintf(stderr, "%2x %2x %2x %2x\n", pointer[0], pointer[1], 
		pointer[2], pointer[3]);
	    exit(8);
        }

	/* set to NCEP  to eliminate error message from wgrib */
	PDS_Center(pds) = 7;
	PDS_Vsn(pds) = 2;
	modified = 1;

	if (GDS_Polar(gds)  ) {
	    fprintf(stderr,"found polar stereographic grid\n");
	    fprintf(stderr,"Lov=%d\n", GDS_Polar_Lov(gds));
	    /* if Lov == 100 -> -80 */
	    if (GDS_Polar_Lov(gds) == 100000) set_int3(-80000,gds+17);
	}
	else {
	    fprintf(stderr,"expected polar stereographic grid\n");
	}
	if (modified) {
	    n_mod++;
	    if (fseek(input,pos,SEEK_SET) == -1) {
		fprintf(stderr,"fatal error - i/o system\n");
		exit(12);
	    }
	    i = fwrite(msg, sizeof (unsigned char), len_grib, input);
	    if (i != len_grib) {
		fprintf(stderr,"fatal error - i/o system\n");
		exit(12);
	    }
	} 
        pos += len_grib;
        count++;
    }
    fclose(input);
    printf("number of records modified %d\n",n_mod);
}
