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
#define VERSION "new date v1.0"


/*
 * this code updates the reference date code on a grib-1 files
 *
 *                              Wesley Ebisuzaki
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

int main(int argc, char **argv) {

    unsigned char *buffer;
    int i, nx, ny;
    long int date;
    int year,month,day,hour;
    long int len_grib, pos = 0, nxny, buffer_size, count = 1;
    unsigned char *msg, *pds, *gds, *bms, *bds, *pointer;
    FILE *input;
    int n_mod = 0, modified;

    if (argc  != 3) {
	printf(VERSION);
	printf("\n  %s gribfile YYYYMMDDHH\n", argv[0]);
	exit(8);
    }

    if ((input = fopen(argv[1],"r+")) == NULL) {
        fprintf(stderr,"could not open file: %s\n", argv[1]);
        exit(7);
    }
    date = atoi(argv[2]);

    if ((buffer = (unsigned char *) malloc(BUFF_ALLOC0)) == NULL) {
	fprintf(stderr,"not enough memory\n");
    }
    buffer_size = BUFF_ALLOC0;
    hour = date % 100;
    date = date / 100;
    day = date % 100;
    date = date / 100;
    month = date % 100;
    date = date / 100;
    year = date;

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

        PDS_Hour(pds) = hour;
        PDS_Day(pds) = day;
        PDS_Month(pds) = month;
	PDS_Century(pds) = (year-1)/100 + 1;
	PDS_Year(pds) = (year-1) % 100 + 1;


	
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

        pos += len_grib;
        count++;
    }
    fclose(input);
    printf("number of records modified %d\n",n_mod);
    return 0;
}
