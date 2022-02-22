#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "grib.h"

/* public domain 9/2019 Wesley Ebisuzaki
   set 3bytes (grib1 stream) to a signed int
 */
void set_int3(int i, unsigned char *p) {
    int j;

    if (i >= 0) {
	p[0] = (i >> 16) & 255;
	p[1] = (i >> 8) & 255;
	p[2] = i & 255;
    }
    else {
        j = -i;
	p[0] = ((j >> 16) & 255) | 128;
	p[1] = (j >> 8) & 255;
	p[2] = j & 255;
    }
    return;
}
