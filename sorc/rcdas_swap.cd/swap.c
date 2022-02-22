#include <stdio.h>
#include <stdlib.h>

/* take binary file (4 byte quantities and swap byte order) */
/* uses stdin and stdout */


#define BSIZE (4096*4)

int main(int argc, char **argv) {

        char buffer[BSIZE], c1, c2;
	int i, j, ii;
        FILE *in, *out;

	if (argc != 3) {
		fprintf(stderr,"usage: %s [in file] [out file]\n",argv[0]);
		fprintf(stderr,"swaps bytes of binary file (int*4/real*4)\n");
		exit(8);
	}
	if ((in = fopen(argv[1],"rb")) == NULL) {
		fprintf(stderr,"could not open %s as input\n",argv[1]);
		exit(8);
	}
	if ((out = fopen(argv[2],"wb")) == NULL) {
		fprintf(stderr,"could not open %s as as\n",argv[2]);
		exit(8);
	}

        for (;;) {
                i = fread(buffer, sizeof (char), BSIZE, in);
		if (i <= 0) exit(0);

		for (j = 0; j < i; j += 4) {
			c1 = buffer[j];
			buffer[j] = buffer[j+3];
			buffer[j+3] = c1;

			c2 = buffer[j+1];
			buffer[j+1] = buffer[j+2];
			buffer[j+2] = c2;
		}
    		ii = fwrite(buffer, sizeof (char), j, out);
		if (ii != j) exit(9);
		if (i != j) exit(9);
	}
}
