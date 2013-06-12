#include <stdio.h>
#include <stdlib.h>

/*Not mine, found at the following
http://stackoverflow.com/questions/1344758/is-it-possible-to-view-a-binary-in-ones-and-zeros
*/


const char *lookup[] = {
   /*  0       1       2       3       4       5       6       7 */
    "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
   /*  8       9       A       B       C       D       E       F */
    "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111",
};

int main(int argc, char *argv[]) {
    FILE *fin;
    int c;
    size_t bytes_read = 0;

    if ( argc != 2 ) {
        fputs("No filename provided", stderr);
        exit(EXIT_FAILURE);
    }

    fin = fopen(argv[1], "rb");
    if ( !fin ) {
        fprintf(stderr, "Cannot open %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }

    while ( EOF != (c = fgetc(fin)) ) {
        printf("%s", lookup[ (c & 0xf0) >> 4 ]);
        printf("%s", lookup[ (c & 0x0f) ]);

        bytes_read += 1;
        if ( bytes_read % 9 == 0 ) {
            puts("");
        }
    }

    fclose(fin);

    return EXIT_SUCCESS;
}
