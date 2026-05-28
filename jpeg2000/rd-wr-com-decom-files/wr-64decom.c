#include <stdio.h>
#include <stdlib.h>

/* to compile gcc -Wall -Werror wr-64decom.c -o wr-64decom */

int main() {
    FILE *fptr;
    int CR, enc, xx0, yy0, xx1, yy1;
    CR = 25;
    enc = 0;
    xx0 = 0;
    yy0 = 0;
    xx1 = 64;
    yy1 = 64;
    int buffer[6]; // Storage for the data we read
    buffer[0] = CR;
    buffer[1] = enc;
    buffer[2] = xx0;
    buffer[3] = yy0;
    buffer[4] = xx1;
    buffer[5] = yy1;
    printf("CR %d  enc %d  \n", CR, enc);
    printf("CR 0x%x  enc 0x%x  \n", CR, enc);
    printf("xx0 %d yy0 %d \n", xx0,yy0);
    printf("xx0 0x%x yy0 0x%x \n", xx0,yy0);
    printf("xx1 %d yy1 %d \n", xx1,yy1);
    printf("xx1 0x%x yy1 0x%x \n", xx1,yy1);
    // 1. Open file in "wb" (wb binary) mode
    // Using the standard fopen function as described on [GeeksforGeeks](https://www.geeksforgeeks.org/c/basics-file-handling-c/)
    fptr = fopen("test_64decom", "wb");

    // 2. Check for successful opening
    if (fptr == NULL) {
        perror("Error opening file");
        return 1;
    }
    // 3. Read 6 int from the file into the buffer
    // fread returns the number of elements successfully read [GeeksforGeeks](https://www.geeksforgeeks.org/c/basics-file-handling-c/)
    size_t elements_write = fwrite(buffer, sizeof(int), 6, fptr);
    if (elements_write < 6) {
        if (feof(fptr)) {
            printf("End of file reached. Write %zu elements.\n", elements_write);
        } else if (ferror(fptr)) {
            perror("Error write from file");
        }
    }
    CR = buffer[0];
    enc = buffer[1];
    printf("CR %d  enc %d  \n", CR, enc);
    printf("CR 0x%x  enc 0x%x  \n", CR, enc);
    xx0 = buffer[2];
    yy0 = buffer[3];
    printf("xx0 %d yy0 %d \n", xx0,yy0);
    printf("xx0 0x%x yy0 0x%x \n", xx0,yy0);
    xx0 = buffer[3];
    yy0 = buffer[4];
    printf("xx1 %d yy1 %d \n", xx1,yy1);
    printf("xx1 0x%x yy1 0x%x \n", xx1,yy1);
}
