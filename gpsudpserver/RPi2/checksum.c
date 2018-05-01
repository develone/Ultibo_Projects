/*
**  CHECKSUM.C - Compute the checksum of a file
**
**  public somain demo by Bob Stout
*8 gcc -DTEST checksum.c -o chsum
** ./chsum 
** 17 bytes read
** The checksum of ch_sum_test.dat is 0x27
*/

#include <stdlib.h>

unsigned checksum(void *buffer, size_t len, unsigned int seed)
{
      unsigned char *buf = (unsigned char *)buffer;
      size_t i;
      seed = 0;
      for (i = 0; i < len; ++i)
            seed = seed^(*buf++);
      return seed;
}

#ifdef TEST

#include <stdio.h>

main()
{
      FILE *fp;
      size_t len;
      char buf[4096], *file = "ch_sum_test.dat";

      if (NULL == (fp = fopen(file, "rb")))
      {
            printf("Unable to open %s for reading\n", file);
            return -1;
      }
      len = fread(buf, sizeof(char), sizeof(buf), fp);
      printf("%d bytes read\n", len);
      printf("The checksum of %s is %#x\n", file, checksum(buf, len, 0));
}

#endif
