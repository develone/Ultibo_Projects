#include "filter.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>


void band_low_example()
{
    //BWBandPass* filter = create_bw_band_pass_filter(4, 250, 2, 45);
    BWLowPass* filter = create_bw_low_pass_filter(4,250,45);
    for(int i = 0; i < 100; i++){
        printf("Output[%d]:%f\n", i, bw_low_pass(filter, i* 100));
    }

    free_bw_low_pass(filter);

}

int test() {   
    printf("========= Band low filter example =========\n\n");
    band_low_example();
    printf("========= Done. =========\n\n");
    return 0;
}
