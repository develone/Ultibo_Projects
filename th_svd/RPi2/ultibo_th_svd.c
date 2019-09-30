#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include<unistd.h>
#include "master_slave.h"


void test_svd() {
	
	
	
	char *inp1, *inp2, *inp3;
	struct FILEs *files1 = (struct FILEs *)malloc(sizeof(struct FILEs));
    struct FILEs *files2 = (struct FILEs *)malloc(sizeof(struct FILEs));
    struct FILEs *files3 = (struct FILEs *)malloc(sizeof(struct FILEs));
    
    inp1 = "red.pgm";
    inp2 = "grn.pgm";
    inp3 = "blu.pgm";
    
    files1->input_file = inp1;
    files2->input_file = inp2;
    files3->input_file = inp3;

    inp1 = "Sred.bin";
    inp2 = "Sgrn.bin";
    inp3 = "Sblu.bin";    
 
    
	files1->first_output = inp1;
	files2->first_output = inp2;
	files3->first_output = inp3;
	
	inp1 = "rcred.bin";
    inp2 = "rcgrn.bin";
    inp3 = "rcblu.bin";    
 
    
	files1->second_output = inp1;
	files2->second_output = inp2;
	files3->second_output = inp3;
	
    files1->status = 0;
    files2->status = 0;
    files3->status = 0;
    
    files1->num_bytes_rd = 0;
    files2->num_bytes_rd = 0;
    files3->num_bytes_rd = 0;
    
    files1->mem_allocated = 0;
    files2->mem_allocated = 0;
    files3->mem_allocated = 0;
        
    printf("In main %s %s %s %d %d\n",files1->input_file,files1->first_output,files1->second_output,files1->status,files1->mem_allocated);
    printf("In main %s %s %s %d %d\n",files2->input_file,files2->first_output,files2->second_output,files2->status,files2->mem_allocated);
    printf("In main %s %s %s %d %d\n",files3->input_file,files3->first_output,files3->second_output,files3->status,files3->mem_allocated);

   
    struct args *Allen = (struct args *)malloc(sizeof(struct args));
    char f1[] = "files1";
    char allen[] = "Allen";
    Allen->name = allen;
    Allen->age = 20;
    

    

    pthread_t tid;
    th_id[0] = 0;
    th_id[1] = 0;
    th_id[2] = 0;
    pthread_create(&tid, NULL, hello, (void *)Allen);
    pthread_join(tid, NULL);
    printf("0x%x\n",th_id[0]);
    pthread_create(&th_id[0], NULL, mysvd, (void *)files1);
    
	pthread_join(th_id[0], NULL);
	
	pthread_create(&th_id[1], NULL, mysvd, (void *)files2);
	
	pthread_join(th_id[1], NULL);

	pthread_create(&th_id[2], NULL, mysvd, (void *)files3);

	pthread_join(th_id[2], NULL);
	printf("all threads joined\n");
	 

		printf("In main status %d num_bytes_rd %d \n",files1->status,files1->num_bytes_rd);
		printf("In main status %d num_bytes_rd %d \n",files2->status,files2->num_bytes_rd);
		printf("In main status %d num_bytes_rd %d \n",files3->status,files3->num_bytes_rd);
	
    
    free(files1);
    free(files2);
    free(files3);
    
    free(Allen);
}
