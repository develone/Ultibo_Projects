#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdlib.h>

void rb_block(unsigned char *in, int *pstart, int *pend, unsigned char *out) {
    int i,j;
    //offset = *pstart*512;
    printf("rb_block %d  %d 0x%x \n",*pstart,*pend,in);
    for(j=*pstart;j<*pend;j++) {
	for(i=0;i<512;i++) {
	    out[i] = in[i];
	    printf("%d %d %d   \n",j, i,in[i],out[i]);
	}
    }
    
}

unsigned char* pgmReadFile(
  char *fname,
  unsigned char *img,
  int *ncols, 
  int *nrows);
void pgmWriteFile(
  char *fname, 
  unsigned char *img, 
  int ncols, 
  int nrows);    
int main(void)
{
    int socket_desc,flag=1,userflg,msg;
    struct sockaddr_in server_addr;
    unsigned char server_message[2047], client_message[2047];
    char *inp1, *inp2, *inp3,*out1;
    unsigned char *inbuf, *inbufsav,*pclient_message;

	int *pncols,*pnrows, ncols, nrows, *pstart, *pend, start, end, k;
	pclient_message = client_message;
	ncols = 512;
	nrows = 512;
	pncols = &ncols;
	pnrows = &nrows;
	start = 0;
	end = 4;
	pstart = &start;
	pend = &end;
	//char *rd_buff;
	inp1 = "red.pgm";
	inp2 = "grn.pgm";
	inp3 = "blu.pgm";
	out1 = "ored.pgm";
	//printf("%s 0x%x %d 0x%x %d \n",inp1,&ncols,ncols,&nrows,nrows);
	//printf("%s 0x%x %d 0x%x %d \n",inp1,pncols,ncols,pnrows,nrows);
	inbuf = (char *)malloc(sizeof(char)*ncols*nrows);
	inbufsav = inbuf;
	printf("%s 0x%x %d 0x%x %d 0x%x\n",inp1,pncols,ncols,pnrows,nrows,inbuf);
	inbuf = pgmReadFile(inp1, inbuf, pncols, pnrows);
	printf("0x%x %d 0x%x %d 0x%x \n",pstart,start,pend,end,inbuf);
	for(k=0;k<1;k++) {
	    
	    rb_block(inbuf,pstart,pend,pclient_message);
	    start += 4;
	    end += 4;
	    inbuf += 2048;
	}
    printf("%s 0x%x %d 0x%x %d \n",out1,&ncols,ncols,&nrows,nrows);
    pgmWriteFile(out1, inbufsav,ncols, nrows);
    printf("%s 0x%x %d 0x%x %d 0x%x\n",inp1,ncols,ncols,pnrows,nrows,inbuf);
    // Clean buffers:
    //memset(server_message,'\0',sizeof(server_message));
    //memset(client_message,'\0',sizeof(client_message));
    
    // Create socket:
    socket_desc = socket(AF_INET, SOCK_STREAM, 0);
    
    if(socket_desc < 0){
        printf("Unable to create socket\n");
        return -1;
    }
    
    printf("Socket created successfully\n");
    
    // Set port and IP the same as server-side:
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(5050);
    server_addr.sin_addr.s_addr = inet_addr("192.168.1.143");
    
    // Send connection request to server:
    if(connect(socket_desc, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0){
        printf("Unable to connect\n");
        return -1;
    }
    printf("Connected with server successfully\n");
    userflg=1;
    //msg=1;
    while(flag) {
        //if(msg==4) msg=1;
        if (userflg==0) {
            
            // Get input from the user:
            printf("Enter message: ");
            gets(client_message);
        }
        else {
	    /*
            printf("%d\n",msg);
            
		
                if(msg==1) sprintf(client_message,"0123456789\n");
                if(msg==2) sprintf(client_message,"abcdef\n");
                if(msg==3) sprintf(client_message,"ghijklmno\n");
                if(msg==4) sprintf(client_message,"pqrstuvwxyz\n");
            
            msg= msg + 1;
            if(msg==5) msg=1;
            sleep(1);
	    */
        }
        // Send the message to server:
        if(send(socket_desc, client_message, strlen(client_message), 0) < 0){
            printf("Unable to send message\n");
            return -1;
        }
        sleep(10);
        // Receive the server's response:
        /*if(recv(socket_desc, server_message, sizeof(server_message), 0) < 0){
            printf("Error while receiving server's msg\n");
            return -1;
        }*/
    
        printf("Server's response: %s\n",server_message);
    }
    
    // Close the socket:
    close(socket_desc);
    
    return 0;
}
