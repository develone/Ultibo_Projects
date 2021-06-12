#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>

int main(void)
{
    int socket_desc,flag=1,userflg,msg;
    struct sockaddr_in server_addr;
    char server_message[2000], client_message[2000];
    
    // Clean buffers:
    memset(server_message,'\0',sizeof(server_message));
    memset(client_message,'\0',sizeof(client_message));
    
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
    server_addr.sin_addr.s_addr = inet_addr("192.168.1.247");
    
    // Send connection request to server:
    if(connect(socket_desc, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0){
        printf("Unable to connect\n");
        return -1;
    }
    printf("Connected with server successfully\n");
    userflg=1;
    msg=1;
    while(flag) {
        //if(msg==4) msg=1;
        if (userflg==0) {
            
            // Get input from the user:
            printf("Enter message: ");
            gets(client_message);
        }
        else {
            printf("%d\n",msg);
            
 
                if(msg==1) sprintf(client_message,"0123456789\n");
                if(msg==2) sprintf(client_message,"abcdef\n");
                if(msg==3) sprintf(client_message,"ghijklmno\n");
                if(msg==4) sprintf(client_message,"pqrstuvwxyz\n");
            
            msg= msg + 1;
            if(msg==5) msg=1;
            sleep(1);
        }
        // Send the message to server:
        if(send(socket_desc, client_message, strlen(client_message), 0) < 0){
            printf("Unable to send message\n");
            return -1;
        }
    
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
