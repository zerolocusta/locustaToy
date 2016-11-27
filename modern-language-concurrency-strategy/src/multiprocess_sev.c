#include "unp.h"
#include <time.h>

int handle_connection(int, struct sockaddr_in *);

int main(int argc, char **argv){
    int listenfd, connfd, pid, len;
    struct sockaddr_in servaddr, cliaddr;

    listenfd = Socket(AF_INET, SOCK_STREAM, 0);

    bzero(&servaddr, sizeof(servaddr));

    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
    servaddr.sin_port = htons(13);
    
    Bind(listenfd, (SA *) &servaddr, sizeof(servaddr));
    
    Listen(listenfd, LISTENQ);
    while(1){
        int len = sizeof(cliaddr);
        connfd = Accept(listenfd, (SA *) &cliaddr, &len);
        if ( (pid = Fork()) == 0 ){
            Close(listenfd);
            handle_connection(connfd, &cliaddr);
            exit(0);
        }
        Close(connfd);
    }
}

int handle_connection(int connfd, struct sockaddr_in *cliaddr){

    time_t ticks;
    char buff[MAXLINE];
    printf("connection from %s, port %d\n",
            inet_ntop(AF_INET, &cliaddr->sin_addr , buff, sizeof(buff)),
            ntohs(cliaddr -> sin_port));
    ticks = time(NULL);
    snprintf(buff, sizeof(buff), "%.24s\r\n", ctime(&ticks));
    Write(connfd, buff, strlen(buff));
    Close(connfd);

}
