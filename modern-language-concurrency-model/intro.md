title: Modern Language Concurrency Strategy
speaker: zero马达
url: https://github.com/zerolocusta
transition: slide3
files: /js/demo.js, /css/demo.css

----
[slide]

# Modern Language Concurrency Model

## 现代语言服务器并发模型

### zero马达

----
[slide]

# 使用语言 

- C (然而讲并发并不能没有C)
- Python (3.5)
- Go (CSP, goroutine)
- Erlang (抢占式协程, 非严格Actor)
- Rust
- Clojure (STM)

----
[slide]

# 并发逻辑的载体

- multiprocessing
- multithreading
- coroutine

----
[slide]

# 基本并发/并行服务器

----
[slide]

##fork/0

```
#include "unp.h"#include <time.h>int handle_connection(int, struct sockaddr_in *);int main(int argc, char **argv){    int listenfd, connfd, pid, len;    struct sockaddr_in servaddr, cliaddr;    listenfd = Socket(AF_INET, SOCK_STREAM, 0);    bzero(&servaddr, sizeof(servaddr));    servaddr.sin_family = AF_INET;    servaddr.sin_addr.s_addr = htonl(INADDR_ANY);    servaddr.sin_port = htons(13);    
    Bind(listenfd, (SA *) &servaddr, sizeof(servaddr));    
    Listen(listenfd, LISTENQ);    while(1){        int len = sizeof(cliaddr);        connfd = Accept(listenfd, (SA *) &cliaddr, &len);        if ( (pid = Fork()) == 0 ){            Close(listenfd);            handle_connection(connfd, &cliaddr);            exit(0);        }        Close(connfd);    }}
```

----
[slide]

```int handle_connection(int connfd, struct sockaddr_in *cliaddr){    time_t ticks;    char buff[MAXLINE];    printf("connection from %s, port %d\n",            inet_ntop(AF_INET, &cliaddr->sin_addr , buff, sizeof(buff)),            ntohs(cliaddr -> sin_port));    ticks = time(NULL);    snprintf(buff, sizeof(buff), "%.24s\r\n", ctime(&ticks));    Write(connfd, buff, strlen(buff));    Close(connfd);}
```

----
[slide]
# 并发模型
## 用于处理并发逻辑之间的通信
#### 这次就讲三件小事


1. STM
2. Actor
3. CSP

----
[slide]



----
[slide]

##hello
