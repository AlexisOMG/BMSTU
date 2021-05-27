#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    struct sockaddr_in peer = {
        AF_INET ,
        htons (9010),
        { inet_addr("185.20.226.174") }
    };

    int s = socket(AF_INET , SOCK_STREAM , 0);
    if (s < 0) {
        fprintf(stderr , "unable to create socket\n");
        return 1;
    }
    int res;
    res = connect(s, ( struct sockaddr *)&peer, sizeof (peer));
    if (res < 0) {
        fprintf(stderr , "unable to connect\n");
        return 1;
    }
    for (;;) {
        char *message = (char*)calloc(128, sizeof(char));
        printf("Enter message: ");
        fflush(stdin);
        fgets(message, 127, stdin);
        res = send(s, message, sizeof(char) * strlen(message), 0);
        if (res < 0) {
            res = close(s);
            if (res < 0) {
                fprintf(stderr , "unable to close\n");
                return 1;
            }
            fprintf(stderr , "unable to send\n");
            return 1;
        }
        if (strncmp("exit", message, 4) == 0) {
            break;
        }
        free(message);
        char *buf = (char*)calloc(127, sizeof(char));
        res = recv(s, buf , 127, 0);
        if (res < 0) {
            res = close(s);
            if (res < 0) {
                fprintf(stderr , "unable to close\n");
                return 1;
            }
            fprintf(stderr , "unable to recive\n");
            return 1;
        }
        printf("Answer: %s\n", buf);
        free(buf);
    }
    res = close(s);
    if (res < 0) {
        fprintf(stderr , "unable to close\n");
        return 1;
    }
    return 0;
}
