#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char* itoa(int i, char b[]){
    char const digit[] = "0123456789";
    char* p = b;
    if (i < 0) {
        *p++ = '-';
        i *= -1;
    }
    int shifter = i;
    do {
        ++p;
        shifter = shifter / 10;
    } while(shifter);
    *p = '\0';
    do {
        *--p = digit[i % 10];
        i = i / 10;
    } while(i);
    return b;
}

void toUpper(char *str) {
    for (int i = 0; i < strlen(str); ++i) {
        str[i] = (char)toupper(str[i]);
    }
}

void handle(int s, int s2, struct sockaddr_in *client_addr) {
    int res;
    for (;;) {
        char *buf = (char*)calloc(127, sizeof(char));
        res = recv(s2 , buf , 127, 0);
        if (res < 0) {
            res = close(s2);
            if (res < 0) {
                fprintf(stderr , "unable to close\n");
                exit(EXIT_FAILURE);
            }
            res = close(s);
            if (res < 0) {
                fprintf(stderr , "unable to close\n");
                exit(EXIT_FAILURE);
            }
            fprintf(stderr , "unable to recive\n");
            exit(EXIT_FAILURE);
        }
        printf("%s from %s:%hu\n", buf, inet_ntoa(client_addr->sin_addr), ntohs(client_addr->sin_port ));
        if (strncmp("exit", buf, 4) == 0) {
            break;
        }
        char *answ = (char*)calloc(127, sizeof(char));
        answ = itoa(strlen(buf) - 1, answ);
        res = send(s2 , answ, strlen(answ), 0);
        if (res < 0) {
            res = close(s2);
            if (res < 0) {
                fprintf(stderr , "unable to close\n");
                exit(EXIT_FAILURE);
            }
            res = close(s);
            if (res < 0) {
                fprintf(stderr , "unable to close\n");
                exit(EXIT_FAILURE);
            }
            fprintf(stderr , "unable to send\n");
            exit(EXIT_FAILURE);
        }
        free(buf);
    }
}

int main() {
    struct sockaddr_in addr = {
        AF_INET ,
        htons (9010),
        { inet_addr("127.0.0.1") }
    };

    int s = socket(AF_INET , SOCK_STREAM , 0);
    if (s < 0) {
        fprintf(stderr , "unable to create socket\n");
        return 1;
    }
    int res = bind(s, ( struct sockaddr *)&addr, sizeof (addr));
    if (res < 0) {
        fprintf(stderr , "unable to bind\n");
        return 1;
    }

    res = listen(s, 32);
    if (res < 0) {
        fprintf(stderr , "unable to listen\n");
        return 1;
    }
    struct sockaddr_in client_addr;
    int client_addr_size = sizeof (client_addr );
    int s2 = accept (s,( struct sockaddr *)&client_addr, &client_addr_size );
    if (s2 < 0) {
        fprintf(stderr , "unable accept\n");
        return 1;
    }
    handle(s, s2, &client_addr);
    res = close(s2);
    if (res < 0) {
        fprintf(stderr , "unable to close\n");
        return 1;
    }
    res = close(s);
    if (res < 0) {
        fprintf(stderr , "unable to close\n");
        return 1;
    }
    return 0;
}
