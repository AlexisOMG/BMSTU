package main

import (
    "net"
    "log"
    "bufio"
    "strings"
    "strconv"
)

func handleClient(port net.Conn) {
    defer port.Close()
    word := ""
    for {
        msg, _ := bufio.NewReader(port).ReadString('\n')
        var answ string
        if len(msg) < 5 {
            answ = "Wrong format";
        } else {
            if msg[:3] == "set" {
                word = msg[4:]
                if word[len(word) - 1] == '\n' {
                    word = word[:len(word) - 1]
                }
                answ = "OK"
            } else if msg[:3] == "has" {
                request := msg[4:]
                if request[len(request) - 1] == '\n' {
                    request = request[:len(request) - 1]
                }
                contain := strings.Index(word, request)
                if contain != -1 {
                    answ = "Contain from " + strconv.Itoa(contain)
                } else {
                    answ = "Not contain"
                }
            } else {
                answ = "Wrong format"
            }
        }
        port.Write([]byte(answ + "\n"))
    }
}

func main()  {
    listener, err := net.Listen("tcp", ":8010")
    if err != nil {
        log.Fatal("Unable to listen", err.Error())
    }
    for {
        port, err := listener.Accept()
        if err != nil {
            log.Println("Unable to open port", err.Error())
            continue
        }
        go handleClient(port)
    }
}
