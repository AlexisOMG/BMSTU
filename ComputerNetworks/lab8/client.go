package main

import (
    "net"
    "log"
    "bufio"
    "os"
    "fmt"
)

func main() {
    var address string
    fmt.Println("Enter address with port")
    fmt.Scanf("%s", &address)
    conn, err := net.Dial("tcp", address)
    if err != nil {
        log.Println("Unable to connect", err.Error())
    }
    for {
        cin := bufio.NewReader(os.Stdin)
        fmt.Print("Request: ")
        msg, _ := cin.ReadString('\n')
        if msg == "exit\n" {
            conn.Close()
            break
        }
        fmt.Fprintf(conn, msg)
        answ, _ := bufio.NewReader(conn).ReadString('\n')
        fmt.Print("Answer from server: " + answ)
    }
}
