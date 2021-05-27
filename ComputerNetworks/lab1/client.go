package main

import (
    "fmt"
    "github.com/jlaffaye/ftp"
    "log"
    "io/ioutil"
    "bytes"
    "path"
    "strings"
    "bufio"
    "os"
)

type client struct{
    server string
    login string
    password string
    conn *ftp.ServerConn
}

func (obj *client) CreateConnection() {
    conn, err := ftp.Dial(obj.server)
    obj.conn = conn
    if err != nil {
        log.Fatal("Connection: ", err)
    }
    err = obj.conn.Login(obj.login, obj.password)
    if err != nil {
        log.Fatal("Login: ", err)
    }
}

func (obj *client) CreateDir(path string) {
    err := obj.conn.MakeDir(path)
    if err != nil {
        log.Fatal("MakeDir: ", err)
    }
}

func (obj *client) Store(pathLocal, pathRemote string) {
    content, err := ioutil.ReadFile(pathLocal)
    if err != nil {
        log.Fatal("Read local file: ", err)
    }
    data := bytes.NewBuffer(content)
    err = obj.conn.Stor(pathRemote, data)
    if err != nil {
        log.Fatal("Stor: ", err)
    }
}

func (obj *client) ReadFile(pathRemote string) {
    _, file := path.Split(pathRemote)
    r, err := obj.conn.Retr(pathRemote)
    if err != nil {
        log.Fatal("Retr: ", err)
    }
    buf, err := ioutil.ReadAll(r)
    if err != nil {
        log.Fatal("ReadAll: ", err)
    }
    err = ioutil.WriteFile(file, buf, 0644)
    if err != nil {
        log.Fatal("WriteFile: ", err)
    }
    fmt.Println(string(buf))
    r.Close()
}

func (obj *client) ReadDir(dir string) {
    entry, err := obj.conn.List(dir)
    if err != nil {
        log.Fatal("List: ", err)
    }
    for _, el := range entry {
        fmt.Println(el.Name)
    }
}

func (obj *client) DeleteFile(pathRemote string) {
    err := obj.conn.Delete(pathRemote)
    if err != nil {
        log.Fatal("Delete: ", err)
    }
}

func (obj *client) DeleteDir(pathRemote string)  {
    err := obj.conn.RemoveDir(pathRemote)
    if err != nil {
        log.Fatal("RemoveDir: ", err)
    }
}

func main()  {
    var cl client
    fmt.Println("Enter server address: ")
    fmt.Scanf("%s", &cl.server)
    fmt.Println("Enter login: ")
    fmt.Scanf("%s", &cl.login)
    fmt.Println("Enter password: ")
    fmt.Scanf("%s", &cl.password)
    cl.CreateConnection()
    defer cl.conn.Quit()
    fmt.Println("Enter command: ")
    cmd := ""
    in := bufio.NewReader(os.Stdin)
    for cmd != "exit\n" {
        cmd, _ = in.ReadString('\n')
        comm := strings.Split(cmd, " ")
        comm[len(comm) - 1] = comm[len(comm) - 1][:len(comm[len(comm) - 1]) - 1]
        if comm[0] == "ls" {
            cl.ReadDir(string(comm[1]))
        } else if comm[0] == "rm" {
            cl.DeleteFile(string(comm[1]))
        } else if comm[0] == "rmdir" {
            cl.DeleteDir(string(comm[1]))
        } else if comm[0] == "cat" {
            cl.ReadFile(string(comm[1]))
        } else if comm[0] == "touch" {
            cl.Store(string(comm[1]), string(comm[2]))
        } else if comm[0] == "mkdir" {
            cl.CreateDir(string(comm[1]))
        }
    }
}
