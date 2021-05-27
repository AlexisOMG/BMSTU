package main

import (
	"fmt"
	"golang.org/x/crypto/ssh"
	"log"
  "bufio"
  "os"
)

func connect(server string, config *ssh.ClientConfig) *ssh.Session {
  client, err := ssh.Dial("tcp", server, config)
  if err != nil {
    log.Fatal("Error with dial: ", err)
  }
  session, err := client.NewSession()
  if err != nil {
    log.Fatal(err)
  }
  return session
}

func main()  {
  var login, password, server, server1 string
  fmt.Println("Enter server and port:")
  fmt.Scanf("%s", &server)
  fmt.Println("Enter login:")
  fmt.Scanf("%s", &login)
  fmt.Println("Enter password:")
  fmt.Scanf("%s", &password)
  config := &ssh.ClientConfig {
    User: login,
    Auth: []ssh.AuthMethod {
      ssh.Password(password),
    },
    HostKeyCallback: ssh.InsecureIgnoreHostKey(),
  }
  session := connect(server, config)
  defer session.Close()
  stdin, err := session.StdinPipe()
  if err != nil {
    log.Fatal(err)
  }

  session.Stdout = os.Stdout
  session.Stderr = os.Stderr

  err = session.Shell()
  if err != nil {
    log.Fatal(err)
  }

  fmt.Println("Enter another server:")
  fmt.Scanf("%s", &server1)

  session1 := connect(server1, config)
  if err != nil {
    log.Fatal(err)
  }
  defer session1.Close()
  stdin1, err := session1.StdinPipe()
  if err != nil {
    log.Fatal(err)
  }

  session1.Stdout = os.Stdout
  session1.Stderr = os.Stderr

  err = session1.Shell()
  if err != nil {
    log.Fatal(err)
  }

  cmd := ""
  fmt.Println("Write your commands, master")
  in := bufio.NewReader(os.Stdin)
  for {
    inp,_,_ := in.ReadLine()
    cmd = string(inp)
    if cmd == "exit" {
      break
    }
    go stdin.Write([]byte(cmd + "\n"))
    go stdin1.Write([]byte(cmd + "\n"))
  }
}
