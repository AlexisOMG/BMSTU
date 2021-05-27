package main

import (
  "github.com/gliderlabs/ssh"
  "os/exec"
  "strings"
  "log"
  "bufio"
  "io"
  "fmt"
)

func main()  {
  log.Fatal(ssh.ListenAndServe(":2222",
    func(s ssh.Session)  {
      log.Println("Terminal:")
      for {
        line, err := bufio.NewReader(s).ReadString('\n')
        if err != nil {
          break
        }
    		if len(line) > 0 && line[len(line) - 1] == '\n' {
    			line = line[:len(line) - 1]
    		}
        in := strings.Split(line, " ")
		    log.Println(in)
        if in[0] == "" {
          continue
        }
        if in[0] == "exit" {
          break
        }
        exe := exec.Command(in[0], in[1:]...)
        out, err := exe.Output()
        if err != nil {
          log.Println(err)
        }
        io.WriteString(s, fmt.Sprintf("\nServer2:\n%s", out))
      }
      log.Println("session closed")
    },
    ssh.PasswordAuth(func(context ssh.Context, password string) bool {
      return context.User() == "alexis" && password == "ggwp"
    }),
  ))
}
