package main

import (
  "log"
  "net/smtp"
  "io/ioutil"
  "fmt"
  "encoding/json"
  "bufio"
  "os"
  "strings"
)

type Config struct{
  Addr string
  Port string
  From string
}

func main()  {
  var pathToConfig string
  fmt.Println("Enter path to config:")
  fmt.Scanf("%s", &pathToConfig);
  file, err := ioutil.ReadFile(pathToConfig)
  if err != nil {
    log.Fatal("Error with reading config: ", err);
  }
  var config Config
  err = json.Unmarshal(file, &config)
  if err != nil {
    log.Fatal("Error with deserialisation config: ", err)
  }
  var passw string
  fmt.Print("Enter password for ", config.From, "\n")
  fmt.Scanf("%s", &passw)
  auth := smtp.PlainAuth("", config.From, passw, config.Addr)

  fmt.Println("Enter recipientes:")
  in := bufio.NewReader(os.Stdin)
  inp,_,_ := in.ReadLine()
  recipientes := strings.Split(string(inp), " ")

  message := ""
  fmt.Println("Enter title:")
  inp,_,_ = in.ReadLine()
  message += "Subject: " + string(inp) + "\r\n\r\n"
  fmt.Println("Enter message:")
  inp,_,_ = in.ReadLine()
  message += string(inp) + "\r\n"

  fmt.Println("Sending")
  err = smtp.SendMail(config.Addr + ":" + config.Port, auth, config.From, recipientes, []byte(message))
  if err != nil {
    log.Fatal("Error with sending mail: ", err)
  }
  fmt.Println("Sent")
}
