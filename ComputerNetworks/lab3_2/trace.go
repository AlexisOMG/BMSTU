package main

import (
  "fmt"
  "github.com/lixiangzhong/traceroute"
  "log"
)

func main()  {
  var addr string
  fmt.Println("Enter address:")
  fmt.Scanf("%s", &addr)
  trace := traceroute.New(addr);
  trace.MaxTTL = 32
  res, err := trace.Do()
  if (err != nil) {
    log.Fatal("Cannot trace: ", err)
  }
  for _, info := range res {
    fmt.Println(info)
  }
}
