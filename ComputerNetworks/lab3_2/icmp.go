package main

import (
  "github.com/go-ping/ping"
  "log"
  "fmt"
)

func main()  {
  var addr string
  fmt.Println("Enter address:")
  fmt.Scanf("%s", &addr)
  pinger, err := ping.NewPinger(addr)
  if err != nil {
    log.Fatal("Error with new pinger: ", err)
  }
  pinger.OnRecv = func(packet *ping.Packet) {
    fmt.Println("-----------------")
    fmt.Println("Bytes:", packet.Nbytes)
    fmt.Println("Address:", packet.Addr, "IP:", packet.IPAddr.IP)
    fmt.Println("Sequence:", packet.Seq)
    fmt.Println("Time To Live:", packet.Ttl)
    fmt.Println("Round-trip time:", packet.Rtt)
  }
  pinger.OnFinish = func(stat *ping.Statistics) {
    fmt.Println("Packets received:", stat.PacketsRecv)
    fmt.Println("Packets sent:", stat.PacketsSent)
    fmt.Print("Packets loss: ", stat.PacketLoss, "%\n")
  }
  pinger.SetPrivileged(true)
  pinger.Count = 4
  fmt.Println("Ping", pinger.Addr())
  err = pinger.Run()
  if err != nil {
    log.Fatal(err)
  }
}
