package main

import (
    "net/http"
    "fmt"
    "io"
)

func httpProxy(resp http.ResponseWriter, req *http.Request) {
    ans, err := http.DefaultTransport.RoundTrip(req)
    if err != nil {
        http.Error(resp, err.Error(), http.StatusServiceUnavailable)
        return
    }
    defer ans.Body.Close()
    for i, hs := range ans.Header {
        for _, h := range hs {
            resp.Header().Add(i, h)
        }
    }
    fmt.Println(ans.StatusCode, req.Method, req.URL)
    resp.WriteHeader(ans.StatusCode)
    io.Copy(resp, ans.Body)
}

func main() {
    http.HandleFunc("/", func(resp http.ResponseWriter, req *http.Request) {
        httpProxy(resp, req)
    })
    http.ListenAndServe(":6010", nil)
}
