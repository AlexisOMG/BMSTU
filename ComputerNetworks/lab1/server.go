package main

import (
    "goftp.io/server/core"
    "goftp.io/server/driver/file"
    "log"
)

func main()  {
    factory := &file.DriverFactory {
        RootPath: "./server",
        Perm: core.NewSimplePerm("alexis", "alexis"),
    }
    auth := &core.SimpleAuth {
        Name: "alexis",
        Password: "alexis",
    }
    opts := &core.ServerOpts {
        Auth: auth,
        Factory: factory,
        Port: 4010,
    }
    server := core.NewServer(opts)
    err := server.ListenAndServe()
    if err != nil {
        log.Fatal("Run server: ", err)
    }
}
