package main

import (
    "fmt"
    "net/http"
    "log"
    "path/filepath"
    "html/template"
)

func HomeRouterHandler(res http.ResponseWriter, req *http.Request) {
    req.ParseForm();
    fmt.Println("form: ", req.Form);
    fmt.Println("path: ", req.URL.Path);
    fmt.Println("scheme: ", req.URL.Scheme);
    fmt.Println(req.Form["url_long"]);
    for key, value := range req.Form {
        fmt.Println("key: ", key, " -> value: ", value);
    }

    path := filepath.Join("templates", "menu.html");
    tmpl, err := template.ParseFiles(path);
    if err != nil {
      log.Fatal("Template: ", err);
      fmt.Fprintf(res, "400");
      return;
    }
    err = tmpl.Execute(res, nil);
    if err != nil {
      log.Fatal("Execute: ", err);
      fmt.Fprintf(res, "400");
      return;
    }

}

func handleHabr(res http.ResponseWriter, req *http.Request) {
    res.Write([]byte("Habr Best"));
}

func handleForbes(res http.ResponseWriter, req *http.Request) {
    res.Write([]byte("Forbes Investing"));
}

func handleFl(res http.ResponseWriter, req *http.Request) {
    res.Write([]byte("FL projects"));
}

func main() {
    http.HandleFunc("/", HomeRouterHandler);
    http.HandleFunc("/habr", handleHabr);
    http.HandleFunc("/forbes", handleForbes);
    http.HandleFunc("/fl", handleFl);
    err := http.ListenAndServe(":3010", nil);
    if err != nil {
        log.Fatal("ListenAndServe: ", err);
    }
}

