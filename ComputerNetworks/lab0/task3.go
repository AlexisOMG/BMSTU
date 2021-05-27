package main

import (
    "fmt"
    "net/http"
    "log"
    "sort"
    "github.com/mmcdole/gofeed"
    "io/ioutil"
)



func makeHtml(feed *gofeed.Feed) string {
    answ := "";
    for _, el := range feed.Items {
        answ += "<h2>" + el.Title + "</h2>";
        answ += "<p><a href=\"" + el.Link + "\">" + el.Published + "</a></p>";
        answ += "<p>" + el.Description + "</p><hr>";
    }

    return answ;
}

func HomeRouterHandler(res http.ResponseWriter, req *http.Request) {
    html, err := ioutil.ReadFile("templates/menu.html");
    var obj []*gofeed.Item;
    if err != nil {
      log.Fatal("Main: ", err);
      res.Write([]byte("Cannot load feed"));
    }
    sites := [3]string{
  		"http://habrahabr.ru/rss/best",
  		"https://www.forbes.com/investing/feed2/",
  		"http://www.fl.ru/rss/projects.xml",
  	};
    for _, site := range sites {
        fp := gofeed.NewParser();
        feed, err := fp.ParseURL(site);
    		if err != nil {
      			fmt.Println("Problem with ", site);
      			return;
    		}
        obj = append(obj, feed.Items...);
    }
    html = append(html, []byte("<br>")...);
    sort.Slice(obj, func(i, j int) bool {
        return !obj[i].PublishedParsed.Before(*obj[j].PublishedParsed);
    });
    for _, el := range obj {
        html = append(html, []byte("<h2>" + el.Title + "</h2>")...);
        html = append(html, []byte("<p><a href=\"" + el.Link + "\">" + el.Published + "</a></p>")...);
        html = append(html, []byte("<p>" + el.Description + "</p><hr>")...);
    }
    res.Write([]byte(html));
}

func handleHabr(res http.ResponseWriter, req *http.Request) {
    fp := gofeed.NewParser();
    feed, err := fp.ParseURL("http://habrahabr.ru/rss/best");
    if err != nil {
        log.Fatal("Habr: ", err);
        res.Write([]byte("Cannot load feed"));
    } else {
        answ := `<html>
                  <head>
                    <title>HabrBest</title>
                  </head>
                  <body>
                    <h1 align="center">Habr Best</h1>`;
        for _, el := range feed.Items {
            answ += "<h2>" + el.Title + "</h2>";
            answ += "<p>" + el.Published + "</p>";
            answ += "<p>" + el.Description + "</p><hr>";
        }
        answ += "</body></html>";
        res.Write([]byte(answ));
    }
}

func handleForbes(res http.ResponseWriter, req *http.Request) {
    fp := gofeed.NewParser();
    feed, err := fp.ParseURL("https://www.forbes.com/investing/feed2/");
    if err != nil {
        log.Fatal("Forbes: ", err);
        res.Write([]byte("Cannot load feed"));
    } else {
        answ := `<html>
                  <head>
                    <title>Forbes</title>
                  </head>
                  <body>
                    <h1 align="center">Forbes</h1>`;
        answ += makeHtml(feed);
        answ += "</body></html>";
        res.Write([]byte(answ));
    }
}

func handleFl(res http.ResponseWriter, req *http.Request) {
  fp := gofeed.NewParser();
  feed, err := fp.ParseURL("http://www.fl.ru/rss/projects.xml");
  if err != nil {
      log.Fatal("FL: ", err);
      res.Write([]byte("Cannot load feed"));
  } else {
      answ := `<html>
                <head>
                  <title>FreeLance</title>
                </head>
                <body>
                  <h1 align="center">FreeLance</h1>`;
      answ += makeHtml(feed);
      answ += "</body></html>";
      res.Write([]byte(answ));
  }
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
