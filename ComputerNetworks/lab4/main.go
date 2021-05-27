package main

import (
  "golang.org/x/net/html"
  "net/http"
  "log"
  "strings"
  "html/template"
)

const site = "https://kod.ru"
const feed = site + "/tag/news"

type item struct{
  Text string
  Href string
  ImgSrc string
}

func isNeeded(node *html.Node, class string) bool {
  var names []string
  for _, attr := range node.Attr {
    if attr.Key == "class" {
      names = strings.Split(attr.Val, " ")
      break
    }
  }
  for _, name := range names {
    if name == class {
      return true
    }
  }
  return false
}


func getElementsByClass(node *html.Node, class string) []*html.Node {
  var elements []*html.Node
  if isNeeded(node, class) {
    elements = append(elements, node)
  }
  for child := node.FirstChild; child != nil; child = child.NextSibling {
    elements = append(elements, getElementsByClass(child, class)...)
  }
  return elements
}


func getElements(url string) []item {
  resp, err := http.Get(url)
  if err != nil {
    log.Fatal("Cannot get html: ", err)
  }
  node, err := html.Parse(resp.Body)
  if err != nil {
    log.Fatal("Cannot parse html: ", err)
  }
  elements := getElementsByClass(node, "post-card__image")
  var allElements []item
  for _, elem := range elements {
    var itm item
    for _, attr := range elem.Attr {
      if attr.Key == "href" {
        itm.Href = site + attr.Val
      } else if attr.Key == "title" {
        itm.Text = attr.Val
      }
    }
    img := getElementsByClass(elem, "lazy")[0]
    for _, attr := range img.Attr {
      if attr.Key == "data-srcset" {
        sizes := strings.Split(attr.Val, ",")
        size := strings.Split(sizes[len(sizes) - 1], " ")
        size[0] = size[0][1:]
        itm.ImgSrc = site + size[0]
      }
    }
    allElements = append(allElements, itm)
  }
  return allElements
}

func main()  {
  tmpl, err := template.ParseFiles("index.html")
  if err != nil {
    log.Fatal("Unable to open html: ", err.Error())
  }
  http.HandleFunc("/", func(resp http.ResponseWriter, req *http.Request) {
    items := getElements(feed)
    err = tmpl.Execute(resp, map[string] interface{} {
      "items": items,
    })
    if err != nil {
      log.Fatal("Unable to execute: ", err.Error())
    }
  })
  http.ListenAndServe(":4010", nil)
}
