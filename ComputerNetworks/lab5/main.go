package main

import (
    "golang.org/x/net/html"
    "net/http"
    "log"
    "strings"
    "html/template"
)

const site = "https://bmstu.ru"
const feed = site + "/mstu/info/bauman-news/"

type article struct {
    Text template.HTML
    HasText bool
    Title string
    HasTitle bool
    Href string
    ImgSrc string
    HasImg bool
}

type item struct{
  Text string
  HasText bool
  Title string
  HasTitle bool
  Href string
  ImgSrc string
  HasImg bool
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

func getText(node *html.Node) string {
    s := ""
    if node == nil {
        return s
    }
    for el := node; el != nil; el = el.NextSibling {
        if el.FirstChild != nil {
            s1 := getText(el.FirstChild)
            if s1 != "" {
                s += " " + s1
            }
        } else if el.Data != "" {
            s += " " + el.Data
        }
    }
    return s
}

func parseArticle(url string) []item {
    resp, err := http.Get(url)
    if err != nil {
      log.Fatal("Cannot get html: ", err)
    }
    node, err := html.Parse(resp.Body)
    if err != nil {
      log.Fatal("Cannot parse html: ", err)
    }
    var itm item
    itm.HasImg = false
    itm.HasTitle = false
    itm.HasText = false
    itm.Text = ""
    if len(getElementsByClass(node, "img-responsive")) > 1 {
        for _, attr := range getElementsByClass(node, "img-responsive")[1].Attr {
            if attr.Key == "src" {
                itm.ImgSrc = site + attr.Val
                itm.HasImg = true
            }
        }
    }
    if len(getElementsByClass(node, "j-marg-top-md")) > 0 {
        itm.Text = "<h2>" + getElementsByClass(node, "j-marg-top-md")[0].FirstChild.Data + "</h2>"
        itm.HasText = true
    }
    if len(getElementsByClass(node, "b-newsdetail-text")) > 0 {
        for child := getElementsByClass(node, "b-newsdetail-text")[0].FirstChild; child != nil; child = child.NextSibling {
            if child != nil && child.FirstChild != nil {
                itm.Text += "<p>"
                s := getText(child.FirstChild)
                if s != "" {
                    s = s[:len(s) - 3]
                    itm.Text += s
                }
                itm.Text += "</p>"
            }
        }
    }
    return []item{itm}
}

func parseFeed(url string) []item {
  resp, err := http.Get(url)
  if err != nil {
    log.Fatal("Cannot get html: ", err)
  }
  node, err := html.Parse(resp.Body)
  if err != nil {
    log.Fatal("Cannot parse html: ", err)
  }
  elements := getElementsByClass(node, "j-padd-bottom-sm")
  var allElements []item
  for _, elem := range elements {
    var itm item
    itm.HasImg = false
    itm.HasText = false
    itm.HasTitle = false
    if len(getElementsByClass(elem, "j-padd-right")) > 0 {
        img := getElementsByClass(elem, "j-padd-right")[0]
        if img.FirstChild != nil && img.FirstChild.FirstChild != nil {
            for _, attr := range img.FirstChild.FirstChild.Attr {
                if attr.Key == "src" {
                    itm.ImgSrc = site + attr.Val
                    itm.HasImg = true
                }
            }
        }
    }
    text := getElementsByClass(elem, "j-bold")[0].FirstChild.FirstChild
    for _, attr := range text.Attr {
        if attr.Key == "href" {
            itm.Href = site + attr.Val
        }
    }
    itm.Title = text.FirstChild.Data
    itm.HasTitle = true
    allElements = append(allElements, itm)
  }
  return allElements
}

func main()  {
    http.HandleFunc("/article", func(resp http.ResponseWriter, req *http.Request)  {
        url := req.URL.Query().Get("url")
        tmpl, err := template.ParseFiles("index.html")
        if err != nil {
          log.Fatal("Unable to open html: ", err.Error())
        }
        items := parseArticle(url)
        htmlItems := []article{article{
            Text: template.HTML(items[0].Text),
            HasText: items[0].HasText,
            Title: items[0].Title,
            HasTitle: items[0].HasTitle,
            Href: items[0].Href,
            ImgSrc: items[0].ImgSrc,
            HasImg: items[0].HasImg,
        }}
        err = tmpl.Execute(resp, map[string] interface{} {
        "items": htmlItems,
        })
        if err != nil {
        log.Fatal("Unable to execute: ", err.Error())
        }
    })
    http.HandleFunc("/", func(resp http.ResponseWriter, req *http.Request) {
        tmpl, err := template.ParseFiles("index.html")
        if err != nil {
          log.Fatal("Unable to open html: ", err.Error())
        }
        items := parseFeed(feed)
        err = tmpl.Execute(resp, map[string] interface{} {
        "items": items,
        })
        if err != nil {
        log.Fatal("Unable to execute: ", err.Error())
        }
    })
    http.ListenAndServe(":5010", nil)
}
