package main

import (
    "golang.org/x/net/html"
    "net/http"
    "log"
    "strings"
    "html/template"
    "net/url"
    "io"
    "bytes"
)
const port = ":7010"
const mirrorUrl = "http://localhost" + port + "/template/"


func getAttrValue(node *html.Node, key string) string {
	for _, attr := range node.Attr {
		if attr.Key == key {
			return attr.Val
		}
	}
	return ""
}

func setAttrValue(node *html.Node, key string, val string) {
	for _, attr := range node.Attr {
		if attr.Key == key {
			attr.Val = val
		}
	}
}

func replaceLinks(node *html.Node, site string) {
    for i, attr := range node.Attr {
        if (attr.Key == "href" || attr.Key == "src") && attr.Val != "" {
            attr.Val = strings.TrimSpace(attr.Val)
            u, err := url.Parse(attr.Val)
            if err != nil {
                log.Println("Unable to parse url:", attr.Val, err.Error())
            } else {
                if u.IsAbs() {
                    attr.Val = mirrorUrl + attr.Val[strings.Index(attr.Val, "://") + 3:]
                } else if len(attr.Val) > 2 && attr.Val[:2] == "//" {
                    attr.Val = mirrorUrl + attr.Val[2:]
                } else if attr.Val[0] == '/' {
                    attr.Val = mirrorUrl + site + attr.Val
                } else {
                    attr.Val = mirrorUrl + site + "/" + attr.Val
                }
            }
            node.Attr[i].Val = attr.Val
        }
    }
    for child := node.FirstChild; child != nil; child = child.NextSibling {
        replaceLinks(child, site)
    }
}

func handleLinks(link string, resp http.ResponseWriter) bool {
    ans, err := http.Get(link)
    if err != nil {
        log.Println("Unsuccessful GET request to ", link, ":", err.Error())
        return false
    }
    defer ans.Body.Close()
    contentType := ans.Header.Get("Content-type")
    for i, hs := range ans.Header {
        if i != "Content-Security-Policy" {
            for _, h := range hs {
                resp.Header().Add(i, h)
            }
        }
    }
    resp.WriteHeader(ans.StatusCode)
    if strings.Contains(contentType, "text/html") {
        node, err := html.Parse(ans.Body)
        if err != nil {
            log.Println("Unable to parse html:", err.Error())
            return false
        }
        parts := strings.Split(link, "/")
        site := ""
        if len(parts) >= 3 {
            site = parts[2]
        }
        replaceLinks(node, site)
        var buff bytes.Buffer
        html.Render(&buff, node)
        resp.Write(buff.Bytes())
    } else {
        io.Copy(resp, ans.Body)
    }
    return true
}


func main()  {
    http.HandleFunc("/template/", func(resp http.ResponseWriter, req *http.Request) {
        link := "http://" + req.URL.String()[10:]
        if handleLinks(link, resp) == false {
            link = "https://" + req.URL.String()[10:]
            if handleLinks(link, resp) == false {
                log.Println("Unable to serve", link)
            } else {
                log.Println("GET:", link)
            }
        } else {
            log.Println("GET:", link)
        }
    })
    http.HandleFunc("/", func(resp http.ResponseWriter, req *http.Request) {
        tmpl, err := template.ParseFiles("index.html")
        if err != nil {
          log.Fatal("Unable to open html: ", err.Error())
        }
        err = tmpl.Execute(resp, map[string] interface{} {
            "mirrorUrl": mirrorUrl,
        })
        if err != nil {
            log.Fatal("Unable to execute: ", err.Error())
        }
    })
    http.ListenAndServe(port, nil)
}
