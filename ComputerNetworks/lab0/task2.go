package main

import (
	"fmt"
	"github.com/mmcdole/gofeed"
)

func main() {
	sites := [3]string{
		"http://habrahabr.ru/rss/best",
		"https://www.forbes.com/investing/feed2/",
		"http://www.fl.ru/rss/projects.xml",
	};
	for _, site := range sites {
		fmt.Println(site);
		fp := gofeed.NewParser();
		feed, err := fp.ParseURL(site);
		if err != nil {
			fmt.Println("Problem with ", site);
			return;
		}
		for _, el := range feed.Items {
			fmt.Println("Title: ", el.Title);
			fmt.Println("\nDescription: ", el.Description);
		}
	}
}

