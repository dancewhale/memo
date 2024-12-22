package index

import (
	"fmt"
	"github.com/blevesearch/bleve"
)

func Index() {
	message := struct {
		Id   string
		From string
		Body string
	}{
		Id:   "example1",
		From: "marty.schoch@gmail.com",
		Body: "bleve test for ok indexing is easy",
	}

	//mapping := bleve.NewIndexMapping()
	//indexB, err := bleve.New("test", mapping)
	indexB, err := bleve.Open("test")
	if err != nil {
		panic(err)
	}
	indexB.Index(message.Id, message)
	query := bleve.NewQueryStringQuery("example")
	searchRequest := bleve.NewSearchRequest(query)
	searchRequest.Fields = []string{"Id", "From", "Body"}
	searchResult, _ := indexB.Search(searchRequest)
	for _, hit := range searchResult.Hits {
		fmt.Println(hit.ID)
		fmt.Println(hit.Score)
	}
}
