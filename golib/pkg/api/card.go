package api

import (
	"context"

	note "memo/proto/grpc/memo/v1"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	"github.com/jinzhu/copier"
)

var DB = storage.InitDBEngine()

type server struct {
	note.UnimplementedNoteServiceServer
}

func (s *server) GetNote(ctx context.Context, in *note.GetNoteRequest) (*note.GetNoteResponse, error) {
	// use for test function
	n := dal.Use(DB).Note

	noteData, err := n.WithContext(ctx).FindByOrgID(in.GetOrgid())
	if err != nil {
		return nil, err
	}

	response := note.GetNoteResponse{}
	copier.Copy(&noteData, &response)


	return &response, nil
}
	
func (s *server) CreateNote(ctx context.Context, in *note.CreateNoteRequest) (*note.CreateNoteResponse, error) {
	// use for test function
	n := dal.Use(DB).Note

	noteData := storage.Note{}
	copier.Copy(&in, &noteData)

	err := n.WithContext(ctx).Create(&noteData)
	if err != nil {
		return nil, err
	}


	return &note.CreateNoteResponse{Orgid: noteData.Orgid}, nil
}
	
