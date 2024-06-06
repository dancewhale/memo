package api

import (
	"context"

	pb "memo/proto/grpc/memo/v1"
	"memo/pkg/storage"
	"memo/pkg/storage/dal"

	//import dump
	"github.com/spewerspew/spew"
)

var DB = storage.InitDBEngine()

type noteServer struct {
	pb.UnimplementedNoteServiceServer
}

func (s *noteServer) GetNote(ctx context.Context, in *pb.GetNoteRequest) (*pb.GetNoteResponse, error) {
	// use for test function
	n := dal.Use(DB).Note

	noteData, err := n.WithContext(ctx).FindByOrgID(in.GetOrgid())
	if err != nil {
		return nil, err
	}

	return &pb.GetNoteResponse{Orgid: noteData.Orgid }, nil
}
	
func (s *noteServer) CreateNote(ctx context.Context, in *pb.CreateNoteRequest) (*pb.CreateNoteResponse, error) {
	// use for test function
	n := dal.Use(DB).Note

	noteData := storage.Note{
		Orgid: in.GetOrgid(),
		Type: in.GetType(),
		Content: in.GetContent(),
	}

	spew.Dump(&noteData)

	err := n.WithContext(ctx).Create(&noteData)
	if err != nil {
		return nil, err
	}


	return &pb.CreateNoteResponse{Orgid: noteData.Orgid}, nil
}
	
