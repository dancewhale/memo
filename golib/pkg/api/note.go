package api

import (
	"context"

	pb "memo/proto/grpc/memo/v1"
	"memo/pkg/fsrs"
	//gfsrs "memo/pkg/fsrs"

	//import dump
	//"github.com/spewerspew/spew"
	//import fsrs
	gfsrs "github.com/open-spaced-repetition/go-fsrs"
)


type noteServer struct {
	pb.UnimplementedNoteServiceServer
}

func (s *noteServer) GetNote(ctx context.Context, in *pb.GetNoteRequest) (*pb.GetNoteResponse, error) {
	// use for test function
	return &pb.GetNoteResponse{Orgid: "jjj" }, nil
}
	
func (s *noteServer) CreateNote(ctx context.Context, in *pb.CreateNoteRequest) (*pb.CreateNoteResponse, error) {
	// use for test function

	fnote := fsrs.FSRSNote{C: &gfsrs.Card{}, N: &fsrs.Note{Content: in.GetContent(), Type: in.GetType(), OrgID: in.GetOrgid()}}

	fnote.AddCard()

	return &pb.CreateNoteResponse{Orgid: "test for create card"}, nil
}
	
