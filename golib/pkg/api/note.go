package api

import (
	"context"

	pb "memo/proto/grpc/memo/v1"
	"memo/pkg/fsrs"

	//import dump
	//"github.com/spewerspew/spew"
)


type noteServer struct {
	pb.UnimplementedNoteServiceServer
}

func (s *noteServer) GetNote(ctx context.Context, in *pb.GetNoteRequest) (*pb.GetNoteResponse, error) {
	// use for test function
	fnote := &fsrs.FSRSNote{}

	fnote = fnote.GetNoteByOrgID(in.GetOrgid())
	return &pb.GetNoteResponse{Orgid: fnote.N.OrgID, Type: fnote.N.Type, Content: fnote.N.Content}, nil
}
	
func (s *noteServer) CreateNote(ctx context.Context, in *pb.CreateNoteRequest) (*pb.CreateNoteResponse, error) {
	// use for test function
	note := fsrs.Note{Content: in.GetContent(), Type: in.GetType(), OrgID: in.GetOrgid()}

	fnote := &fsrs.FSRSNote{}

	fnote = fnote.CreateNote(&note)

	return &pb.CreateNoteResponse{Orgid: fnote.N.OrgID }, nil
}
	
