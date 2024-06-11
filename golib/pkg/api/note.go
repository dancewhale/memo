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
	fapi := &fsrs.FSRSApi{}

	fnote := fapi.GetNoteByOrgID(in.GetOrgid())
	return &pb.GetNoteResponse{Orgid: fnote.N.OrgID, Type: fnote.N.Type, Content: fnote.N.Content}, nil
}
	
func (s *noteServer) CreateNote(ctx context.Context, in *pb.CreateNoteRequest) (*pb.CreateNoteResponse, error) {
	// use for test function
	note := fsrs.Note{Content: in.GetContent(), Type: in.GetType(), OrgID: in.GetOrgid()}

	fapi := &fsrs.FSRSApi{}

	fnote := fapi.CreateNote(&note)

	return &pb.CreateNoteResponse{Orgid: fnote.N.OrgID }, nil
}
	

func (s *noteServer) RemoveNote(ctx context.Context, in *pb.DeleteNoteRequest) (*pb.DeleteNoteResponse, error) {
	// use for test function
	fapi := &fsrs.FSRSApi{}

	err := fapi.RemoveNote(in.GetOrgid())

	return &pb.DeleteNoteResponse{ErrMsg: err.Error()}, err
}
