package api

import (
	"context"

	pb "memo/proto/grpc/memo/v1"
	"memo/pkg/fsrs"
	"memo/pkg/storage"

	//"github.com/spewerspew/spew"
)


type noteServer struct {
	pb.UnimplementedNoteServiceServer
}

var fapi = fsrs.NewFsrsApi()

func (s *noteServer) GetNote(ctx context.Context, in *pb.GetNoteRequest) (*pb.GetNoteResponse, error) {
	// use for test function

	fnote := fapi.GetNoteByOrgID(in.GetOrgid())
	if fnote != nil {
		return &pb.GetNoteResponse{Orgid: fnote.Orgid, Type: fnote.Type, Content: fnote.Content}, nil
	} else {
		return &pb.GetNoteResponse{Orgid: "", Type: "", Content: ""}, nil
	}
}
	
func (s *noteServer) CreateNote(ctx context.Context, in *pb.CreateNoteRequest) (*pb.CreateNoteResponse, error) {
	// use for test function
	note := storage.Note{Content: in.GetContent(), Type: in.GetType(), Orgid: in.GetOrgid()}

	fnote := fapi.CreateNote(&note)

	if fnote != nil {
		return &pb.CreateNoteResponse{Orgid: fnote.Orgid }, nil
	} else {
		return &pb.CreateNoteResponse{Orgid: "" }, nil
	}
}
	

func (s *noteServer) RemoveNote(ctx context.Context, in *pb.DeleteNoteRequest) (*pb.DeleteNoteResponse, error) {
	// use for test function

	err := fapi.RemoveNote(in.GetOrgid())
	if err != nil {
		return &pb.DeleteNoteResponse{Result: "failed"}, nil
	} else {
		return &pb.DeleteNoteResponse{Result: "success"}, nil
	}
}

func (s *noteServer) ReviewNote(ctx context.Context, in *pb.ReviewNoteRequest) (*pb.ReviewNoteResponse, error) {
	// use for test function
	orgid := in.GetOrgid()
	rate := fsrs.Rate(in.GetRate())

	err := fapi.ReviewNote(orgid, rate)
	if err != nil {
		return &pb.ReviewNoteResponse{Orgid: orgid}, nil
	} else {
		return &pb.ReviewNoteResponse{Orgid: orgid}, nil
	}
}
