package api

import (
	"context"

	pb "memo/proto/grpc/memo/v1"
	"memo/pkg/fsrs"
	"memo/pkg/storage"

	//import dump
	//"github.com/spewerspew/spew"
)


type noteServer struct {
	pb.UnimplementedNoteServiceServer
}

func (s *noteServer) GetNote(ctx context.Context, in *pb.GetNoteRequest) (*pb.GetNoteResponse, error) {
	// use for test function
	fapi := fsrs.NewFsrsApi()

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

	fapi := fsrs.NewFsrsApi()

	fnote := fapi.CreateNote(&note)

	if fnote != nil {
		return &pb.CreateNoteResponse{Orgid: fnote.Orgid }, nil
	} else {
		return &pb.CreateNoteResponse{Orgid: "" }, nil
	}
}
	

func (s *noteServer) RemoveNote(ctx context.Context, in *pb.DeleteNoteRequest) (*pb.DeleteNoteResponse, error) {
	// use for test function
	fapi := fsrs.NewFsrsApi()

	err := fapi.RemoveNote(in.GetOrgid())
	if err != nil {
		return &pb.DeleteNoteResponse{ErrMsg: err.Error()}, err
	} else {
		return &pb.DeleteNoteResponse{ErrMsg: ""}, nil
	}
}

func (s *noteServer) ReviewNote(ctx context.Context, in *pb.ReviewNoteRequest) (*pb.ReviewNoteResponse, error) {
	// use for test function
	fapi := fsrs.NewFsrsApi()
	orgid := in.GetOrgid()
	rate := fsrs.Rating(in.GetRate())

	err := fapi.ReviewNote(orgid, rate)
	if err != nil {
		return &pb.ReviewNoteResponse{Orgid: orgid}, err
	} else {
		return &pb.ReviewNoteResponse{Orgid: orgid}, nil
	}
}
