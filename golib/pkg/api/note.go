package api

import (
	"context"

	pb "memo/proto/grpc/memo/v1"
	"memo/pkg/fsrs"
	"memo/pkg/storage"
	"memo/pkg/logger"

	"google.golang.org/grpc"
	//"github.com/spewerspew/spew"
)

func ApiRegister(s *grpc.Server) {
	pb.RegisterNoteServiceServer(s, &noteServer{})
}

type noteServer struct {
	pb.UnimplementedNoteServiceServer
}

var napi = fsrs.NewNoteApi()

func (s *noteServer) GetNote(ctx context.Context, in *pb.GetNoteRequest) (*pb.GetNoteResponse, error) {
	// use for test function

	fnote := napi.GetNoteByOrgID(in.GetOrgid())
	if fnote != nil {
		return &pb.GetNoteResponse{Orgid: fnote.Orgid, Type: fnote.Type, Content: fnote.Content}, nil
	} else {
		return &pb.GetNoteResponse{Orgid: "", Type: "", Content: ""}, nil
	}
}
	
func (s *noteServer) CreateNote(ctx context.Context, in *pb.CreateNoteRequest) (*pb.CreateNoteResponse, error) {
	// use for test function
	note := storage.Note{Content: in.GetContent(), Type: in.GetType(), Orgid: in.GetOrgid()}

	fnote := napi.CreateNote(&note)

	if fnote != nil {
		return &pb.CreateNoteResponse{Orgid: fnote.Orgid }, nil
	} else {
		return &pb.CreateNoteResponse{Orgid: "" }, nil
	}
}
	

func (s *noteServer) RemoveNote(ctx context.Context, in *pb.DeleteNoteRequest) (*pb.DeleteNoteResponse, error) {
	// use for test function

	err := napi.RemoveNote(in.GetOrgid())
	if err != nil {
		return &pb.DeleteNoteResponse{Result: "failed"}, nil
	} else {
		return &pb.DeleteNoteResponse{Result: "success"}, nil
	}
}

func (s *noteServer) ReviewNote(ctx context.Context, in *pb.ReviewNoteRequest) (*pb.ReviewNoteResponse, error) {
	orgid := in.GetOrgid()
	rate := storage.Rate(in.GetRate())
	logger.Debugf("ReviewNote: orgid: %s, input rate: %s , rate: %s", orgid, in.GetRate(), rate)

	err := napi.ReviewNote(orgid, rate)

	if err != nil {
		return &pb.ReviewNoteResponse{Orgid: orgid}, nil
	} else {
		return &pb.ReviewNoteResponse{Orgid: orgid}, nil
	}
}


func (s *noteServer) DueNotes(ctx context.Context, in *pb.DueNotesRequest) (*pb.DueNotesResponse, error) {
	dueDay := in.GetDay()
	notes := napi.DueNotes(dueDay)

	noteOrgIdList := []string{}

	for _, note := range notes {
		noteOrgIdList = append(noteOrgIdList, note.Orgid)
	}
		
	return &pb.DueNotesResponse{Orgid: noteOrgIdList}, nil
}

func (s *noteServer) InitDueNotes(ctx context.Context, in *pb.InitDueNotesRequest) (*pb.MessageResponse, error) {
	dueDay := in.GetDay()
	napi.InitTodayDueNotes(int(dueDay))

	return &pb.MessageResponse{Message: "Success"}, nil
}
