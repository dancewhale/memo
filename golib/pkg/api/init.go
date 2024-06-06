package api

import (
	pb "memo/proto/grpc/memo/v1"

	"google.golang.org/grpc"
)

func ApiRegister(s *grpc.Server) {
	pb.RegisterNoteServiceServer(s, &noteServer{})
}
