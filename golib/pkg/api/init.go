package api

import (
	card "memo/proto/grpc/memo/v1"

	"google.golang.org/grpc"
)

func ApiRegister(s *grpc.Server) {
	card.RegisterNoteServiceServer(s, &server{})
}
