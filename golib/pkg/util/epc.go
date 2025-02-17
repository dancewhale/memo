package util

import (
	"fmt"
	"net"
)

type NoteResult struct {
	Data []string
	Err  string
}

func QueryFreePort() (int64, error) {
	s, err := net.Listen("tcp", ":0")
	if err != nil {
		return -1, fmt.Errorf("could not listen TCP port 0: %v", err)
	}
	defer s.Close()
	tcpa, _ := s.Addr().(*net.TCPAddr)
	return int64(tcpa.Port), nil
}
