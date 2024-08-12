// Code generated by protoc-gen-go-grpc. DO NOT EDIT.
// versions:
// - protoc-gen-go-grpc v1.5.1
// - protoc             (unknown)
// source: memo/v1/notes.proto

package note

import (
	context "context"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
)

// This is a compile-time assertion to ensure that this generated file
// is compatible with the grpc package it is being compiled against.
// Requires gRPC-Go v1.64.0 or later.
const _ = grpc.SupportPackageIsVersion9

const (
	NoteService_GetNote_FullMethodName      = "/memo.v1.NoteService/GetNote"
	NoteService_CreateNote_FullMethodName   = "/memo.v1.NoteService/CreateNote"
	NoteService_RemoveNote_FullMethodName   = "/memo.v1.NoteService/RemoveNote"
	NoteService_ReviewNote_FullMethodName   = "/memo.v1.NoteService/ReviewNote"
	NoteService_DueNotes_FullMethodName     = "/memo.v1.NoteService/DueNotes"
	NoteService_InitDueNotes_FullMethodName = "/memo.v1.NoteService/InitDueNotes"
	NoteService_GetCard_FullMethodName      = "/memo.v1.NoteService/GetCard"
	NoteService_ReviewCard_FullMethodName   = "/memo.v1.NoteService/ReviewCard"
)

// NoteServiceClient is the client API for NoteService service.
//
// For semantics around ctx use and closing/ending streaming RPCs, please refer to https://pkg.go.dev/google.golang.org/grpc/?tab=doc#ClientConn.NewStream.
type NoteServiceClient interface {
	GetNote(ctx context.Context, in *GetNoteRequest, opts ...grpc.CallOption) (*GetNoteResponse, error)
	CreateNote(ctx context.Context, in *CreateNoteRequest, opts ...grpc.CallOption) (*CreateNoteResponse, error)
	RemoveNote(ctx context.Context, in *DeleteNoteRequest, opts ...grpc.CallOption) (*DeleteNoteResponse, error)
	ReviewNote(ctx context.Context, in *ReviewNoteRequest, opts ...grpc.CallOption) (*ReviewNoteResponse, error)
	DueNotes(ctx context.Context, in *DueNotesRequest, opts ...grpc.CallOption) (*DueNotesResponse, error)
	InitDueNotes(ctx context.Context, in *InitDueNotesRequest, opts ...grpc.CallOption) (*MessageResponse, error)
	GetCard(ctx context.Context, in *GetCardRequest, opts ...grpc.CallOption) (*GetCardResponse, error)
	ReviewCard(ctx context.Context, in *ReviewCardRequest, opts ...grpc.CallOption) (*ReviewCardResponse, error)
}

type noteServiceClient struct {
	cc grpc.ClientConnInterface
}

func NewNoteServiceClient(cc grpc.ClientConnInterface) NoteServiceClient {
	return &noteServiceClient{cc}
}

func (c *noteServiceClient) GetNote(ctx context.Context, in *GetNoteRequest, opts ...grpc.CallOption) (*GetNoteResponse, error) {
	cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
	out := new(GetNoteResponse)
	err := c.cc.Invoke(ctx, NoteService_GetNote_FullMethodName, in, out, cOpts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *noteServiceClient) CreateNote(ctx context.Context, in *CreateNoteRequest, opts ...grpc.CallOption) (*CreateNoteResponse, error) {
	cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
	out := new(CreateNoteResponse)
	err := c.cc.Invoke(ctx, NoteService_CreateNote_FullMethodName, in, out, cOpts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *noteServiceClient) RemoveNote(ctx context.Context, in *DeleteNoteRequest, opts ...grpc.CallOption) (*DeleteNoteResponse, error) {
	cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
	out := new(DeleteNoteResponse)
	err := c.cc.Invoke(ctx, NoteService_RemoveNote_FullMethodName, in, out, cOpts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *noteServiceClient) ReviewNote(ctx context.Context, in *ReviewNoteRequest, opts ...grpc.CallOption) (*ReviewNoteResponse, error) {
	cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
	out := new(ReviewNoteResponse)
	err := c.cc.Invoke(ctx, NoteService_ReviewNote_FullMethodName, in, out, cOpts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *noteServiceClient) DueNotes(ctx context.Context, in *DueNotesRequest, opts ...grpc.CallOption) (*DueNotesResponse, error) {
	cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
	out := new(DueNotesResponse)
	err := c.cc.Invoke(ctx, NoteService_DueNotes_FullMethodName, in, out, cOpts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *noteServiceClient) InitDueNotes(ctx context.Context, in *InitDueNotesRequest, opts ...grpc.CallOption) (*MessageResponse, error) {
	cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
	out := new(MessageResponse)
	err := c.cc.Invoke(ctx, NoteService_InitDueNotes_FullMethodName, in, out, cOpts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *noteServiceClient) GetCard(ctx context.Context, in *GetCardRequest, opts ...grpc.CallOption) (*GetCardResponse, error) {
	cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
	out := new(GetCardResponse)
	err := c.cc.Invoke(ctx, NoteService_GetCard_FullMethodName, in, out, cOpts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *noteServiceClient) ReviewCard(ctx context.Context, in *ReviewCardRequest, opts ...grpc.CallOption) (*ReviewCardResponse, error) {
	cOpts := append([]grpc.CallOption{grpc.StaticMethod()}, opts...)
	out := new(ReviewCardResponse)
	err := c.cc.Invoke(ctx, NoteService_ReviewCard_FullMethodName, in, out, cOpts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// NoteServiceServer is the server API for NoteService service.
// All implementations must embed UnimplementedNoteServiceServer
// for forward compatibility.
type NoteServiceServer interface {
	GetNote(context.Context, *GetNoteRequest) (*GetNoteResponse, error)
	CreateNote(context.Context, *CreateNoteRequest) (*CreateNoteResponse, error)
	RemoveNote(context.Context, *DeleteNoteRequest) (*DeleteNoteResponse, error)
	ReviewNote(context.Context, *ReviewNoteRequest) (*ReviewNoteResponse, error)
	DueNotes(context.Context, *DueNotesRequest) (*DueNotesResponse, error)
	InitDueNotes(context.Context, *InitDueNotesRequest) (*MessageResponse, error)
	GetCard(context.Context, *GetCardRequest) (*GetCardResponse, error)
	ReviewCard(context.Context, *ReviewCardRequest) (*ReviewCardResponse, error)
	mustEmbedUnimplementedNoteServiceServer()
}

// UnimplementedNoteServiceServer must be embedded to have
// forward compatible implementations.
//
// NOTE: this should be embedded by value instead of pointer to avoid a nil
// pointer dereference when methods are called.
type UnimplementedNoteServiceServer struct{}

func (UnimplementedNoteServiceServer) GetNote(context.Context, *GetNoteRequest) (*GetNoteResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetNote not implemented")
}
func (UnimplementedNoteServiceServer) CreateNote(context.Context, *CreateNoteRequest) (*CreateNoteResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method CreateNote not implemented")
}
func (UnimplementedNoteServiceServer) RemoveNote(context.Context, *DeleteNoteRequest) (*DeleteNoteResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method RemoveNote not implemented")
}
func (UnimplementedNoteServiceServer) ReviewNote(context.Context, *ReviewNoteRequest) (*ReviewNoteResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method ReviewNote not implemented")
}
func (UnimplementedNoteServiceServer) DueNotes(context.Context, *DueNotesRequest) (*DueNotesResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method DueNotes not implemented")
}
func (UnimplementedNoteServiceServer) InitDueNotes(context.Context, *InitDueNotesRequest) (*MessageResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method InitDueNotes not implemented")
}
func (UnimplementedNoteServiceServer) GetCard(context.Context, *GetCardRequest) (*GetCardResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetCard not implemented")
}
func (UnimplementedNoteServiceServer) ReviewCard(context.Context, *ReviewCardRequest) (*ReviewCardResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method ReviewCard not implemented")
}
func (UnimplementedNoteServiceServer) mustEmbedUnimplementedNoteServiceServer() {}
func (UnimplementedNoteServiceServer) testEmbeddedByValue()                     {}

// UnsafeNoteServiceServer may be embedded to opt out of forward compatibility for this service.
// Use of this interface is not recommended, as added methods to NoteServiceServer will
// result in compilation errors.
type UnsafeNoteServiceServer interface {
	mustEmbedUnimplementedNoteServiceServer()
}

func RegisterNoteServiceServer(s grpc.ServiceRegistrar, srv NoteServiceServer) {
	// If the following call pancis, it indicates UnimplementedNoteServiceServer was
	// embedded by pointer and is nil.  This will cause panics if an
	// unimplemented method is ever invoked, so we test this at initialization
	// time to prevent it from happening at runtime later due to I/O.
	if t, ok := srv.(interface{ testEmbeddedByValue() }); ok {
		t.testEmbeddedByValue()
	}
	s.RegisterService(&NoteService_ServiceDesc, srv)
}

func _NoteService_GetNote_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(GetNoteRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(NoteServiceServer).GetNote(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: NoteService_GetNote_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(NoteServiceServer).GetNote(ctx, req.(*GetNoteRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _NoteService_CreateNote_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(CreateNoteRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(NoteServiceServer).CreateNote(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: NoteService_CreateNote_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(NoteServiceServer).CreateNote(ctx, req.(*CreateNoteRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _NoteService_RemoveNote_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(DeleteNoteRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(NoteServiceServer).RemoveNote(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: NoteService_RemoveNote_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(NoteServiceServer).RemoveNote(ctx, req.(*DeleteNoteRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _NoteService_ReviewNote_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(ReviewNoteRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(NoteServiceServer).ReviewNote(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: NoteService_ReviewNote_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(NoteServiceServer).ReviewNote(ctx, req.(*ReviewNoteRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _NoteService_DueNotes_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(DueNotesRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(NoteServiceServer).DueNotes(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: NoteService_DueNotes_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(NoteServiceServer).DueNotes(ctx, req.(*DueNotesRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _NoteService_InitDueNotes_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(InitDueNotesRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(NoteServiceServer).InitDueNotes(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: NoteService_InitDueNotes_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(NoteServiceServer).InitDueNotes(ctx, req.(*InitDueNotesRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _NoteService_GetCard_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(GetCardRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(NoteServiceServer).GetCard(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: NoteService_GetCard_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(NoteServiceServer).GetCard(ctx, req.(*GetCardRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _NoteService_ReviewCard_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(ReviewCardRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(NoteServiceServer).ReviewCard(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: NoteService_ReviewCard_FullMethodName,
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(NoteServiceServer).ReviewCard(ctx, req.(*ReviewCardRequest))
	}
	return interceptor(ctx, in, info, handler)
}

// NoteService_ServiceDesc is the grpc.ServiceDesc for NoteService service.
// It's only intended for direct use with grpc.RegisterService,
// and not to be introspected or modified (even as a copy)
var NoteService_ServiceDesc = grpc.ServiceDesc{
	ServiceName: "memo.v1.NoteService",
	HandlerType: (*NoteServiceServer)(nil),
	Methods: []grpc.MethodDesc{
		{
			MethodName: "GetNote",
			Handler:    _NoteService_GetNote_Handler,
		},
		{
			MethodName: "CreateNote",
			Handler:    _NoteService_CreateNote_Handler,
		},
		{
			MethodName: "RemoveNote",
			Handler:    _NoteService_RemoveNote_Handler,
		},
		{
			MethodName: "ReviewNote",
			Handler:    _NoteService_ReviewNote_Handler,
		},
		{
			MethodName: "DueNotes",
			Handler:    _NoteService_DueNotes_Handler,
		},
		{
			MethodName: "InitDueNotes",
			Handler:    _NoteService_InitDueNotes_Handler,
		},
		{
			MethodName: "GetCard",
			Handler:    _NoteService_GetCard_Handler,
		},
		{
			MethodName: "ReviewCard",
			Handler:    _NoteService_ReviewCard_Handler,
		},
	},
	Streams:  []grpc.StreamDesc{},
	Metadata: "memo/v1/notes.proto",
}
