// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.34.2
// 	protoc        (unknown)
// source: memo/v1/notes.proto

package note

import (
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	reflect "reflect"
	sync "sync"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

type CreateNoteRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Orgid   string `protobuf:"bytes,1,opt,name=orgid,proto3" json:"orgid,omitempty"`
	Content string `protobuf:"bytes,2,opt,name=content,proto3" json:"content,omitempty"`
	Type    string `protobuf:"bytes,3,opt,name=type,proto3" json:"type,omitempty"`
}

func (x *CreateNoteRequest) Reset() {
	*x = CreateNoteRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[0]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *CreateNoteRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*CreateNoteRequest) ProtoMessage() {}

func (x *CreateNoteRequest) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[0]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use CreateNoteRequest.ProtoReflect.Descriptor instead.
func (*CreateNoteRequest) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{0}
}

func (x *CreateNoteRequest) GetOrgid() string {
	if x != nil {
		return x.Orgid
	}
	return ""
}

func (x *CreateNoteRequest) GetContent() string {
	if x != nil {
		return x.Content
	}
	return ""
}

func (x *CreateNoteRequest) GetType() string {
	if x != nil {
		return x.Type
	}
	return ""
}

type CreateNoteResponse struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Orgid string `protobuf:"bytes,1,opt,name=orgid,proto3" json:"orgid,omitempty"`
}

func (x *CreateNoteResponse) Reset() {
	*x = CreateNoteResponse{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[1]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *CreateNoteResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*CreateNoteResponse) ProtoMessage() {}

func (x *CreateNoteResponse) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[1]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use CreateNoteResponse.ProtoReflect.Descriptor instead.
func (*CreateNoteResponse) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{1}
}

func (x *CreateNoteResponse) GetOrgid() string {
	if x != nil {
		return x.Orgid
	}
	return ""
}

type GetNoteRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Orgid string `protobuf:"bytes,1,opt,name=orgid,proto3" json:"orgid,omitempty"`
}

func (x *GetNoteRequest) Reset() {
	*x = GetNoteRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[2]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *GetNoteRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*GetNoteRequest) ProtoMessage() {}

func (x *GetNoteRequest) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[2]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use GetNoteRequest.ProtoReflect.Descriptor instead.
func (*GetNoteRequest) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{2}
}

func (x *GetNoteRequest) GetOrgid() string {
	if x != nil {
		return x.Orgid
	}
	return ""
}

type GetNoteResponse struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Orgid   string `protobuf:"bytes,1,opt,name=orgid,proto3" json:"orgid,omitempty"`
	Content string `protobuf:"bytes,2,opt,name=content,proto3" json:"content,omitempty"`
	Type    string `protobuf:"bytes,3,opt,name=type,proto3" json:"type,omitempty"`
}

func (x *GetNoteResponse) Reset() {
	*x = GetNoteResponse{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[3]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *GetNoteResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*GetNoteResponse) ProtoMessage() {}

func (x *GetNoteResponse) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[3]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use GetNoteResponse.ProtoReflect.Descriptor instead.
func (*GetNoteResponse) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{3}
}

func (x *GetNoteResponse) GetOrgid() string {
	if x != nil {
		return x.Orgid
	}
	return ""
}

func (x *GetNoteResponse) GetContent() string {
	if x != nil {
		return x.Content
	}
	return ""
}

func (x *GetNoteResponse) GetType() string {
	if x != nil {
		return x.Type
	}
	return ""
}

type DeleteNoteRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Orgid string `protobuf:"bytes,1,opt,name=orgid,proto3" json:"orgid,omitempty"`
}

func (x *DeleteNoteRequest) Reset() {
	*x = DeleteNoteRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[4]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *DeleteNoteRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*DeleteNoteRequest) ProtoMessage() {}

func (x *DeleteNoteRequest) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[4]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use DeleteNoteRequest.ProtoReflect.Descriptor instead.
func (*DeleteNoteRequest) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{4}
}

func (x *DeleteNoteRequest) GetOrgid() string {
	if x != nil {
		return x.Orgid
	}
	return ""
}

type DeleteNoteResponse struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Result string `protobuf:"bytes,1,opt,name=result,proto3" json:"result,omitempty"`
}

func (x *DeleteNoteResponse) Reset() {
	*x = DeleteNoteResponse{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[5]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *DeleteNoteResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*DeleteNoteResponse) ProtoMessage() {}

func (x *DeleteNoteResponse) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[5]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use DeleteNoteResponse.ProtoReflect.Descriptor instead.
func (*DeleteNoteResponse) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{5}
}

func (x *DeleteNoteResponse) GetResult() string {
	if x != nil {
		return x.Result
	}
	return ""
}

type ReviewNoteRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Orgid string `protobuf:"bytes,1,opt,name=orgid,proto3" json:"orgid,omitempty"`
	Rate  string `protobuf:"bytes,2,opt,name=rate,proto3" json:"rate,omitempty"`
}

func (x *ReviewNoteRequest) Reset() {
	*x = ReviewNoteRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[6]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *ReviewNoteRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ReviewNoteRequest) ProtoMessage() {}

func (x *ReviewNoteRequest) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[6]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ReviewNoteRequest.ProtoReflect.Descriptor instead.
func (*ReviewNoteRequest) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{6}
}

func (x *ReviewNoteRequest) GetOrgid() string {
	if x != nil {
		return x.Orgid
	}
	return ""
}

func (x *ReviewNoteRequest) GetRate() string {
	if x != nil {
		return x.Rate
	}
	return ""
}

type ReviewNoteResponse struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Orgid string `protobuf:"bytes,1,opt,name=orgid,proto3" json:"orgid,omitempty"`
}

func (x *ReviewNoteResponse) Reset() {
	*x = ReviewNoteResponse{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[7]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *ReviewNoteResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ReviewNoteResponse) ProtoMessage() {}

func (x *ReviewNoteResponse) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[7]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ReviewNoteResponse.ProtoReflect.Descriptor instead.
func (*ReviewNoteResponse) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{7}
}

func (x *ReviewNoteResponse) GetOrgid() string {
	if x != nil {
		return x.Orgid
	}
	return ""
}

type DueNotesRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Day int64 `protobuf:"varint,1,opt,name=day,proto3" json:"day,omitempty"`
}

func (x *DueNotesRequest) Reset() {
	*x = DueNotesRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[8]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *DueNotesRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*DueNotesRequest) ProtoMessage() {}

func (x *DueNotesRequest) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[8]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use DueNotesRequest.ProtoReflect.Descriptor instead.
func (*DueNotesRequest) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{8}
}

func (x *DueNotesRequest) GetDay() int64 {
	if x != nil {
		return x.Day
	}
	return 0
}

type DueNotesResponse struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Orgid []string `protobuf:"bytes,1,rep,name=orgid,proto3" json:"orgid,omitempty"`
}

func (x *DueNotesResponse) Reset() {
	*x = DueNotesResponse{}
	if protoimpl.UnsafeEnabled {
		mi := &file_memo_v1_notes_proto_msgTypes[9]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *DueNotesResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*DueNotesResponse) ProtoMessage() {}

func (x *DueNotesResponse) ProtoReflect() protoreflect.Message {
	mi := &file_memo_v1_notes_proto_msgTypes[9]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use DueNotesResponse.ProtoReflect.Descriptor instead.
func (*DueNotesResponse) Descriptor() ([]byte, []int) {
	return file_memo_v1_notes_proto_rawDescGZIP(), []int{9}
}

func (x *DueNotesResponse) GetOrgid() []string {
	if x != nil {
		return x.Orgid
	}
	return nil
}

var File_memo_v1_notes_proto protoreflect.FileDescriptor

var file_memo_v1_notes_proto_rawDesc = []byte{
	0x0a, 0x13, 0x6d, 0x65, 0x6d, 0x6f, 0x2f, 0x76, 0x31, 0x2f, 0x6e, 0x6f, 0x74, 0x65, 0x73, 0x2e,
	0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x07, 0x6d, 0x65, 0x6d, 0x6f, 0x2e, 0x76, 0x31, 0x22, 0x57,
	0x0a, 0x11, 0x43, 0x72, 0x65, 0x61, 0x74, 0x65, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x71, 0x75,
	0x65, 0x73, 0x74, 0x12, 0x14, 0x0a, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01,
	0x28, 0x09, 0x52, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x12, 0x18, 0x0a, 0x07, 0x63, 0x6f, 0x6e,
	0x74, 0x65, 0x6e, 0x74, 0x18, 0x02, 0x20, 0x01, 0x28, 0x09, 0x52, 0x07, 0x63, 0x6f, 0x6e, 0x74,
	0x65, 0x6e, 0x74, 0x12, 0x12, 0x0a, 0x04, 0x74, 0x79, 0x70, 0x65, 0x18, 0x03, 0x20, 0x01, 0x28,
	0x09, 0x52, 0x04, 0x74, 0x79, 0x70, 0x65, 0x22, 0x2a, 0x0a, 0x12, 0x43, 0x72, 0x65, 0x61, 0x74,
	0x65, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x12, 0x14, 0x0a,
	0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x05, 0x6f, 0x72,
	0x67, 0x69, 0x64, 0x22, 0x26, 0x0a, 0x0e, 0x47, 0x65, 0x74, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65,
	0x71, 0x75, 0x65, 0x73, 0x74, 0x12, 0x14, 0x0a, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x18, 0x01,
	0x20, 0x01, 0x28, 0x09, 0x52, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x22, 0x55, 0x0a, 0x0f, 0x47,
	0x65, 0x74, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x12, 0x14,
	0x0a, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x05, 0x6f,
	0x72, 0x67, 0x69, 0x64, 0x12, 0x18, 0x0a, 0x07, 0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x18,
	0x02, 0x20, 0x01, 0x28, 0x09, 0x52, 0x07, 0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x12, 0x12,
	0x0a, 0x04, 0x74, 0x79, 0x70, 0x65, 0x18, 0x03, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x74, 0x79,
	0x70, 0x65, 0x22, 0x29, 0x0a, 0x11, 0x44, 0x65, 0x6c, 0x65, 0x74, 0x65, 0x4e, 0x6f, 0x74, 0x65,
	0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x12, 0x14, 0x0a, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64,
	0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x22, 0x2c, 0x0a,
	0x12, 0x44, 0x65, 0x6c, 0x65, 0x74, 0x65, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x73, 0x70, 0x6f,
	0x6e, 0x73, 0x65, 0x12, 0x16, 0x0a, 0x06, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x18, 0x01, 0x20,
	0x01, 0x28, 0x09, 0x52, 0x06, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x22, 0x3d, 0x0a, 0x11, 0x52,
	0x65, 0x76, 0x69, 0x65, 0x77, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74,
	0x12, 0x14, 0x0a, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52,
	0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x12, 0x12, 0x0a, 0x04, 0x72, 0x61, 0x74, 0x65, 0x18, 0x02,
	0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x72, 0x61, 0x74, 0x65, 0x22, 0x2a, 0x0a, 0x12, 0x52, 0x65,
	0x76, 0x69, 0x65, 0x77, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65,
	0x12, 0x14, 0x0a, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52,
	0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x22, 0x23, 0x0a, 0x0f, 0x44, 0x75, 0x65, 0x4e, 0x6f, 0x74,
	0x65, 0x73, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x12, 0x10, 0x0a, 0x03, 0x64, 0x61, 0x79,
	0x18, 0x01, 0x20, 0x01, 0x28, 0x03, 0x52, 0x03, 0x64, 0x61, 0x79, 0x22, 0x28, 0x0a, 0x10, 0x44,
	0x75, 0x65, 0x4e, 0x6f, 0x74, 0x65, 0x73, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x12,
	0x14, 0x0a, 0x05, 0x6f, 0x72, 0x67, 0x69, 0x64, 0x18, 0x01, 0x20, 0x03, 0x28, 0x09, 0x52, 0x05,
	0x6f, 0x72, 0x67, 0x69, 0x64, 0x32, 0xe1, 0x02, 0x0a, 0x0b, 0x4e, 0x6f, 0x74, 0x65, 0x53, 0x65,
	0x72, 0x76, 0x69, 0x63, 0x65, 0x12, 0x3c, 0x0a, 0x07, 0x47, 0x65, 0x74, 0x4e, 0x6f, 0x74, 0x65,
	0x12, 0x17, 0x2e, 0x6d, 0x65, 0x6d, 0x6f, 0x2e, 0x76, 0x31, 0x2e, 0x47, 0x65, 0x74, 0x4e, 0x6f,
	0x74, 0x65, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x1a, 0x18, 0x2e, 0x6d, 0x65, 0x6d, 0x6f,
	0x2e, 0x76, 0x31, 0x2e, 0x47, 0x65, 0x74, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x73, 0x70, 0x6f,
	0x6e, 0x73, 0x65, 0x12, 0x45, 0x0a, 0x0a, 0x43, 0x72, 0x65, 0x61, 0x74, 0x65, 0x4e, 0x6f, 0x74,
	0x65, 0x12, 0x1a, 0x2e, 0x6d, 0x65, 0x6d, 0x6f, 0x2e, 0x76, 0x31, 0x2e, 0x43, 0x72, 0x65, 0x61,
	0x74, 0x65, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x1a, 0x1b, 0x2e,
	0x6d, 0x65, 0x6d, 0x6f, 0x2e, 0x76, 0x31, 0x2e, 0x43, 0x72, 0x65, 0x61, 0x74, 0x65, 0x4e, 0x6f,
	0x74, 0x65, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x12, 0x45, 0x0a, 0x0a, 0x52, 0x65,
	0x6d, 0x6f, 0x76, 0x65, 0x4e, 0x6f, 0x74, 0x65, 0x12, 0x1a, 0x2e, 0x6d, 0x65, 0x6d, 0x6f, 0x2e,
	0x76, 0x31, 0x2e, 0x44, 0x65, 0x6c, 0x65, 0x74, 0x65, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x71,
	0x75, 0x65, 0x73, 0x74, 0x1a, 0x1b, 0x2e, 0x6d, 0x65, 0x6d, 0x6f, 0x2e, 0x76, 0x31, 0x2e, 0x44,
	0x65, 0x6c, 0x65, 0x74, 0x65, 0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73,
	0x65, 0x12, 0x45, 0x0a, 0x0a, 0x52, 0x65, 0x76, 0x69, 0x65, 0x77, 0x4e, 0x6f, 0x74, 0x65, 0x12,
	0x1a, 0x2e, 0x6d, 0x65, 0x6d, 0x6f, 0x2e, 0x76, 0x31, 0x2e, 0x52, 0x65, 0x76, 0x69, 0x65, 0x77,
	0x4e, 0x6f, 0x74, 0x65, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x1a, 0x1b, 0x2e, 0x6d, 0x65,
	0x6d, 0x6f, 0x2e, 0x76, 0x31, 0x2e, 0x52, 0x65, 0x76, 0x69, 0x65, 0x77, 0x4e, 0x6f, 0x74, 0x65,
	0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x12, 0x3f, 0x0a, 0x08, 0x44, 0x75, 0x65, 0x4e,
	0x6f, 0x74, 0x65, 0x73, 0x12, 0x18, 0x2e, 0x6d, 0x65, 0x6d, 0x6f, 0x2e, 0x76, 0x31, 0x2e, 0x44,
	0x75, 0x65, 0x4e, 0x6f, 0x74, 0x65, 0x73, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x1a, 0x19,
	0x2e, 0x6d, 0x65, 0x6d, 0x6f, 0x2e, 0x76, 0x31, 0x2e, 0x44, 0x75, 0x65, 0x4e, 0x6f, 0x74, 0x65,
	0x73, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x42, 0x1e, 0x5a, 0x1c, 0x6d, 0x65, 0x6d,
	0x6f, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x2f, 0x67, 0x72, 0x70, 0x63, 0x2f, 0x6d, 0x65, 0x6d,
	0x6f, 0x2f, 0x76, 0x31, 0x3b, 0x6e, 0x6f, 0x74, 0x65, 0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f,
	0x33,
}

var (
	file_memo_v1_notes_proto_rawDescOnce sync.Once
	file_memo_v1_notes_proto_rawDescData = file_memo_v1_notes_proto_rawDesc
)

func file_memo_v1_notes_proto_rawDescGZIP() []byte {
	file_memo_v1_notes_proto_rawDescOnce.Do(func() {
		file_memo_v1_notes_proto_rawDescData = protoimpl.X.CompressGZIP(file_memo_v1_notes_proto_rawDescData)
	})
	return file_memo_v1_notes_proto_rawDescData
}

var file_memo_v1_notes_proto_msgTypes = make([]protoimpl.MessageInfo, 10)
var file_memo_v1_notes_proto_goTypes = []any{
	(*CreateNoteRequest)(nil),  // 0: memo.v1.CreateNoteRequest
	(*CreateNoteResponse)(nil), // 1: memo.v1.CreateNoteResponse
	(*GetNoteRequest)(nil),     // 2: memo.v1.GetNoteRequest
	(*GetNoteResponse)(nil),    // 3: memo.v1.GetNoteResponse
	(*DeleteNoteRequest)(nil),  // 4: memo.v1.DeleteNoteRequest
	(*DeleteNoteResponse)(nil), // 5: memo.v1.DeleteNoteResponse
	(*ReviewNoteRequest)(nil),  // 6: memo.v1.ReviewNoteRequest
	(*ReviewNoteResponse)(nil), // 7: memo.v1.ReviewNoteResponse
	(*DueNotesRequest)(nil),    // 8: memo.v1.DueNotesRequest
	(*DueNotesResponse)(nil),   // 9: memo.v1.DueNotesResponse
}
var file_memo_v1_notes_proto_depIdxs = []int32{
	2, // 0: memo.v1.NoteService.GetNote:input_type -> memo.v1.GetNoteRequest
	0, // 1: memo.v1.NoteService.CreateNote:input_type -> memo.v1.CreateNoteRequest
	4, // 2: memo.v1.NoteService.RemoveNote:input_type -> memo.v1.DeleteNoteRequest
	6, // 3: memo.v1.NoteService.ReviewNote:input_type -> memo.v1.ReviewNoteRequest
	8, // 4: memo.v1.NoteService.DueNotes:input_type -> memo.v1.DueNotesRequest
	3, // 5: memo.v1.NoteService.GetNote:output_type -> memo.v1.GetNoteResponse
	1, // 6: memo.v1.NoteService.CreateNote:output_type -> memo.v1.CreateNoteResponse
	5, // 7: memo.v1.NoteService.RemoveNote:output_type -> memo.v1.DeleteNoteResponse
	7, // 8: memo.v1.NoteService.ReviewNote:output_type -> memo.v1.ReviewNoteResponse
	9, // 9: memo.v1.NoteService.DueNotes:output_type -> memo.v1.DueNotesResponse
	5, // [5:10] is the sub-list for method output_type
	0, // [0:5] is the sub-list for method input_type
	0, // [0:0] is the sub-list for extension type_name
	0, // [0:0] is the sub-list for extension extendee
	0, // [0:0] is the sub-list for field type_name
}

func init() { file_memo_v1_notes_proto_init() }
func file_memo_v1_notes_proto_init() {
	if File_memo_v1_notes_proto != nil {
		return
	}
	if !protoimpl.UnsafeEnabled {
		file_memo_v1_notes_proto_msgTypes[0].Exporter = func(v any, i int) any {
			switch v := v.(*CreateNoteRequest); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_memo_v1_notes_proto_msgTypes[1].Exporter = func(v any, i int) any {
			switch v := v.(*CreateNoteResponse); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_memo_v1_notes_proto_msgTypes[2].Exporter = func(v any, i int) any {
			switch v := v.(*GetNoteRequest); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_memo_v1_notes_proto_msgTypes[3].Exporter = func(v any, i int) any {
			switch v := v.(*GetNoteResponse); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_memo_v1_notes_proto_msgTypes[4].Exporter = func(v any, i int) any {
			switch v := v.(*DeleteNoteRequest); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_memo_v1_notes_proto_msgTypes[5].Exporter = func(v any, i int) any {
			switch v := v.(*DeleteNoteResponse); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_memo_v1_notes_proto_msgTypes[6].Exporter = func(v any, i int) any {
			switch v := v.(*ReviewNoteRequest); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_memo_v1_notes_proto_msgTypes[7].Exporter = func(v any, i int) any {
			switch v := v.(*ReviewNoteResponse); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_memo_v1_notes_proto_msgTypes[8].Exporter = func(v any, i int) any {
			switch v := v.(*DueNotesRequest); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_memo_v1_notes_proto_msgTypes[9].Exporter = func(v any, i int) any {
			switch v := v.(*DueNotesResponse); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_memo_v1_notes_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   10,
			NumExtensions: 0,
			NumServices:   1,
		},
		GoTypes:           file_memo_v1_notes_proto_goTypes,
		DependencyIndexes: file_memo_v1_notes_proto_depIdxs,
		MessageInfos:      file_memo_v1_notes_proto_msgTypes,
	}.Build()
	File_memo_v1_notes_proto = out.File
	file_memo_v1_notes_proto_rawDesc = nil
	file_memo_v1_notes_proto_goTypes = nil
	file_memo_v1_notes_proto_depIdxs = nil
}
