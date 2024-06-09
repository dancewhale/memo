package fsrs

// note 描述了闪卡保存内容。
type Note struct {
	Content   string  `json:"Content",copier:"Content`
	Type      string  `json:"Type",copier:"Type`
	OrgID     string  `gorm:"unique",copier:"Orgid`
	Hash      string  `json:"Hash",copier:"Hash"`
}

func (n *Note) 	GetOrgId() string {
	return n.OrgID
}
func (n *Note) 	GetContent() string {
	return n.Content
}

func (n *Note) 	GetType() string {
	return n.Type
}

