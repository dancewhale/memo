package location

type IDLocation struct {
	Location
}

func (l *IDLocation) ParseLink() LocationApi {
	return l
}

func (l *IDLocation) String() string {
	return "[[" + l.Protocol + ":" + l.Location.Link + "]]"
}
