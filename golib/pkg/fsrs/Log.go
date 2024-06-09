package fsrs

// Log 描述了复习日志记录。
type Log struct {
	ID            string
	CardID        string
	Rating        Rating
	ScheduledDays uint64
	ElapsedDays   uint64
	Reviewed      int64
	State         State
}
