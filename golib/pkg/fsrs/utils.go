package fsrs

import (
	//import fsrs
	gfsrs "github.com/open-spaced-repetition/go-fsrs"
)

// change string to fsrs.rate
func Rate(rate string) gfsrs.Rating {
	switch rate {
	case "Good":
		return gfsrs.Good
	case "Easy":
		return gfsrs.Easy
	case "Hard":
		return gfsrs.Hard
	case "Again":
		return gfsrs.Again
	default:
		return gfsrs.Again
	}
}

