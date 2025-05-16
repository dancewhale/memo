package _import

import (
	"os"

	"memo/pkg/logger"
)

// GetLeadingChars processes a string to remove leading unwanted characters
// and returns the first 'num' characters of the result.
// Unwanted leading characters are: space, tab, '-', '/'.
func GetLeadingChars(s string, num int) string {
	if num <= 0 {
		return ""
	}

	startIndex := 0
	runes := []rune(s) // Convert to runes for correct character indexing

	for i, r := range runes {
		if r == ' ' || r == '\t' || r == '-' || r == '/' {
			// Continue skipping if it's one of the specified characters
			// This loop structure was slightly off in thought, correcting here:
			// We need to find the *first* character that is NOT one of these.
		} else {
			startIndex = i
			break // Found the first valid character
		}
		// If loop finishes, all characters were skippable
		if i == len(runes)-1 && (r == ' ' || r == '\t' || r == '-' || r == '/') {
			startIndex = len(runes)
		}
	}

	// If all characters were trimmed or string was empty initially
	if startIndex >= len(runes) {
		return ""
	}

	trimmedRunes := runes[startIndex:]

	if num >= len(trimmedRunes) {
		return string(trimmedRunes)
	}

	return string(trimmedRunes[:num])
}

// removeDirIfEmpty checks if a directory is empty and removes it if it is.
// It returns an error for unexpected issues during check or removal.
func removeDirIfEmpty(dirPath string) error {
	entries, err := os.ReadDir(dirPath)
	if err != nil {
		if os.IsNotExist(err) {
			return nil // Not an error if dir doesn't exist
		}
		return logger.Errorf("failed to read directory %s for cleanup: %v", dirPath, err)
	}

	if len(entries) == 0 {
		err = os.RemoveAll(dirPath)
		if err != nil {
			return logger.Errorf("failed to remove empty directory %s: %v", dirPath, err)
		}
	}
	return nil
}
