package client

import (
	"fmt"
	"strconv"
	"strings"
)

// formatSexpArg formats a single Go argument into an S-expression string fragment.
func formatSexpArg(arg interface{}) string {
	switch v := arg.(type) {
	case string:
		// Escape backslashes and double quotes within the string
		escaped := strings.ReplaceAll(v, "\\", "\\\\")
		escaped = strings.ReplaceAll(escaped, "\"", "\\\"")
		return "\"" + escaped + "\""
	case int:
		return strconv.Itoa(v)
	case int64:
		return strconv.FormatInt(v, 10)
	case float32:
		return strconv.FormatFloat(float64(v), 'f', -1, 32)
	case float64:
		return strconv.FormatFloat(v, 'f', -1, 64)
	case bool:
		if v {
			return "t"
		}
		return "nil"
	case nil:
		return "nil"
	case []interface{}: // Handle slices specifically for lists
		var parts []string
		for _, item := range v {
			parts = append(parts, formatSexpArg(item))
		}
		return "(" + strings.Join(parts, " ") + ")"
	// Add other types as needed (e.g., structs, maps might need specific formatting)
	default:
		// Default to string representation, might need refinement
		return fmt.Sprintf("%v", v)
	}
}
