package client

import (
	"fmt"
	"strings"

	"memo/pkg/logger"

	"github.com/kiwanami/go-elrpc"
)

var EClient *EmacsEpcClient

type EmacsEpcClient struct {
	epcClient elrpc.Service
	goPort    int
	emacsPort int
}

func NewEmacsEpcClient(emacsPort int) error {
	if EClient == nil || EClient.epcClient == nil {
		// Added error logging context
		logger.Infof("Attempting to start EPC client for Emacs on port %d", emacsPort)
		cs, err := elrpc.StartClient(emacsPort, nil)
		if err != nil {
			return logger.Errorf("Failed to start epc client on port %d: %v", emacsPort, err)
		}
		EClient = &EmacsEpcClient{epcClient: cs, emacsPort: emacsPort}
		logger.Infof("Successfully started EPC client for Emacs on port %d", emacsPort)
		return nil
	}
	return nil
}

func (c *EmacsEpcClient) RefreshEmacsGoPort(goPort int) {
	// Storing goPort in the struct
	c.goPort = goPort
	startCommand := fmt.Sprintf("(memo-bridge--first-start %d)", goPort)
	logger.Infof("Calling eval-in-emacs with command: %s", startCommand)
	r, err := c.epcClient.Call("eval-in-emacs", startCommand)
	// Improved error logging
	if err != nil {
		logger.Errorf("Error calling eval-in-emacs for RefreshEmacsGoPort: %v", err)
	} else if r != nil {
		// Log the response if it's not nil, as it might indicate an Elisp-level issue
		logger.Warnf("Non-nil response from eval-in-emacs for RefreshEmacsGoPort: %v", r)
	} else {
		logger.Debug("Successfully called eval-in-emacs for RefreshEmacsGoPort")
	}
}

func (c *EmacsEpcClient) Close() {
	if c.epcClient != nil {
		logger.Infof("Attempting to stop EPC client for Emacs on port %d", c.emacsPort)
		err := c.epcClient.Stop()
		if err != nil {
			logger.Errorf("Failed to stop epc client: %v", err)
		} else {
			logger.Debugf("Successfully stopped EPC client for Emacs on port %d", c.emacsPort)
		}
	}
}

// EvalInEmacs evaluates an S-expression in Emacs via EPC.
// It takes a method name and variable arguments, formats them into an S-expression string,
// and calls the 'eval-in-emacs' function on the Emacs side.
func (c *EmacsEpcClient) EvalInEmacs(methodName string, args ...interface{}) (interface{}, error) {
	if c.epcClient == nil {
		return nil, logger.Errorf("EPC client is not initialized")
	}

	// Format arguments into an S-expression list string: "(methodName arg1 arg2 ...)"
	formattedArgs := make([]string, 0, len(args)+1)
	formattedArgs = append(formattedArgs, methodName) // Method name is usually a symbol
	for _, arg := range args {
		formattedArgs = append(formattedArgs, formatSexpArg(arg))
	}
	sexp := "(" + strings.Join(formattedArgs, " ") + ")"

	logger.Debugf("EvalInEmacs Sexp: %s", sexp)

	// Call the 'eval-in-emacs' elisp function with the generated S-expression string
	// Assuming 'eval-in-emacs' in Elisp expects ONE string argument which is the S-expression to evaluate.
	result, err := c.epcClient.Call("eval-in-emacs", sexp)
	if err != nil {
		logger.Errorf("Error calling eval-in-emacs with sexp '%s': %v", sexp, err)
		return nil, err
	}
	if result != nil {
		// Log non-nil results which might indicate errors or return values from Emacs Lisp
		logger.Debugf("Result from eval-in-emacs with sexp '%s': %v", sexp, result)
	}

	return result, nil
}

func (c *EmacsEpcClient) GetEmacsVars(varName string) (interface{}, error) {
	if c.epcClient == nil {
		return nil, logger.Errorf("EPC client is not initialized")
	}
	// Call the 'get-emacs-var' elisp function with the variable name
	result, err := c.epcClient.Call("get-emacs-vars", varName)
	if err != nil {
		logger.Errorf("Error calling get-emacs-var with varName '%s': %v", varName, err)
		return nil, err
	}
	if result != nil {
		// Log non-nil results which might indicate errors or return values from Emacs Lisp
		logger.Debugf("Result from get-emacs-var with varName '%s': %v", varName, result)
	}
	return result, nil
}

// MessageEmacs sends a message string to be displayed in the Emacs echo area.
func (c *EmacsEpcClient) MessageEmacs(message string) {
	// Use the new EvalInEmacs function
	_, err := c.EvalInEmacs("message", message)
	if err != nil {
		// Error is already logged within EvalInEmacs
		logger.Warnf("Failed to send message '%s' to Emacs: %v", message, err)
	}
}
