;;; epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro memo-bridge-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar memo-bridge-deferred-debug nil
  "Debug output switch.")

(defvar memo-bridge-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun memo-bridge-deferred-log (&rest args)
  "[internal] Debug log function."
  (when memo-bridge-deferred-debug
    (with-current-buffer (get-buffer-create "*memo-bridge-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" memo-bridge-deferred-debug-count (apply #'format args)))))
    (cl-incf memo-bridge-deferred-debug-count)))

(defvar memo-bridge-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro memo-bridge-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`memo-bridge-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal memo-bridge-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar memo-bridge-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar memo-bridge-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `memo-bridge-deferred-post-task' and `memo-bridge-deferred-worker'.")

(defun memo-bridge-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`memo-bridge-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack memo-bridge-deferred-queue)
    (memo-bridge-deferred-log "QUEUE-POST [%s]: %s" (length memo-bridge-deferred-queue) pack)
    (run-at-time memo-bridge-deferred-tick-time nil 'memo-bridge-deferred-worker)
    d))

(defun memo-bridge-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when memo-bridge-deferred-queue
    (let* ((pack (car (last memo-bridge-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq memo-bridge-deferred-queue (nbutlast memo-bridge-deferred-queue))
      (condition-case err
          (setq value (memo-bridge-deferred-exec-task d which arg))
        (error
         (memo-bridge-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: memo-bridge-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `memo-bridge-deferred-resignal')
;; cancel      : a canceling function (default `memo-bridge-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct memo-bridge-deferred-object
  (callback 'identity)
  (errorback 'memo-bridge-deferred-resignal)
  (cancel 'memo-bridge-deferred-default-cancel)
  next status value)

(defun memo-bridge-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun memo-bridge-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (memo-bridge-deferred-log "CANCEL : %s" d)
  (setf (memo-bridge-deferred-object-callback d) 'identity)
  (setf (memo-bridge-deferred-object-errorback d) 'memo-bridge-deferred-resignal)
  (setf (memo-bridge-deferred-object-next d) nil)
  d)

(defun memo-bridge-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (memo-bridge-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "memo-bridge-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (memo-bridge-deferred-object-callback d)
                    (memo-bridge-deferred-object-errorback d)))
        (next-deferred (memo-bridge-deferred-object-next d)))
    (cond
     (callback
      (memo-bridge-deferred-condition-case err
        (let ((value (funcall callback arg)))
          (cond
           ((memo-bridge-deferred-object-p value)
            (memo-bridge-deferred-log "WAIT NEST : %s" value)
            (if next-deferred
                (memo-bridge-deferred-set-next value next-deferred)
              value))
           (t
            (if next-deferred
                (memo-bridge-deferred-post-task next-deferred 'ok value)
              (setf (memo-bridge-deferred-object-status d) 'ok)
              (setf (memo-bridge-deferred-object-value d) value)
              value))))
        (error
         (cond
          (next-deferred
           (memo-bridge-deferred-post-task next-deferred 'ng err))
          (t
           (memo-bridge-deferred-log "ERROR : %S" err)
           (message "deferred error : %S" err)
           (setf (memo-bridge-deferred-object-status d) 'ng)
           (setf (memo-bridge-deferred-object-value d) err)
           err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (memo-bridge-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (memo-bridge-deferred-resignal arg)))))))

(defun memo-bridge-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (memo-bridge-deferred-object-next prev) next)
  (cond
   ((eq 'ok (memo-bridge-deferred-object-status prev))
    (setf (memo-bridge-deferred-object-status prev) nil)
    (let ((ret (memo-bridge-deferred-exec-task
                next 'ok (memo-bridge-deferred-object-value prev))))
      (if (memo-bridge-deferred-object-p ret) ret
        next)))
   ((eq 'ng (memo-bridge-deferred-object-status prev))
    (setf (memo-bridge-deferred-object-status prev) nil)
    (let ((ret (memo-bridge-deferred-exec-task next 'ng (memo-bridge-deferred-object-value prev))))
      (if (memo-bridge-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun memo-bridge-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-memo-bridge-deferred-object :callback callback)
    (make-memo-bridge-deferred-object)))

(defun memo-bridge-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (memo-bridge-deferred-exec-task d 'ok arg))

(defun memo-bridge-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (memo-bridge-deferred-exec-task d 'ng arg))

(defun memo-bridge-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (memo-bridge-deferred-post-task d 'ok arg))

(defun memo-bridge-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (memo-bridge-deferred-callback-post (memo-bridge-deferred-new callback))."
  (let ((d (if callback
               (make-memo-bridge-deferred-object :callback callback)
             (make-memo-bridge-deferred-object))))
    (memo-bridge-deferred-callback-post d arg)
    d))

(defun memo-bridge-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-memo-bridge-deferred-object :callback callback)))
    (memo-bridge-deferred-set-next d nd)))

(defun memo-bridge-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-memo-bridge-deferred-object :errorback callback)))
    (memo-bridge-deferred-set-next d nd)))

(defvar memo-bridge-epc-debug nil)

(defun memo-bridge-epc-log (&rest args)
  (when memo-bridge-epc-debug
    (with-current-buffer (get-buffer-create "*memo-bridge-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun memo-bridge-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar memo-bridge-epc-uid 1)

(defun memo-bridge-epc-uid ()
  (cl-incf memo-bridge-epc-uid))

(defvar memo-bridge-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct memo-bridge-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun memo-bridge-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return memo-bridge-epc-connection object."
  (memo-bridge-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (memo-bridge-epc-uid))
         (connection-name (format "memo-bridge-epc con %s" connection-id))
         (connection-buf (memo-bridge-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-memo-bridge-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (memo-bridge-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (memo-bridge-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (memo-bridge-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun memo-bridge-epc-process-sentinel (connection process msg)
  (memo-bridge-epc-log "!! Process Sentinel [%s] : %S : %S"
                      (memo-bridge-epc-connection-name connection) process msg)
  (memo-bridge-epc-disconnect connection))

(defun memo-bridge-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (memo-bridge-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (memo-bridge-epc-connection-process connection)))
    (memo-bridge-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun memo-bridge-epc-disconnect (connection)
  (let ((process (memo-bridge-epc-connection-process connection))
        (buf (memo-bridge-epc-connection-buffer connection))
        (name (memo-bridge-epc-connection-name connection)))
    (memo-bridge-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (memo-bridge-epc-log "!! Disconnected finished [%s]" name)))

(defun memo-bridge-epc-process-filter (connection process message)
  (memo-bridge-epc-log "INCOMING: [%s] [%S]" (memo-bridge-epc-connection-name connection) message)
  (with-current-buffer (memo-bridge-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (memo-bridge-epc-process-available-input connection process)))

(defun memo-bridge-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (memo-bridge-deferred-new callback)
             (memo-bridge-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun memo-bridge-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (memo-bridge-deferred-callback-post d event))))

(defun memo-bridge-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (memo-bridge-epc-net-have-input-p)
      (let ((event (memo-bridge-epc-net-read-or-lose process))
            (ok nil))
        (memo-bridge-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'memo-bridge-epc-signal-send
                         (cons (memo-bridge-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (memo-bridge-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (memo-bridge-epc-process-available-input connection process)))))))

(defun memo-bridge-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (memo-bridge-epc-net-decode-length))))

(defun memo-bridge-epc-net-read-or-lose (_process)
  (condition-case error
      (memo-bridge-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun memo-bridge-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (memo-bridge-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun memo-bridge-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun memo-bridge-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct memo-bridge-epc-manager
  "Root object that holds all information related to an EPC activity.

`memo-bridge-epc-start-epc' returns this object.

title          : instance name for displaying on the `memo-bridge-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : memo-bridge-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct memo-bridge-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar memo-bridge-epc-live-connections nil
  "[internal] A list of `memo-bridge-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun memo-bridge-epc-server-process-name (uid)
  (format "memo-bridge-epc-server:%s" uid))

(defun memo-bridge-epc-server-buffer-name (uid)
  (format " *%s*" (memo-bridge-epc-server-process-name uid)))

(defun memo-bridge-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (memo-bridge-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (memo-bridge-epc-disconnect (memo-bridge-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 memo-bridge-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq memo-bridge-epc-live-connections (delete mngr memo-bridge-epc-live-connections))
    ))

(defun memo-bridge-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun memo-bridge-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an memo-bridge-epc-connection instance."
  (let* ((mngr mngr)
         (conn (memo-bridge-epc-manager-connection mngr))
         (channel (memo-bridge-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (memo-bridge-epc-log "SIG CALL: %S" args)
                    (apply 'memo-bridge-epc-handler-called-method ,mngr (memo-bridge-epc-args args))))
               (return
                . (lambda (args)
                    (memo-bridge-epc-log "SIG RET: %S" args)
                    (apply 'memo-bridge-epc-handler-return ,mngr (memo-bridge-epc-args args))))
               (return-error
                . (lambda (args)
                    (memo-bridge-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'memo-bridge-epc-handler-return-error ,mngr (memo-bridge-epc-args args))))
               (epc-error
                . (lambda (args)
                    (memo-bridge-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'memo-bridge-epc-handler-epc-error ,mngr (memo-bridge-epc-args args))))
               (methods
                . (lambda (args)
                    (memo-bridge-epc-log "SIG METHODS: %S" args)
                    (memo-bridge-epc-handler-methods ,mngr (caadr args))))
               ) do
             (memo-bridge-epc-signal-connect channel method body))
    (push mngr memo-bridge-epc-live-connections)
    mngr))

(defun memo-bridge-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (memo-bridge-epc-manager-connection mngr)))
    (memo-bridge-epc-net-send conn (cons method messages))))

(defun memo-bridge-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (memo-bridge-epc-manager-methods mngr)
	   ;; use equal because method-name is string not symbol.
	   ;; call from go server use string not symbol.
           if (equal method-name (memo-bridge-epc-method-name i))
           do (cl-return i)))

(defun memo-bridge-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (memo-bridge-epc-manager-methods mngr)
                  collect
                  (list
                   (memo-bridge-epc-method-name i)
                   (or (memo-bridge-epc-method-arg-specs i) "")
                   (or (memo-bridge-epc-method-docstring i) "")))))
    (memo-bridge-epc-manager-send mngr 'return uid info)))

(defun memo-bridge-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (memo-bridge-epc-manager-methods mngr))
           (method (memo-bridge-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (memo-bridge-epc-log "ERR: No such method : %s" name)
        (memo-bridge-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (memo-bridge-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((memo-bridge-deferred-object-p ret)
                (memo-bridge-deferred-nextc ret
                  (lambda (xx) (memo-bridge-epc-manager-send mngr 'return uid xx))))
               (t (memo-bridge-epc-manager-send mngr 'return uid ret))))
          (error
           (memo-bridge-epc-log "ERROR : %S" err)
           (memo-bridge-epc-manager-send mngr 'return-error uid err))))))))

(defun memo-bridge-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (memo-bridge-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (memo-bridge-epc-manager-sessions mngr) ret)))

(defun memo-bridge-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (memo-bridge-epc-manager-sessions mngr))))
    (cond
     (pair
      (memo-bridge-epc-log "RET: id:%s [%S]" uid args)
      (memo-bridge-epc-manager-remove-session mngr uid)
      (memo-bridge-deferred-callback (cdr pair) args))
     (t                                 ; error
      (memo-bridge-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun memo-bridge-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (memo-bridge-epc-manager-sessions mngr))))
    (cond
     (pair
      (memo-bridge-epc-log "RET-ERR: id:%s [%S]" uid args)
      (memo-bridge-epc-manager-remove-session mngr uid)
      (memo-bridge-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (memo-bridge-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun memo-bridge-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (memo-bridge-epc-manager-sessions mngr))))
    (cond
     (pair
      (memo-bridge-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (memo-bridge-epc-manager-remove-session mngr uid)
      (memo-bridge-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (memo-bridge-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun memo-bridge-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (memo-bridge-epc-uid))
        (sessions (memo-bridge-epc-manager-sessions mngr))
        (d (memo-bridge-deferred-new)))
    (push (cons uid d) sessions)
    (setf (memo-bridge-epc-manager-sessions mngr) sessions)
    (memo-bridge-epc-manager-send mngr 'call uid method-name args)
    d))

(defun memo-bridge-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-memo-bridge-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (memo-bridge-epc-manager-methods mngr))))
    (setf (memo-bridge-epc-manager-methods mngr) methods)
    method))

(defun memo-bridge-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'memo-bridge-epc-nothing))
    (memo-bridge-deferred-chain
      d
      (memo-bridge-deferred-nextc it
        (lambda (x) (setq result x)))
      (memo-bridge-deferred-error it
        (lambda (er) (setq result er))))
    (while (eq result 'memo-bridge-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (memo-bridge-epc-connection-process (memo-bridge-epc-manager-connection mngr))
         0 memo-bridge-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cadr result)) result)))

(defun memo-bridge-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (memo-bridge-epc-sync mngr (memo-bridge-epc-call-deferred mngr method-name args)))

(defun memo-bridge-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (memo-bridge-epc-connection-process (memo-bridge-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar memo-bridge-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`memo-bridge-epc-manager' instance]).
When the server process accepts the client connection, the
`memo-bridge-epc-manager' instance is created and stored in this variable
`memo-bridge-epc-server-client-processes'. This variable is used for the management
purpose.")

;; memo-bridge-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `memo-bridge-epc-manager' instances
(cl-defstruct memo-bridge-epc-server name process port connect-function)

(defvar memo-bridge-epc-server-processes nil
  "[internal] A list of ([process object] . [`memo-bridge-epc-server' instance]).
This variable is used for the management purpose.")

(defun memo-bridge-epc-server-get-server-by-process (proc)
  "[internal] Return the memo-bridge-epc-server instance for the local Emacs PROC."
  (cl-loop for (pp . server) in memo-bridge-epc-server-processes
           if (eql pp proc)
           do (cl-return server)
           finally return nil))

(defun memo-bridge-epc-server-get-manager-by-process (proc)
  "[internal] Return the memo-bridge-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in memo-bridge-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun memo-bridge-epc-server-accept (process)
  "[internal] Initialize the process and return memo-bridge-epc-manager object."
  (memo-bridge-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (memo-bridge-epc-uid))
         (connection-name (format "memo-bridge-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-memo-bridge-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (memo-bridge-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (memo-bridge-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (memo-bridge-epc-process-sentinel connection p e)))
    (make-memo-bridge-epc-manager :server-process process :port t
                                 :connection connection)))

(defun memo-bridge-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (memo-bridge-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (memo-bridge-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (memo-bridge-epc-server-accept process)))
            (push (cons process mngr) memo-bridge-epc-server-client-processes)
            (memo-bridge-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (memo-bridge-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (memo-bridge-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process memo-bridge-epc-server-client-processes)) _d)
        (when pair
          (memo-bridge-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (memo-bridge-epc-stop-epc (cdr pair))
          (setq memo-bridge-epc-server-client-processes
                (assq-delete-all process memo-bridge-epc-server-client-processes))
          ))
      nil))))

(defun memo-bridge-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "MEMO-BRIDGE EPC Server %s" (memo-bridge-epc-uid)))
       (buf (memo-bridge-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (memo-bridge-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-memo-bridge-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          memo-bridge-epc-server-processes)
    main-process))

(provide 'memo-bridge-epc)
;;; memo-bridge-epc.el ends here
