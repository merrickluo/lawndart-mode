;;; lawndart.el --- Major mode for editing dart  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Matthew Carter <m@ahungry.com>

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; Version: 0.0.1
;; Date: 2018-02-06
;; Keywords: languages, dart

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The dart-mode that exists doesn't properly indent, so give this a try.
;;
;; Much of this mode is derived from the js.el package.
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "lawndart-"; private names start with
;; "lawndart--".

;;; Code:

(require 'js)
(require 'flycheck)

;; configuration
(defcustom dart-sdk-path
  ;; Use Platform.resolvedExecutable so that this logic works through symlinks
  ;; and wrapper scripts.
  (-when-let (dart (executable-find "dart"))
    (dart--with-temp-file input
      (with-temp-file input (insert "
        import 'dart:io';
        void main() {
          print(Platform.resolvedExecutable);
        }
        "))
      (-when-let (result (dart--try-process dart input))
        (file-name-directory
         (directory-file-name
          (file-name-directory (string-trim result)))))))
  "The absolute path to the root of the Dart SDK."
  :group 'lawndart
  :type 'directory
  :package-version '(lawndart-mode . "1.0.0"))

(defcustom dart-debug nil
  "If non-nil, enables writing debug messages for dart-mode."
  :group 'lawndart
  :type 'bool)

(defun dart-executable-path ()
  "The absolute path to the 'dart' executable.

Returns nil if `dart-sdk-path' is nil."
  (when dart-sdk-path
    (concat dart-sdk-path
            (file-name-as-directory "bin")
            (if (memq system-type '(ms-dos windows-nt))
                "dart.exe"
              "dart"))))

;; analysis server
(cl-defstruct
    (dart--analysis-server
     (:constructor dart--make-analysis-server))
  "Struct containing data for an instance of a Dart analysis server.

The slots are:
- `process': the process of the running server.
- `buffer': the buffer where responses from the server are written."
  process buffer)

(defvar dart--analysis-server nil
  "The instance of the Dart analysis server we are communicating with.")

(defun dart--analysis-server-snapshot-path ()
  "The absolute path to the snapshot file that runs the Dart analysis server."
  (when dart-sdk-path
    (concat dart-sdk-path
            (file-name-as-directory "bin")
            (file-name-as-directory "snapshots")
            "analysis_server.dart.snapshot")))

;; helpers

(defun dart-info (msg)
  "Logs MSG to the dart log if `dart-debug' is non-nil."
  (when dart-debug (dart-log msg)))

(defun dart-log (msg)
  "Logs MSG to the dart log."
  (let* ((log-buffer (get-buffer-create "*dart-debug*"))
         (iso-format-string "%Y-%m-%dT%T%z")
         (timestamp-and-log-string
          (format-time-string iso-format-string (current-time))))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert "\n\n\n")
      (insert (concat timestamp-and-log-string
                      "\n"
                      msg))
      (insert "\n"))))

(defun dart--get (alist &rest keys)
  "Recursively calls `cdr' and `assoc' on ALIST with KEYS.
Returns the value rather than the full alist cell."
  (--reduce-from (cdr (assoc it acc)) alist keys))

(defmacro dart--json-let (json fields &rest body)
  "Assigns variables named FIELDS to the corresponding fields in JSON.
FIELDS may be either identifiers or (ELISP-IDENTIFIER JSON-IDENTIFIER) pairs."
  (declare (indent 2))
  (let ((json-value (make-symbol "json")))
    `(let ((,json-value ,json))
       (let ,(--map (if (symbolp it)
                        `(,it (dart--get ,json-value ',it))
                      (-let [(variable key) it]
                        `(,variable (dart--get ,json-value ',key))))
                    fields)
         ,@body))))

(defun dart--flash-highlight (offset length)
  "Briefly highlights the text defined by OFFSET and LENGTH.
OFFSET and LENGTH are expected to come from the analysis server,
rather than Elisp."
  (-let [overlay (make-overlay (+ 1 offset) (+ 1 offset length))]
    (overlay-put overlay 'face 'highlight)
    (run-at-time "1 sec" nil (lambda () (delete-overlay overlay)))))

(defvar dart-analysis-roots nil
  "The list of analysis roots that are known to the analysis server.

All Dart files underneath the analysis roots are analyzed by the analysis
server.")

(defvar dart--analysis-server-next-id 0
  "The ID to use for the next request to the Dart analysis server.")

(defvar dart--analysis-server-callbacks nil
  "An alist of ID to callback to be called when the analysis server responds.

Each request to the analysis server has an associated ID.  When the analysis
server sends a response to a request, it tags the response with the ID of the
request.  We look up the callback for the request in this alist and run it with
the JSON decoded server response.")

(defvar dart--analysis-server-subscriptions nil
  "An alist of event names to lists of callbacks to be called for those events.

These callbacks take the event object and an opaque subcription
object which can be passed to `dart--analysis-server-unsubscribe'.")

(defun dart--start-analysis-server-for-current-buffer ()
  "Initialize Dart analysis server for current buffer.

This starts Dart analysis server and adds either the pub root
directory or the current file directory to the analysis roots."
  (unless dart--analysis-server (dart-start-analysis-server))
  ;; TODO(hterkelsen): Add this file to the priority files.
  (dart-add-analysis-root-for-file)
  (add-hook 'first-change-hook 'dart-add-analysis-overlay t t)
  (add-hook 'after-change-functions 'dart-change-analysis-overlay t t)
  (add-hook 'after-save-hook 'dart-remove-analysis-overlay t t))

(defun dart-start-analysis-server ()
  (interactive)
  "Start the Dart analysis server.

Initializes analysis server support for all `dart-mode' buffers."
  (when dart--analysis-server
    (-let [process (dart--analysis-server-process dart--analysis-server)]
      (when (process-live-p process) (kill-process process)))
    (kill-buffer (dart--analysis-server-buffer dart--analysis-server)))

  (let* ((process-connection-type nil)
         (dart-process
          (start-process "dart-analysis-server"
                         "*dart-analysis-server*"
                         (dart-executable-path)
                         (dart--analysis-server-snapshot-path)
                         "--no-error-notification")))
    (set-process-query-on-exit-flag dart-process nil)
    (setq dart--analysis-server
          (dart--analysis-server-create dart-process)))

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'dart-mode)
        (dart--start-analysis-server-for-current-buffer)
        (when (buffer-modified-p buffer) (dart-add-analysis-overlay))))))

(defun dart--analysis-server-create (process)
  "Create a Dart analysis server from PROCESS."
  (-let [instance (dart--make-analysis-server
                   :process process
                   :buffer (generate-new-buffer (process-name process)))]
    (buffer-disable-undo (dart--analysis-server-buffer instance))
    (set-process-filter
     process
     (lambda (_ string)
       (dart--analysis-server-process-filter instance string)))
    instance))

(defun dart-add-analysis-overlay ()
  "Report to the Dart analysis server that it should overlay this buffer.

The Dart analysis server allows clients to 'overlay' file contents with
a client-supplied string.  This is needed because we want Emacs to report
errors for the current contents of the buffer, not whatever is saved to disk."
  ;; buffer-file-name can be nil within revert-buffer, but in that case the
  ;; buffer is just being reverted to its format on disk anyway.
  (when buffer-file-name
    (dart--analysis-server-send
     "analysis.updateContent"
     `((files .
              ((,buffer-file-name . ((type . "add")
                                     (content . ,(save-restriction
                                                   (widen)
                                                   (buffer-string)))))))))))

(defun dart-change-analysis-overlay
    (change-begin change-end change-before-length)
  "Report to analysis server that it should change the overlay for this buffer.

The region that changed ranges from CHANGE-BEGIN to CHANGE-END, and the
length of the text before the change is CHANGE-BEFORE-LENGTH. See also
`dart-add-analysis-overlay'."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files
      . ((,buffer-file-name
          . ((type . "change")
             (edits
              . (((offset . ,(- change-begin 1))
                  (length . ,change-before-length)
                  (replacement
                   . ,(buffer-substring change-begin change-end))))))))))))

(defun dart-remove-analysis-overlay ()
  "Remove the overlay for the current buffer since it has been saved.

See also `dart-add-analysis-overlay'."
  (dart--analysis-server-send
   "analysis.updateContent"
   `((files . ((,buffer-file-name . ((type . "remove"))))))))

(defun dart-add-analysis-root-for-file (&optional file)
  "Add the given FILE's root to the analysis server's analysis roots.

A file's root is the pub root if it is in a pub package, or the file's directory
otherwise.  If no FILE is given, then this will default to the variable
`buffer-file-name'."
  (let* ((file-to-add (or file buffer-file-name))
         (pub-root (locate-dominating-file file-to-add "pubspec.yaml"))
         (current-dir (file-name-directory file-to-add)))
    (if pub-root
        (dart-add-to-analysis-roots (expand-file-name pub-root))
      (dart-add-to-analysis-roots (expand-file-name current-dir)))))

(defun dart-add-to-analysis-roots (dir)
  "Add DIR to the analysis server's analysis roots.

The analysis roots are directories that contain Dart files. The analysis server
analyzes all Dart files under the analysis roots and provides information about
them when requested."
  (add-to-list 'dart-analysis-roots (directory-file-name dir))
  (dart--send-analysis-roots))

(defun dart--send-analysis-roots ()
  "Send the current list of analysis roots to the analysis server."
  (dart--analysis-server-send
   "analysis.setAnalysisRoots"
   `(("included" . ,dart-analysis-roots)
     ("excluded" . nil))))

(defun dart--analysis-server-send (method &optional params callback)
  "Send the METHOD request to the server with optional PARAMS.

PARAMS should be JSON-encodable.  If you provide a CALLBACK, it will be called
with the JSON decoded response.  Otherwise, the output will just be checked."
  (-let [req-without-id (dart--analysis-server-make-request method params)]
    (dart--analysis-server-enqueue req-without-id callback)))

(defun dart--analysis-server-make-request (method &optional params)
  "Construct a request for the analysis server.

The constructed request will call METHOD with optional PARAMS."
  `((method . ,method) (params . ,params)))

(defun dart--analysis-server-on-error-callback (response)
  "If RESPONSE has an error, report it."
  (-when-let (resp-err (assoc-default 'error response))
    (error "Analysis server error: %s" (assoc-default 'message resp-err))))

(defun dart--analysis-server-enqueue (req-without-id callback)
  "Send REQ-WITHOUT-ID to the analysis server, call CALLBACK with the result."
  (setq dart--analysis-server-next-id (1+ dart--analysis-server-next-id))
  (-let [request
         (json-encode (cons (cons 'id (format "%s" dart--analysis-server-next-id))
                            req-without-id))]

    ;; Enqueue the request so that we can be sure all requests are processed in
    ;; order.
    (push (cons dart--analysis-server-next-id
                (or callback #'dart--analysis-server-on-error-callback))
          dart--analysis-server-callbacks)

    (cond
     ((not dart--analysis-server)
      (message "Starting Dart analysis server.")
      (dart-start-analysis-server))
     ((not (process-live-p (dart--analysis-server-process dart--analysis-server)))
      (message "Dart analysis server crashed, restarting.")
      (dart-start-analysis-server)))

    (dart-info (concat "Sent: " request))
    (process-send-string (dart--analysis-server-process dart--analysis-server)
                         (concat request "\n"))))

(cl-defun dart--analysis-server-process-filter (das string)
  "Handle the event or method response from the dart analysis server.

The server DAS has STRING added to the buffer associated with it.
Method responses are paired according to their pending request and
the callback for that request is given the json decoded response."
  (-let [buf (dart--analysis-server-buffer das)]
    ;; The buffer may have been killed if the server was restarted
    (unless (buffer-live-p buf)
      (cl-return-from dart--analysis-server-process-filter))

    ;; We use a buffer here because emacs might call the filter before the
    ;; entire line has been written out. In this case we store the
    ;; unterminated line in a buffer to be read when the rest of the line is
    ;; output.
    (with-current-buffer buf
      (goto-char (point-max))
      (insert string)
      (-let [buf-lines (s-lines (buffer-string))]
        (delete-region (point-min) (point-max))
        (insert (-last-item buf-lines))

        (-let [messages
               (--filter (and it (not (string-empty-p it)))
                         (-butlast buf-lines))]
          (dolist (message messages)
            (dart-info (concat "Received: " message))
            (dart--analysis-server-handle-msg
             (-let [json-array-type 'list]
               (json-read-from-string message)))))))))

(defun dart--analysis-server-handle-msg (msg)
  "Handle the parsed MSG from the analysis server."
  (-if-let* ((raw-id (dart--get msg 'id))
             (id (string-to-number raw-id)))
      ;; This is a response to a request, so we should invoke a callback in
      ;; dart--analysis-server-callbacks.
      (-if-let (resp-closure (dart--get dart--analysis-server-callbacks id))
          (progn
            (setq dart--analysis-server-callbacks
                  (assq-delete-all id dart--analysis-server-callbacks))
            (funcall resp-closure msg))
        (-if-let (err (dart--get msg 'error))
            (dart--analysis-server-on-error-callback msg)
          (dart-info (format "No callback was associated with id %s" raw-id))))

    ;; This is a notification, so we should invoke callbacks in
    ;; dart--analysis-server-subscriptions.
    (-when-let* ((event (dart--get msg 'event))
                 (params (dart--get msg 'params))
                 (callbacks (dart--get dart--analysis-server-subscriptions event)))
      (dolist (callback callbacks)
        (-let [subscription (cons event callback)]
          (funcall callback params subscription))))))

(defun dart--analysis-server-subscribe (event callback)
  "Registers CALLBACK to be called for each EVENT of the given type.

CALLBACK should take two parameters: the event object and an
opaque subscription object that can be passed to
`dart--analysis-server-unsubscribe'. Returns the same opaque
subscription object."
  (-if-let (cell (assoc event dart--analysis-server-subscriptions))
      (nconc cell (list callback))
    (push (cons event (list callback)) dart--analysis-server-subscriptions))
  (cons event callback))

(defun dart--analysis-server-unsubscribe (subscription)
  "Unregisters the analysis server SUBSCRIPTION.

SUBSCRIPTION is an opaque object provided by
`dart--analysis-server-subscribe'."
  (-let [(event . callback) subscription]
    (delq callback (assoc event dart--analysis-server-subscriptions))))
;;;; Flycheck Error Reporting

(defun dart--flycheck-start (_ callback)
  "Run the CHECKER and report the errors to the CALLBACK."
  (dart-info (format "Checking syntax for %s" (current-buffer)))
  (dart--analysis-server-send
   "analysis.getErrors"
   `((file . ,(buffer-file-name)))
   (-let [buffer (current-buffer)]
     (lambda (response)
       (dart--report-errors response buffer callback)))))

(when (featurep 'flycheck)
  (flycheck-define-generic-checker 'dart-analysis-server
    "Checks Dart source code for errors using Dart analysis server."
    :start 'dart--flycheck-start
    :modes '(lawndart-mode))
  (add-to-list 'flycheck-checkers 'dart-analysis-server))

(defun dart--report-errors (response buffer callback)
  "Report the errors returned from the analysis server.

The errors contained in RESPONSE from Dart analysis server run on BUFFER are
reported to CALLBACK."
  (dart-info (format "Reporting to flycheck: %s" response))
  (-let [fly-errors (--map (dart--to-flycheck-err it buffer)
                           (dart--get response 'result 'errors))]
    (dart-info (format "Parsed errors: %s" fly-errors))
    (funcall callback 'finished fly-errors)))

(defun dart--to-flycheck-err (err buffer)
  "Create a flycheck error from a dart ERR in BUFFER."
  (flycheck-error-new
   :buffer buffer
   :checker 'dart-analysis-server
   :filename (dart--get err 'location 'file)
   :line (dart--get err 'location 'startLine)
   :column (dart--get err 'location 'startColumn)
   :message (dart--get err 'message)
   :level (dart--severity-to-level (dart--get err 'severity))))

(defun dart--severity-to-level (severity)
  "Convert SEVERITY to a flycheck level."
  (cond
   ((string= severity "INFO") 'info)
   ((string= severity "WARNING") 'warning)
   ((string= severity "ERROR") 'error)))

;;;; Hover

(defun dart-show-hover (&optional show-in-buffer)
  "Displays hover information for the current point.

With a prefix argument, opens a new buffer rather than using the
minibuffer."
  (interactive "P")
  (-when-let (filename (buffer-file-name))
    (let ((show-in-buffer show-in-buffer)
          (buffer (current-buffer))
          (pos (point)))
      (dart--analysis-server-send
       "analysis.getHover"
       `(("file" . ,filename) ("offset" . ,pos))
       (lambda (response)
         (-when-let (hover (car (dart--get response 'result 'hovers)))
           (dart--json-let hover
               (offset
                length
                dartdoc
                (element-description elementDescription)
                (element-kind elementKind)
                (is-deprecated isDeprecated)
                parameter)
             (setq is-deprecated (not (eq is-deprecated :json-false)))

             ;; Briefly highlight the region that's being shown.
             (with-current-buffer buffer
               (dart--flash-highlight offset length))

             (with-temp-buffer
               (when is-deprecated
                 (insert (dart--face-string "DEPRECATED" 'font-lock-warning-face) ?\n))

               (when element-description
                 (insert (dart--highlight-description element-description)
                         (dart--face-string (concat " (" element-kind ")") 'italic))
                 (when (or dartdoc parameter) (insert ?\n)))
               (when parameter
                 (insert
                  (dart--highlight-description parameter)
                  (dart--face-string " (parameter type)" 'italic))
                 (when dartdoc) (insert ?\n))
               (when dartdoc
                 (when (or element-description parameter) (insert ?\n))
                 (insert (dart--highlight-dartdoc dartdoc (not show-in-buffer))))

               (let ((text (buffer-string)))
                 (if show-in-buffer
                     (with-current-buffer-window
                      "*Dart Analysis*" nil nil
                      (insert text)
                      (dart-popup-mode)

                      (setq dart--do-it-again-callback
                            (lambda ()
                              (save-excursion
                                (with-current-buffer buffer
                                  (goto-char pos)
                                  (dart-show-hover t))))))
                   (message "%s" text)))))))))))

;;;; Navigation

(defun dart-goto ()
  (interactive)
  (-when-let (filename (buffer-file-name))
    (dart--analysis-server-send
     "analysis.getNavigation"
     `(("file" . ,filename) ("offset" . ,(point)) ("length" . 0))
     (lambda (response)
       (-when-let (result (dart--get response 'result))
         (dart--json-let result (files targets regions)
           (-when-let (region (car regions))
             (let* ((target-index (car (dart--get region 'targets)))
                    (target (elt targets target-index))

                    (file-index (dart--get target 'fileIndex))
                    (offset (dart--get target 'offset))
                    (length (dart--get target 'length))

                    (file (elt files file-index)))
               (find-file file)
               (goto-char (+ 1 offset))
               (dart--flash-highlight offset length)))))))))

;;;; Search

(defun dart-find-refs (pos &optional include-potential)
  (interactive "dP")
  (-when-let (filename (buffer-file-name))
    (dart--analysis-server-send
     "search.findElementReferences"
     `(("file" . ,filename)
       ("offset" . ,pos)
       ("includePotential" . ,(or include-potential json-false)))
     (let ((buffer (current-buffer))
           (include-potential include-potential))
       (lambda (response)
         (-when-let (result (dart--get response 'result))
           (let ((name (dart--get result 'element 'name))
                 (location (dart--get result 'element 'location)))
             (dart--display-search-results
              (dart--get result 'id)
              (lambda ()
                (setq dart--do-it-again-callback
                      (lambda ()
                        (with-current-buffer buffer
                          (dart-find-refs pos include-potential))))

                (insert "References to ")
                (insert-button
                 name
                 'action (lambda (_) (dart--goto-location location)))
                (insert ":\n\n"))))))))))

(defun dart-find-member-decls (name)
  "Find member declarations named NAME."
  (interactive "sMember name: ")
  (dart--find-by-name
   "search.findMemberDeclarations" "name" name "Members named "))

(defun dart-find-member-refs (name)
  "Find member references named NAME."
  (interactive "sMember name: ")
  (dart--find-by-name
   "search.findMemberReferences" "name" name "References to "))

(defun dart-find-top-level-decls (name)
  "Find top-level declarations named NAME."
  (interactive "sDeclaration name: ")
  (dart--find-by-name
   "search.findTopLevelDeclarations" "pattern" name "Declarations matching "))

(defun dart--find-by-name (method argument name header)
  "A helper function for running an analysis server search for NAME.

Calls the given analysis server METHOD passing NAME to the given
ARGUMENT. Displays a header beginning with HEADER in the results."
  (dart--analysis-server-send
   method
   (list (cons argument name))
   (lambda (response)
     (-when-let (id (dart--get response 'result 'id))
       (dart--display-search-results
        id
        (lambda ()
          (setq dart--do-it-again-callback
                (lambda ()
                  (dart--find-by-name method argument name header)))
          (insert header name ":\n\n")))))))

(defun dart--display-search-results (search-id callback)
  "Displays search results with the given SEARCH-ID.

CALLBACK is called with no arguments in the search result buffer
to add a header and otherwise prepare it for displaying results."
  (let (buffer
        beginning-of-results
        (total-results 0))
    (with-current-buffer-window
     "*Dart Search*" nil nil
     (dart-popup-mode)
     (setq buffer (current-buffer))
     (funcall callback)
     (setq beginning-of-results (point))

     (dart--analysis-server-subscribe
      "search.results"
      (lambda (event subscription)
        (with-current-buffer buffer
          (dart--json-let event (id results (is-last isLast))
            (when (equal id search-id)
              (-let [buffer-read-only nil]
                (save-excursion
                  (goto-char (point-max))
                  (dolist (result results)
                    (let ((location (dart--get result 'location))
                          (path (dart--get result 'path))
                          (start (point)))
                      (dart--fontify-excursion '(compilation-info underline)
                        (when (cl-some
                               (lambda (element)
                                 (equal (dart--get element 'kind) "CONSTRUCTOR"))
                               path)
                          (insert "new "))

                        (insert
                         (->> path
                              (--remove (member (dart--get it 'kind)
                                                '("COMPILATION_UNIT" "FILE" "LIBRARY" "PARAMETER")))
                              (--map (dart--get it 'name))
                              (-remove 'string-empty-p)
                              nreverse
                              (s-join ".")))

                        (make-text-button
                         start (point)
                         'action (lambda (_) (dart--goto-location location))))

                      (dart--json-let location (file (line startLine) (column startColumn))
                        (insert " " file ":"
                                (dart--face-string line 'compilation-line-number) ":"
                                (dart--face-string column 'compilation-column-number) ?\n)))))

                (setq total-results (+ total-results (length results)))

                (when (eq is-last t)
                  (dart--analysis-server-unsubscribe subscription)
                  (save-excursion
                    (goto-char (point-max))
                    (insert "\nFound " (dart--face-string total-results 'bold) " results."))))))))))

    (select-window (get-buffer-window buffer))
    (goto-char beginning-of-results)))

(defun dart--goto-location (location)
  "Sends the user to the analysis server LOCATION."
  (dart--json-let location (file offset length)
    (find-file file)
    (goto-char (+ 1 offset))
    (dart--flash-highlight offset length)))


(defvar lawndart-font-lock-keywords-1
      (list
       '("[[:space:]]*\\(\\w*?\\):" 1 font-lock-builtin-face)
       '("^library " . font-lock-keyword-face)
       '("@override" . font-lock-doc-face)
       '("[\t ]*\\(if\\|then\\|else\\|interface\\|pred\\|func\\|module\\|implementation\\)" . font-lock-keyword-face)
       '("[[:space:]{($]\\(_*[[:upper:]]+[[:upper:][:lower:]_$0-9]*\\)" 1 font-lock-type-face)
       '("[[:space:]$]_*[[:upper:]]+[[:upper:][:lower:]_$0-9]*" . font-lock-function-name-face)
       '("\\([[:lower:]_$0-9]*?\\)" 1 font-lock-variable-name-face)
       '("\\([[:upper:][:lower:]_$0-9]*?\\):" 1 font-lock-negation-char-face)
       '("\\(\\w+\\)(" 1 font-lock-function-name-face)
       '("<\\(\\w+\\)>" 1 font-lock-type-face)
       '("<\\(\\w+\\)," 1 font-lock-type-face)
       ))

(defvar lawndart-font-lock-keywords
  (append
   lawndart-font-lock-keywords-1
   js--font-lock-keywords-3
   js--font-lock-keywords-1
   js--font-lock-keywords-2))

;;;###autoload
(define-derived-mode lawndart-mode js-mode "Dart"
  "Major mode for editing dart."
  :group 'js
  (setq-local indent-line-function #'js-indent-line)
  (setq-local beginning-of-defun-function #'js-beginning-of-defun)
  (setq-local end-of-defun-function #'js-end-of-defun)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local font-lock-defaults
              (list '(lawndart-font-lock-keywords)
                    nil nil nil nil
                    '(font-lock-syntactic-face-function
                      . js-font-lock-syntactic-face-function)))
  (setq-local syntax-propertize-function #'js-syntax-propertize)
  (setq-local prettify-symbols-alist js--prettify-symbols-alist)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local which-func-imenu-joiner-function #'js--which-func-joiner)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local fill-paragraph-function #'js-c-fill-paragraph)
  (setq-local normal-auto-fill-function #'js-do-auto-fill)

  ;; Parse cache
  (add-hook 'before-change-functions #'js--flush-caches t t)

  ;; Frameworks
  (js--update-quick-match-re)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (setq imenu-create-index-function #'js--imenu-create-index)

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "\\(@[[:alpha:]]+\\>\\|$\\)"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-line-break-function #'c-indent-new-comment-line)
  (setq-local c-block-comment-start-regexp "/\\*")
  (setq-local comment-multi-line t)

  (setq-local electric-indent-chars
	      (append "{}():;," electric-indent-chars)) ;FIXME: js2-mode adds "[]*".
  (setq-local electric-layout-rules
	      '((?\; . after) (?\{ . after) (?\} . before)))

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expression literal and the problem
  ;; will mysteriously disappear.
  ;; FIXME: We should instead do this fontification lazily by adding
  ;; calls to syntax-propertize wherever it's really needed.
  ;;(syntax-propertize (point-max))

  (dart--start-analysis-server-for-current-buffer)
  )

;;;###autoload (defalias 'javascript-mode 'js-mode)

(eval-after-load 'folding
  '(when (fboundp 'folding-add-to-marks-list)
     (folding-add-to-marks-list 'lawndart-mode "// {{{" "// }}}" )))

;;;###autoload
(dolist (name (list "dart"))
  (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'lawndart-mode)))

(provide 'lawndart)

;;; lawndart.el ends here
