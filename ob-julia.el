;;; ob-julia.el --- org-babel functions for julia code evaluation

;; Copyright (C) 2013
;; Author: G. Jay Kerns, based on ob-R.el by Eric Schulte and Dan Davison

;; Org-Babel support for evaluating julia code

;;; Code:
(require 'cl-lib)
(require 'ob)

(declare-function orgtbl-to-csv "org-table" (table params))
;; (declare-function julia "ext:ess-julia" (&optional start-args))
;; (declare-function inferior-ess-send-input "ext:ess-inf" ())
;; (declare-function ess-make-buffer-current "ext:ess-inf" ())
;; (declare-function ess-eval-buffer "ext:ess-inf" (vis))
;; (declare-function ess-wait-for-process "ext:ess-inf"
;; 		  (&optional proc sec-prompt wait force-redisplay))

(defvar org-babel-header-args:julia
  '((width		 . :any)
    (horizontal		 . :any)
    (results             . ((file list vector table scalar verbatim)
			    (raw org html latex code pp wrap)
			    (replace silent append prepend)
			    (output value graphics))))
  "julia-specific header arguments.")

(add-to-list 'org-babel-tangle-lang-exts '("julia" . "jl"))

(defvar org-babel-default-header-args:julia '())

(defcustom org-babel-julia/ess-installed nil
  "Integrate ob-julia with ESS, and assume ESS is installed"
  :group 'org-babel
  :version "24.3"
  :type 'boolean)

(defcustom org-babel-julia/command "julia"
  "Name of command to use for executing julia code."
  :group 'org-babel
  :version "24.3"
  :type 'string)

;; Disable the fancy Julia banner, because it has backticks and stuff
(defcustom org-babel-julia/process-args '("--banner=no")
  "Arguments to pass to the Julia process"
  :group 'org-babel
  :version "24.3"
  :type 'list)

;; (defvar ess-current-process-name) ; dynamically scoped
;; (defvar ess-local-process-name) ; dynamically scoped
(defun org-babel-edit-prep:julia (info)
  (let ((session (cdr (assq :session (nth 2 info)))))
    (when (and session
	       (string-prefix-p "*"  session)
	       (string-suffix-p "*" session))
      (org-babel-julia/initiate-session session nil))))

(defun org-babel-expand-body:julia (body params &optional _graphics-file)
  "Expand BODY according to PARAMS, return the expanded body."
  (mapconcat 'identity
	     (append
	      (when (cdr (assq :prologue params))
		(list (cdr (assq :prologue params))))
	      (org-babel-variable-assignments:julia params)
	      (list body)
	      (when (cdr (assq :epilogue params))
		(list (cdr (assq :epilogue params)))))
	     "\n"))

(defun org-babel-execute:julia (body params)
  "Execute a block of julia code.
This function is called by `org-babel-execute-src-block'."
  (save-excursion
    (let* ((result-params (cdr (assq :result-params params)))
	   (result-type (cdr (assq :result-type params)))
           (session (org-babel-julia/initiate-session
		     (cdr (assq :session params)) params))
	   (graphics-file (and (member "graphics" (assq :result-params params))
			       (org-babel-graphical-output-file params)))
	   (colnames-p (unless graphics-file (cdr (assq :colnames params))))
	   (rownames-p (unless graphics-file (cdr (assq :rownames params))))
	   (full-body (org-babel-expand-body:julia body params graphics-file))
	   (result
	    (org-babel-julia/evaluate
	     session full-body result-type result-params
	     (or (equal "yes" colnames-p)
		 (org-babel-pick-name
		  (cdr (assq :colname-names params)) colnames-p))
	     (or (equal "yes" rownames-p)
		 (org-babel-pick-name
		  (cdr (assq :rowname-names params)) rownames-p)))))
      (if graphics-file nil result))))

                                        ; Code from https://github.com/gjkerns/ob-julia/pull/14
                                        ;        (if graphics-file nil (if (and result (sequencep result))
                                        ;                                  (org-babel-normalize-newline result)
                                        ;                                result)))))

(defun org-babel-normalize-newline (result)
  (replace-regexp-in-string
   "\\(\n\r?\\)\\{2,\\}"
   "\n"
   result))

(defun org-babel-prep-session:julia (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-julia/initiate-session session params))
	 (var-lines (org-babel-variable-assignments:julia params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:julia (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:julia session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:julia (params)
  "Return list of julia statements assigning the block's variables."
  (let ((vars (org-babel--get-vars params)))
    (mapcar
     (lambda (pair)
       (org-babel-julia/assign-elisp
	(car pair) (cdr pair)
	(equal "yes" (cdr (assq :colnames params)))
	(equal "yes" (cdr (assq :rownames params)))))
     (mapcar
      (lambda (i)
	(cons (car (nth i vars))
	      (org-babel-reassemble-table
	       (cdr (nth i vars))
	       (cdr (nth i (cdr (assq :colname-names params))))
	       (cdr (nth i (cdr (assq :rowname-names params)))))))
      (number-sequence 0 (1- (length vars)))))))

(defun org-babel-julia/quote-csv-field (s)
  "Quote field S for export to julia."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun org-babel-julia/assign-elisp (name value colnames-p rownames-p)
  "Construct julia code assigning the elisp VALUE to a variable named NAME."
  (if (listp value)
      (let* ((lengths (mapcar 'length (cl-remove-if-not 'sequencep value)))
             (max (if lengths (apply 'max lengths) 0))
             (min (if lengths (apply 'min lengths) 0)))
        ;; Ensure VALUE has an orgtbl structure (depth of at least 2).
        (unless (listp (car value)) (setq value (list value)))
        (let ((file (orgtbl-to-tsv value '(:fmt org-babel-julia/quote-tsv-field)))
              (header (if (or (eq (nth 1 value) 'hline) colnames-p)
                          "TRUE" "FALSE"))
              (row-names (if rownames-p "1" "NULL")))
          (if (= max min)
              (format "%s = begin
    using CSV
    CSV.read(\"%s\")
end" name file)
            (format "%s = begin
    using CSV
    CSV.read(\"%s\")
end"
                    name file))))
    (format "%s = %s" name (org-babel-julia/quote-csv-field value))))

(defun org-babel-julia/start-julia (session process-args)
  (interactive "P")
  (when (null org-babel-julia/command)
    (error "'org-babel-julia/command' hasn't been assigned-- is julia-mode installed?"))
  (let* ((inf-buf-name (format "*Julia%s*" (concat ":" session)))
         (inf-buf (get-buffer-create inf-buf-name)))
    (with-current-buffer inf-buf
      )))

(defun org-babel-julia/graphical-output-file (params)
  "Name of file to which julia should send graphical output."
  (and (member "graphics" (cdr (assq :result-params params)))
       (cdr (assq :file params))))

(defconst org-babel-julia/eoe-indicator "print(\"org_babel_julia_eoe\")")
(defconst org-babel-julia/eoe-output "org_babel_julia_eoe")

(defun org-babel-julia/evaluate
    (session body result-type result-params column-names-p row-names-p)
  "Evaluate julia code in BODY."
  (let* ((code-file (org-babel-temp-file "julia-input-"))
         (result-file (org-babel-temp-file "julia-output-"))
         (session-string-bool (if session "true" "false"))
         ;; For more on this execution, look at this location in the Julia library:
         ;; [[file:src/OrgBabel.jl::function execute_julia(input_file::String, output_file::String, is_in_session::Bool)]]
         ;; %S surrounds values with "double quotes"
         (eval-string (format "OrgBabel.execute_julia(%S, %S, %s)"
                              code-file result-file session-string-bool)))
    (cl-case result-type
      (value
       ;; Try to touch `body' code as little as possible. Immediately throw it
       ;; to a temp file, and provide our Julia process with only the filename,
       ;; and let OrgBabel.jl handle *all* of the session logic.
       ;; The filesystem handles all of the escaping. I don't think this is the
       ;; org-babel way to do things, but man do I hate excessive escaping for
       ;; untrusted user code.
       (with-temp-buffer
         (insert body)
         (write-region (point-min) (point-max) code-file))
       (org-babel-eval org-babel-julia/command eval-string)
       (org-babel-julia/process-value-result
        (org-babel-result-cond result-params
          (with-temp-buffer
            (insert-file-contents result-file)
            (buffer-string))
          (org-babel-import-elisp-from-file result-file '(4)))
        column-names-p))
      (output (org-babel-eval org-babel-julia/command eval-string)))))

(defun org-babel-julia/process-value-result (result column-names-p)
  "julia-specific processing of return value.
Insert hline if column names in output have been requested."
  (if column-names-p
      (cons (car result) (cons 'hline (cdr result)))
    result))

(provide 'ob-julia)

;;; ob-julia.el ends here
