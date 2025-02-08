;; Load cl-ppcre
(ql:quickload "cl-ppcre")

;; Make the package available
(defpackage :my-c-converter
(:use :cl)  ; Only use CL package, import cl-ppcre functions explicitly
  (:import-from :cl-ppcre 
                :scan                     ; for regex-search
                :regex-replace-all        ; for regex-replace-all
                :register-groups-bind     
                :create-scanner           ; needed for some regex operations
                :all-matches             ; for multiple matches
                :split)                   ; for string splitting operations
  (:export :convert-c-to-lisp))

  ; (:use :cl :cl-ppcre)
  ; (:export :convert-c-to-lisp))

(in-package :my-c-converter)
;;;; C to Lisp Code Converter

;; Line type definitions using defparameter
(defparameter *line-types*
  '(:if-statement "if"
    :loop-for "for"
    :var-assignment "assignment"
    :var-definition "definition"
    :func-definition "function_def"
    :func-prototype "function_prototype"
    :func-call "function_call"
    :func-return "return"
    :block-start "block_start"
    :block-end "block_end"
    :empty "empty"))

;; Custom string manipulation functions
(defun string-contains (substring string)
  "Check if string contains substring."
  (search substring string))

(defun string-starts-with (prefix string)
  "Check if string starts with prefix."
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun string-ends-with (suffix string)
  "Check if string ends with suffix."
  (and (>= (length string) (length suffix))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(defun string-trim-whitespace (str)
  "Remove whitespace from both ends of string."
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun empty-string-p (str)
  "Check if string is empty or only whitespace."
  (or (null str)
      (string= "" (string-trim-whitespace str))))
(defun split-and-clean-params (params-str)
  "Split parameter string and clean each parameter."
  (when (and params-str (string/= params-str ""))
    (mapcar #'string-trim-whitespace
            (cl-ppcre:split "," params-str))))

(defun extract-param-name (param-str)
  "Extract parameter name from parameter declaration."
  (car (last (cl-ppcre:split "\\s+" (string-trim-whitespace param-str)))))

(defun split-params (params-str)
  "Split parameter string into list of parameters."
  (when (and params-str (not (string= params-str "")))
    (loop for param in (cl-ppcre:split "," params-str)
          collect (string-trim-whitespace param))))

;; Type mapping for C to Lisp types
(defparameter *type-map* 
  '(("int" . "integer")
    ("float" . "single-float")
    ("double" . "double-float")
    ("char" . "character")))


(defstruct block-info
  type           ; Type of block (:func-definition, :if-statement, etc.)
  parent         ; Parent block
  needs-closing  ; Whether block needs closing parenthesis
  let-vars       ; List of (var-name . initial-value) pairs for let bindings
  indent-level   ; Current indentation level
  in-let-block)  ; Flag to indicate if we're inside a let block


;; line type determination function
(defun determine-line-type (line)
  "Determine the type of C code line."
  (let ((trimmed-line (string-trim-whitespace line)))
    (cond
      ((empty-string-p trimmed-line) :empty)
      ((and (string-contains "(" trimmed-line)
            (string-contains ")" trimmed-line)
            (string-ends-with ";" trimmed-line)
            (cl-ppcre:scan "\\w+\\s+\\w+\\s*\\([^)]*\\);" trimmed-line))
       :func-prototype)
      ((and (cl-ppcre:scan "\\w+\\s+\\w+\\s*\\([^)]*\\)\\s*{?" trimmed-line)
            (not (string-ends-with ";" trimmed-line)))
       :func-definition)
      ((string-starts-with "if" trimmed-line) :if-statement)
      ((string-starts-with "for" trimmed-line) :loop-for)
      ((cl-ppcre:scan "^(int|char|float|double|void)\\s+\\w+" trimmed-line)
       :var-definition)
      ((and (string-contains "(" trimmed-line)
            (string-contains ")" trimmed-line)
            (string-ends-with ";" trimmed-line))
       :func-call)
      ((string-starts-with "return" trimmed-line) :func-return)
      ((string= trimmed-line "{") :block-start)
      ((string= trimmed-line "}") :block-end)
      ((and (string-contains "=" trimmed-line)
            (string-ends-with ";" trimmed-line))
       :var-assignment)
      (t :empty))))

;; Conversion functions
(defun convert-condition (condition)
  "Convert C condition to Lisp condition."
  (let ((result (string-trim-whitespace condition)))
    ;; Handle different parts of the condition
    (setf result (cl-ppcre:regex-replace-all "\\s+" result " ")) ; normalize spaces
    (setf result (cl-ppcre:regex-replace-all "([^=!<>])=([^=])" result "\\1==\\2"))
    (setf result (cl-ppcre:regex-replace-all "==" result "="))
    (setf result (cl-ppcre:regex-replace-all "!=" result "/="))
    (setf result (cl-ppcre:regex-replace-all "&&" result "and"))
    (setf result (cl-ppcre:regex-replace-all "\\|\\|" result "or"))
    (setf result (cl-ppcre:regex-replace-all "!" result "not "))
    
    ;; Convert comparison expressions to prefix notation
    (loop for op in '(">" "<" ">=" "<=" "=" "/=")
          do (cl-ppcre:register-groups-bind (left right)
                 ((format nil "(\\w+|\\d+)\\s*~A\\s*(\\w+|\\d+)" op) result)
               (when (and left right)
                 (setf result (format nil "(~A ~A ~A)" op left right)))))
    result))

(defun convert-printf (line)
  "Convert C printf to Lisp format."
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "printf\\s*\\(\"([^\"]+)\"(?:\\s*,\\s*([^)]+))?\\)" line)
    (when match
      (let ((format-str (aref regs 0))
            (args (aref regs 1)))
        (setf format-str 
              (cl-ppcre:regex-replace-all "%d" 
                (cl-ppcre:regex-replace-all "\\\\n" format-str "~%")
                "~d"))
        (format nil "(format t \"~A\"~@[ ~A~])" format-str args)))))

(defun convert-if-statement (line)
  "Convert C if statement to Lisp if with proper block handling."
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "if\\s*\\((.*)\\)" line)
    (when match
      (values
       (format nil "(if ~A" (convert-condition (aref regs 0)))
       1
       (list :type :if-statement 
             :needs-closing t
             :is-if-block t)))))

;; For loop conversion
(defun convert-for-loop (line)
  "Convert C for loop to Lisp loop."
  (multiple-value-bind (matched groups)
      (cl-ppcre:scan-to-strings "for\\s*\\(([^;]+);([^;]+);([^;]+)\\)" line)
    (when matched
      (let* ((init (aref groups 0))
             (condition (aref groups 1))
             (increment (aref groups 2))  ; Keep increment for possible future use
             (block-info (list :type :loop-for 
                              :needs-closing t
                              :increment increment)))  ; Store in block info
        (multiple-value-bind (init-matched init-groups)
            (cl-ppcre:scan-to-strings "(\\w+)\\s+(\\w+)\\s*=\\s*(\\d+)" init)
          (when init-matched
            (let ((var-name (aref init-groups 1))
                  (start-val (aref init-groups 2)))
              (multiple-value-bind (cond-matched cond-groups)
                  (cl-ppcre:scan-to-strings "(\\w+)\\s*<\\s*(\\d+)" condition)
                (when (and cond-matched 
                         (string= (aref cond-groups 0) var-name))
                  (values
                   (format nil "(loop for ~A from ~A below ~A do"
                           var-name start-val (aref cond-groups 1))
                   1
                   block-info))))))))))

;; Function definition conversion
(defun convert-func-definition (line)
  "Convert C function definition to Lisp defun with proper block handling."
  (cl-ppcre:register-groups-bind 
      (return-type name params)
      ("(\\w+)\\s+(\\w+)\\s*\\((.*)\\)" line)
    (if (and return-type name)
        (let ((param-list
               (loop for param in (cl-ppcre:split "," params)
                     when (string/= (string-trim-whitespace param) "")
                     collect (car (last (cl-ppcre:split "\\s+"
                                                      (string-trim-whitespace param)))))))
          (values
           (format nil "(defun ~A (~{~A~^ ~})"
                   name
                   param-list)
           1
           (list :type :func-definition
                 :needs-closing t
                 :name name
                 :is-func-block t)))
        (values "" 0 nil))))


;; Arithmetic expression conversion
(defun convert-arithmetic (expr)
  "Convert C arithmetic expressions to Lisp format."
  (let ((cleaned-expr (string-trim-whitespace 
                      (string-trim '(#\;) 
                                 (string-trim-whitespace expr)))))
    ;; check if it's a function call
    (when (and (search "(" cleaned-expr) 
               (search ")" cleaned-expr))
      (cl-ppcre:register-groups-bind (func-name args)
          ("(\\w+)\\s*\\((.*)\\)" cleaned-expr)
        (when func-name
          (let ((arg-list
                 (mapcar #'(lambda (arg) 
                            (convert-arithmetic (string-trim-whitespace arg)))
                        (cl-ppcre:split "," args))))
            (return-from convert-arithmetic 
              (format nil "(~A ~{~A~^ ~})" func-name arg-list))))))
    
    ;; Then handle arithmetic operators
    (loop for op in '("+" "-" "*" "/" "%")
          when (search op cleaned-expr)
          do (let* ((parts (cl-ppcre:split (concatenate 'string "\\" op) cleaned-expr))
                    (left (string-trim-whitespace (first parts)))
                    (right (string-trim-whitespace (second parts))))
               (when (and left right)  ; Only convert if both operands exists
                 (return-from convert-arithmetic
                   (format nil "(~A ~A ~A)"
                          op
                          (convert-arithmetic left)
                          (convert-arithmetic right))))))
    ;; If no operations found, return the original expression
    cleaned-expr))

;; Function return conversion
(defun convert-func-return (line)
  "Convert C return statement to Lisp return value."
  (let ((value (string-trim-whitespace 
                (string-trim '(#\;) 
                           (cl-ppcre:regex-replace "return" line "")))))
    (values
     (if (string= value "")
         "nil"
         (convert-arithmetic value))
     0)))
;; Function call conversion
(defun convert-func-call (line)
  "Convert C function call to Lisp function call."
  (let ((cleaned-line (string-trim-whitespace 
                      (string-trim '(#\;) line))))
    (cond
      ;; Handle printf
      ((search "printf" cleaned-line)
       (values (convert-printf cleaned-line) 0))
      
      ;; Handle assignment with arithmetic
      ((cl-ppcre:register-groups-bind (var-name expression)
           ("(\\w+)\\s*=\\s*(.+)" cleaned-line)
         (values
          (format nil "setf (~A ~A)"
                  var-name
                  (convert-arithmetic expression))
          0)))
      
      ;; Handle regular function calls
      ((cl-ppcre:register-groups-bind (func-name args)
           ("(\\w+)\\s*\\((.*)\\)" cleaned-line)
         (let ((arg-list
                (mapcar #'(lambda (arg) 
                           (convert-arithmetic (string-trim-whitespace arg)))
                       (cl-ppcre:split "," args))))
           (values
            (format nil "(~A ~{~A~^ ~})"
                    func-name
                    arg-list)
            0))))
      
      (t (values "" 0)))))

;; Function prototype conversion
(defun convert-func-prototype (line)
  "Convert C function prototype (declaration) to Lisp declaim."
  (cl-ppcre:register-groups-bind 
      (return-type name params)
      ("(\\w+)\\s+(\\w+)\\s*\\((.*)\\);" line)
    (if (and return-type name)
        (let ((lisp-return-type (or (cdr (assoc return-type *type-map* :test #'string=))
                                   return-type))
              (param-types
               (when (and params (not (string= params "")))
                 (loop for param in (cl-ppcre:split "," params)
                       for param-type = (first (cl-ppcre:split "\\s+" (string-trim-whitespace param)))
                       collect (or (cdr (assoc param-type *type-map* :test #'string=))
                                 param-type)))))
          (values
           (format nil "(declaim (ftype (function (~{~A~^ ~}) ~A) ~A))"
                   param-types
                   lisp-return-type
                   name)
           0))
        (values "" 0))))


(defun convert-var-definition (line)
  "Convert C variable definition to Lisp let binding with proper indentation."
  (let* ((clean-line (string-trim '(#\;) line))
         (parts (cl-ppcre:split "=" clean-line)))
    (let* ((var-decl (string-trim-whitespace (first parts)))
           (var-name (car (last (cl-ppcre:split "\\s+" var-decl)))))
      (if (> (length parts) 1)
          (let ((value (string-trim-whitespace (second parts))))
            (values
             (format nil "setf (~A ~A)"
                     var-name
                     (convert-arithmetic value))
             1))  ; Return indent increment for nested structures
          (values
           (format nil "(~A nil)" var-name)
           1)))))  ; Return indent increment for consistency

;; Fix the convert-line function to properly use the new-indent value
(defun convert-line (line current-block indent-level)
  "Convert a single line of C code to Lisp with improved block tracking and indentation."
  (let ((trimmed-line (string-trim-whitespace line)))
    (if (empty-string-p trimmed-line)
        (values "" current-block indent-level)
        (let ((line-type (determine-line-type trimmed-line)))
          (case line-type
            (:func-definition
             (multiple-value-bind (converted new-indent)
                 (convert-func-definition trimmed-line)
               (values
                converted
                (make-block-info
                 :type :func-definition
                 :parent current-block
                 :needs-closing t
                 :let-vars nil
                 :indent-level (+ indent-level new-indent))
                (+ indent-level new-indent))))
            
            (:var-definition
             (multiple-value-bind (converted new-indent)
                 (convert-var-definition trimmed-line)
               (let* ((current-let-vars
                       (if current-block
                           (block-info-let-vars current-block)
                           nil))
                      (new-indent-level (+ indent-level new-indent)))
                 (values
                  converted
                  (make-block-info
                   :type (if current-block 
                            (block-info-type current-block)
                            :let-block)
                   :parent (block-info-parent current-block)
                   :needs-closing (if current-block 
                                    (block-info-needs-closing current-block)
                                    t)
                   :let-vars (cons converted current-let-vars)
                   :indent-level new-indent-level)
                  new-indent-level))))
            
            (:if-statement
             (multiple-value-bind (converted new-indent)
                 (convert-if-statement trimmed-line)
               (values
                converted
                (make-block-info
                 :type :if-statement
                 :parent current-block
                 :needs-closing t
                 :let-vars nil
                 :indent-level (+ indent-level new-indent))
                (+ indent-level new-indent))))
            
            (:loop-for
             (multiple-value-bind (converted new-indent)
                 (convert-for-loop trimmed-line)
               (values
                converted
                (make-block-info
                 :type :loop-for
                 :parent current-block
                 :needs-closing t
                 :let-vars nil
                 :indent-level (+ indent-level new-indent))
                (+ indent-level new-indent))))
            
            (otherwise
            (let ((conversion-foo (case line-type
                                   (:func-prototype #'convert-func-prototype)
                                   (:var-assignment #'convert-func-call)
                                   (:func-call #'convert-func-call)
                                   (:func-return #'convert-func-return))))
               (if conversion-foo
                   (multiple-value-bind (converted new-indent)
                       (funcall conversion-foo trimmed-line)
                     (values 
                      converted 
                      current-block 
                      (+ indent-level (or new-indent 0))))
                   (values "" current-block indent-level)))))))))

;; Main recursion function for processing input file line by line conversion
(defun convert-c-to-lisp-recursive (lines current-block indent-level result &optional (index 0))
  "Recursively convert C code to Lisp with proper formatting and block tracking."
  (if (>= index (length lines))
      ;; Base case - end of lines
      (if current-block
          ;; Handle any remaining open blocks
          (let ((final-result result))
            (when (block-info-let-vars current-block)
              ;; Add let block closing
              (push (format nil "~A)"
                          (make-string (* 2 (block-info-indent-level current-block))
                                     :initial-element #\Space))
                    final-result))
            ;; Add function closing if needed
            (when (and (eq (block-info-type current-block) :func-definition)
                      (block-info-needs-closing current-block))
              (push ")" final-result))
            (reverse final-result))
          (reverse result))
      
      ;; Process current line
      (let* ((line (string-trim-whitespace (nth index lines)))
             (line-type (determine-line-type line)))
        (case line-type
          (:block-end
           ;; Handle block end
           (if current-block
               (let* ((parent-block (block-info-parent current-block))
                      (updated-result
                        (if (block-info-needs-closing current-block)
                            (cons (format nil "~A)"
                                        (make-string (* 2 (1- (block-info-indent-level current-block)))
                                                   :initial-element #\Space))
                                  result)
                            result)))
                 (convert-c-to-lisp-recursive
                  lines
                  parent-block
                  (max 0 (1- indent-level))
                  updated-result
                  (1+ index)))
               ;; No current block, continue processing
               (convert-c-to-lisp-recursive
                lines
                nil
                indent-level
                result
                (1+ index))))
          
          (otherwise
           ;; Process regular line
           (multiple-value-bind (converted-line new-block-info new-indent)
               (convert-line line current-block indent-level)
             (when (and new-block-info (not (block-info-p new-block-info)))
               ;; Convert property list to block-info structure
               (setf new-block-info
                     (make-block-info
                      :type (getf new-block-info :type)
                      :parent current-block
                      :needs-closing (getf new-block-info :needs-closing)
                      :let-vars (getf new-block-info :let-vars)
                      :indent-level new-indent)))
             
             (let ((updated-result
                    (if (not (empty-string-p converted-line))
                        (cons
                         (concatenate 'string
                                    (make-string (* 2 indent-level)
                                               :initial-element #\Space)
                                    converted-line)
                         result)
                        result)))
               (convert-c-to-lisp-recursive
                lines
                new-block-info
                new-indent
                updated-result
                (1+ index)))))))))


;; Read file function
(defun read-file (file-name)
  "Read a file and return its contents as a list of lines."
  (handler-case
      (with-open-file (in file-name :direction :input)
        (loop for line = (read-line in nil nil)
              while line
              collect line))
    (file-error (e)
      (format t "Error reading file: ~A~%" e)
      nil)))

;;  write file function
(defun write-file (file-name converted-lines)
  "Write the converted Lisp code to the output file."
  (handler-case
      (with-open-file (out file-name
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out "~{~A~%~}" converted-lines)
        t)  
    (file-error (e)
      (format t "Error writing file: ~A~%" e)
      nil)))

;; main entry point
(defun convert-c-to-lisp (input-file output-file)
  "Main function to convert C file to Lisp with improved block handling."
  (let* ((input-lines (read-file input-file))
         (converted-lines (when input-lines 
                          (convert-c-to-lisp-recursive input-lines nil 0 nil))))
    (cond
      ((null input-lines)
       (format t "Failed to read input file: ~A~%" input-file))
      ((null converted-lines)
       (format t "No lines were converted from file: ~A~%" input-file))
      ((write-file output-file converted-lines)
       (format t "Successfully converted ~A to ~A~%" input-file output-file))
      (t
       (format t "Failed to write output file: ~A~%" output-file)))))

(convert-c-to-lisp "input1.c" "output.lisp")
