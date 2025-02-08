(declaim (ftype (function (string) t) read-input-file))
(declaim (ftype (function (string fixnum list) list) split-words))
(declaim (ftype (function (string fixnum) t) extract-token))
(declaim (ftype (function (string) t) set-final-state))
(declaim (ftype (function (string) t) set-operator-state))
(declaim (ftype (function (string fixnum) fixnum) is-valuef))
(declaim (ftype (function (string) fixnum) is-leading-zero))
(declaim (ftype (function (string fixnum) fixnum) is-identifier))


; Token definitions
(defvar *keywords* '("and" "or" "not" "eq" "gt" "set" "deffun" "defvar" "if" "exit"
                     "true" "false" "progn" "while" "list" "equal" "less" "nil"
                     "append" "concat" "for" "load" "disp"))

(defvar *operators* '("+" "-" "*" "/" "(" ")"))

; State variables (keeping original style)
(defvar *state* "start")
(defvar *error-state* "initial")
(defvar *numpre-state* 0)
(defvar *numpos-state* 0)
(defvar *f-state* 0)
(defvar *prealpha-state* 0)
(defvar *preunderscore-state* 0)
(defvar *comment-state* 0)
(defvar *leading-zero* 0)

; Validate integer number
(defun is-valuei (token cur)
    (cond
        ((>= cur (length token)) 1)  ; End of token, valid integer
        ((not (digit-char-p (char token cur))) 0)  ; Invalid character
        (t (is-valuei token (1+ cur)))))  ; Continue checking

; Reset all states
(defun reset-states ()
    (setq *state* "start")
    (setq *numpre-state* 0)
    (setq *numpos-state* 0)
    (setq *f-state* 0)
    (setq *prealpha-state* 0)
    (setq *preunderscore-state* 0)
    (setq *comment-state* 0)
    (setq *leading-zero* 0))
; File reading function
(defun read-input-file (filename)
    (with-open-file (stream filename :direction :input)
        (let ((content (make-string (file-length stream))))
            (read-sequence content stream)
            content)))



; Main lexer function
(defun gpp-lexer (&optional filename)
    (let ((input (if filename 
                     (read-input-file filename)
                     (progn (format t "~%~%>> ") (read-line)))))
        (setq *state* "start")
        (setq *error-state* "initial")
        (let ((tokens (split-words input 0 '())))
            (when (string/= *error-state* "initial")
                (format t "LEXICAL ERROR: ~S cannot be tokenized" *error-state*)
                (exit))
            tokens)))

; Split input into words
(defun split-words (input cur tokens)
    (when (>= cur (length input))
        (return-from split-words tokens))
    
    ; First check if line starts with ;;
    (let ((line-start t))
        (loop for i from 0 to (1- cur) do
            (when (not (member (char input i) '(#\Space #\Tab #\Return #\Newline)))
                (setf line-start nil)))
        
        ; If it's start of line and we see ;;, get the comment and add as token
        (when (and line-start 
                  (< (1+ cur) (length input))
                  (char= (char input cur) #\;)
                  (char= (char input (1+ cur)) #\;))
            ; Collect the comment text
            (let ((comment-start cur))
                (loop while (and (< cur (length input))
                               (not (member (char input cur) '(#\Return #\Newline))))
                      do (incf cur))
                ; Add the comment as a token
                (let ((comment-text (subseq input comment-start cur)))
                    (setf tokens (append tokens (list (list comment-text "COMMENT")))))
                (return-from split-words (split-words input (1+ cur) tokens)))))
    
    (multiple-value-bind (token new-cur)
        (extract-token input cur)
        
        (let ((cleaned-token (string-trim '(#\Space #\Tab #\Return #\Newline) token)))
            (when (string/= cleaned-token "")
                (set-final-state cleaned-token))
            
            (when (and (string/= cleaned-token "") 
                      (string/= *state* "COMMENT"))
                (if (string= *state* "start")
                    (progn 
                        (setf *error-state* cleaned-token)
                        (return-from split-words tokens))
                    (setf tokens (append tokens 
                                       (list (list cleaned-token *state*))))))
            
            (reset-states)
            (split-words input new-cur tokens))))

; Extract single token - Fixed version
(defun extract-token (input cur)
    (let ((token "")
          (input-length (length input)))
        (loop for char = (char input cur)
      while (< cur input-length)
              do (cond
                    ; Handle comments - with bounds checking
                    ((and (char= char #\;) 
                      (= *comment-state* 0)
                      (< (1+ cur) input-length)
                      (char= (char input (1+ cur)) #\;))
                 (setf *comment-state* 1)
                 (loop while (and (< cur input-length)
                                  (not (member (char input cur) '(#\Return #\Newline))))
                       do (incf cur))
                 (return (values token cur)))


                    ; Handle operators
                    ((and (= *comment-state* 0)
                          (find (string char) *operators* :test #'string=))
                     (if (string= token "")
                         (return (values (string char) (1+ cur)))
                         (return (values token cur))))
                    
                    ; Handle whitespace
                    ((and (= *comment-state* 0)
                          (member char '(#\Space #\Tab #\Return #\Newline)))
                     (when (string/= token "")
                         (return (values token (1+ cur))))
                     (incf cur))
                    
                    ; Accumulate token
                    (t (setf token (concatenate 'string token (string char)))
                       (incf cur)))
              finally (return (values token cur)))))  ; Return accumulated token if loop ends

; Set final state for token
(defun set-final-state (token)
    (cond
        ((= *comment-state* 1) 
         (setq *state* "COMMENT"))
        ((member token *keywords* :test #'string=)
         (setq *state* (concatenate 'string "KW_" (string-upcase token))))
        ((member token *operators* :test #'string=)
         (set-operator-state token))
        ((= (is-valuef token 0) 1) 
         (setq *state* "VALUEF"))
        ((= (is-valuei token 0) 1) 
         (setq *state* "VALUEI"))
        ((= (is-identifier token 0) 1) 
         (setq *state* "IDENTIFIER"))
        (t 
         (setq *error-state* token)
         (format t "LEXICAL ERROR: Invalid token '~A'~%" token))))
; Set operator state
(defun set-operator-state (token)
    (cond
        ((string= token "+") (setq *state* "OP_PLUS"))
        ((string= token "-") (setq *state* "OP_MINUS"))
        ((string= token "*") (setq *state* "OP_MULT"))
        ((string= token "/") (setq *state* "OP_DIV"))
        ((string= token "(") (setq *state* "OP_OP"))
        ((string= token ")") (setq *state* "OP_CP"))))

; Validate fractional number
(defun is-valuef (token cur)
    (cond
        ; Check bounds first
        ((>= cur (length token)) 
         (return-from is-valuef (and *numpre-state* *f-state* *numpos-state*)))
        
        ((and (= cur 0) (= (is-leading-zero token) 1)) 
         (return-from is-valuef 0))
        
        ((digit-char-p (char token cur))
         (if (= *f-state* 0)
             (setq *numpre-state* 1)
             (setq *numpos-state* 1)))
        
        ((and (char= #\f (char token cur))
              (< (1+ cur) (length token))  ; Add bounds check here
              (= (is-leading-zero (subseq token (1+ cur))) 0))
         (cond
             ((= *f-state* 1) (return-from is-valuef 0))
             ((= *numpre-state* 0) (return-from is-valuef 0)))
         (setq *f-state* 1))
        
        ((alpha-char-p (char token cur))
         (return-from is-valuef 0)))
         
    (if (< (1+ cur) (length token))  ; Add bounds check for recursive call
        (is-valuef token (1+ cur))
        (return-from is-valuef (and *numpre-state* *f-state* *numpos-state*)))
)

; Check for leading zero
(defun is-leading-zero (token)
    (if (<= (length token) 1)
        0
        (if (and (char= (char token 0) #\0)
                 (digit-char-p (char token 1)))
            1
            0)))

; Validate identifier
(defun is-identifier (token cur)
    (cond 
        ((>= cur (length token))
         (return-from is-identifier (or *prealpha-state* *preunderscore-state*)))
        ((not (or (alpha-char-p (char token cur))
                 (digit-char-p (char token cur))
                 (char= #\_ (char token cur))))
         (progn
             (format t "LEXICAL ERROR: ~S cannot be tokenized" token)
             (exit)))
        ((char= #\_ (char token cur))
         (setq *preunderscore-state* 1))
        ((alpha-char-p (char token cur))
         (setq *prealpha-state* 1))
        ((digit-char-p (char token cur))
         (when (and (= *prealpha-state* 0)
                   (= *preunderscore-state* 0))
             (return-from is-identifier 0))))
    (is-identifier token (1+ cur)))



; Main execution
; (defun main ()
;     (format t "GPP-Lexer started.~%")
;     (format t "Enter '(exit)' to quit~%")
;     (loop
;         (handler-case
;             (let ((result (gpp-lexer)))
;                 (when (equal result '(("exit" "KW_EXIT")))
;                     (return))
;                 (format t "Tokens: ~S~%" result))
;         (error (condition)
;             (format t "Error: ~A~%" condition))))
;             )

; ; Run the main function when script is loaded
; (main)