(load "gpp_lexer.lisp")


; test execution
(defun main ()
    (format t "GPP-Lexer started.~%")
    (format t "Enter '(exit)' to quit~%")
    (loop
        (handler-case
            (let ((result (gpp-lexer)))
                (when (equal result '(("exit" "KW_EXIT")))
                    (return))
                (format t "Tokens: ~S~%" result))
        (error (condition)
            (format t "Error: ~A~%" condition))))
            )

; Run the main function when script is loaded
(main)