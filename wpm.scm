(use files posix srfi-13 srfi-18 utils)
(use ansi-escape-sequences slice-utf8 utf8)

(define ruler-pos 20)

(define (red text)
  (set-text '(fg-red) text))

(define (tokenize data)
  (string-split data))

(define (format-word word)
  (let* ((len (string-length word))
         (middle (quotient len 2))
         (colorized-word
          (case len
            ((0) "")
            ((1) (red word))
            ((2) (string-append (red (string (string-ref word 0)))
                                (string (string-ref word 1))))
            (else
             (string-append
              (slice word 0 middle)
              (red (string (string-ref word middle)))
              (slice word (fx+ middle 1)))))))
    (string-append
     (make-string
      (fx- ruler-pos
           (if (fx<= len 2)
               0
               middle))
      #\space)
     colorized-word)))

(define (display-word word)
  (let ((ruler (string-append (make-string 20 #\space) "|")))
    (print
     (cursor-position 0 0)
     (erase-line)
     ruler "\n"
     (erase-line)
     (format-word word) "\n"
     ruler)
  (flush-output (current-output-port))))

(define (play words speed)
  (print (erase-display)
         (hide-cursor))
  (for-each (lambda (word)
              (let ((len (string-length word)))
                (display-word word)
                (thread-sleep! speed)
                (when (> len 15)
                  (thread-sleep! speed))
                (when (string-suffix? "." word)
                  (thread-sleep! speed))
                (when (string-suffix? "," word)
                  (thread-sleep! (/ speed 2)))))
            words))

(on-exit
 (lambda ()
   (print (show-cursor))))

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (fprintf port
             "Usage: ~a <file> [<wpm>]

<wpm>:  an integer representing the number of words per minute."
             (pathname-strip-directory (program-name))))
  (when exit-code
    (exit exit-code)))

(let ((args (command-line-arguments)))

  (when (null? args)
    (usage 1))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (let ((file (car args))
        (wpm (if (null? (cdr args))
                 250
                 (or (string->number (cadr args))
                     (usage 1)))))
    (when (< wpm 0)
      (fprintf (current-error-port)
               "<wpm> must be an integer greater than 0.\n")
      (exit 1))
    (play (tokenize (read-all file))
          (/ 60 wpm))))
