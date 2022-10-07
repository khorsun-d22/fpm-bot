(import (chicken condition)
        (chicken file)
        (chicken format)
        (chicken io)
        (chicken pretty-print)
        (chicken process-context)
        (chicken random)
        (prefix comparse p:)
        http-client
        intarweb        ;; http library
        medea           ;; JSON parser/serializer
        spiffy          ;; web server
        sql-de-lite
        srfi-1          ;; lists
        srfi-133        ;; vectors
        srfi-14         ;; charsets
        srfi-152        ;; strings
        srfi-69         ;; hash tables
        sxml-serializer
        telebot         ;; telegram bot api
        uri-common)

(define-syntax define-parameter
  (syntax-rules ()
    ((define-parameter name initial-value)
     (define name (make-parameter initial-value)))))

(define-parameter bot-token #f)
(define-parameter bot-name #f)
(define-parameter quotes-list #f)
(define-parameter quotes-channel-id #f)

(define pipeline '())
(define pipeline-tail pipeline)

(define (handle-update update)
  (define (next pipeline)
    (if (null? pipeline)
      (begin
        (print "WARNING: unhandled update:")
        (pretty-print update))
      ((car pipeline) update
                      (lambda ()
                        (next (cdr pipeline))))))
  (next pipeline))

(define (add-middleware! middleware)
  (if (null? pipeline)
    (begin
      (set! pipeline (list middleware))
      (set! pipeline-tail pipeline))
    (begin
      (assert (and (list? pipeline-tail)
                   (not (null? pipeline-tail))))
      (set-cdr! pipeline-tail (list middleware))
      (set! pipeline-tail (cdr pipeline-tail)))))

(define (make-command-handler command callback)
  (lambda (update next)
    (let* ((text (resolve-query '(message text) update))
           (tokens (p:parse p:command text))
           (command-name (car tokens))
           (command-bot-name (cadr tokens))
           (command-args (caddr tokens)))
      (if (and (or (eq? #f command-bot-name)
                   (string=? command-bot-name (bot-name)))
               (string=? command-name command))
        (callback update command-args)
        (next)))))

(define-syntax define-command
  (syntax-rules ()
    ((define-command (command update args) body ...)
     (add-middleware!
       (make-command-handler
         command
         (lambda (update args)
           body ...))))))

(define (p:map parser f)
  (p:bind parser
          (lambda (x)
            (p:result (f x)))))

(define (p:separated-by separator parser)
  (p:bind (p:sequence
            parser
            (p:zero-or-more
              (p:preceded-by separator parser)))
          (lambda (x)
            (p:result (cons (car x) (cadr x))))))

(define char-set:alphanumeric
  (->char-set "abcdefghijklmnopqrstuvwxyz0123456789_"))

(define p:digits
  (p:as-string (p:one-or-more (p:in char-set:digit))))

(define p:number
  (p:map p:digits string->number))

(define p:word
  (p:as-string (p:one-or-more (p:in char-set:letter))))

(define p:whitespace
  (p:as-string (p:one-or-more (p:in char-set:blank))))

(define p:command-name
  (p:as-string
    (p:preceded-by
      (p:is #\/)
      (p:one-or-more (p:in char-set:alphanumeric)))))

(define p:username
  (p:as-string
    (p:preceded-by
      (p:is #\@)
      (p:one-or-more (p:in char-set:alphanumeric)))))

(define p:rest
  (p:as-string (p:one-or-more p:item)))

(define p:command
  (p:sequence
    p:command-name
    (p:maybe p:username)
    (p:maybe
      (p:preceded-by
        p:whitespace
        p:rest))))

(define p:rand-command-args
  (p:separated-by p:whitespace p:number))

(define (random-list-ref list)
  (list-ref list (pseudo-random-integer (length list))))

(define (get-json url)
  (with-input-from-request url #f read-json))

(define (cat-image-url)
  (let ((result (get-json "https://api.thecatapi.com/v1/images/search")))
    (alist-ref 'url (vector-ref result 0))))

(define random-number
  (case-lambda
    (()
     (random-number 100))
    ((end)
     (random-number 1 end))
    ((start end)
     (+ start
        (pseudo-random-integer (+ 1 (- end start)))))))

(define (extract-numbers str)
  (if str
    (filter number?
            (map string->number
                 (string-split str " ")))
    '()))

(define (message-chat-id update)
  (resolve-query '(message chat id) update))

(define (message-id update)
  (resolve-query '(message message_id) update))

(define (char-index str char-or-charset)
  (string-index str
                (lambda (c)
                  (if (char? char-or-charset)
                    (char=? c char-or-charset)
                    (memv c char-or-charset)))))

(define-command ("start" update args)
  (send-message (bot-token)
                chat_id: (message-chat-id update)
                text: "Ой всё…"
                reply_to_message_id: (message-id update)))

(define-command ("quote" update args)
  (send-message (bot-token)
                chat_id: (message-chat-id update)
                text: (random-list-ref (quotes-list))))

(define-command ("cat" update args)
  (send-photo (bot-token)
              chat_id: (message-chat-id update)
              photo: (cat-image-url)))

(define-command ("rand" update args)
  (let ((args (p:parse p:rand-command-args args)))
    (send-message (bot-token)
                  chat_id: (message-chat-id update)
                  text: (number->string (apply random-number args))
                  reply_to_message_id: (message-id update))))

(define-command ("go" update args)
  (if (string? args)
    (send-poll (bot-token)
               chat_id: (message-chat-id update)
               question: (string-append "Го " args)
               options: #("Да" "Нет")
               is_anonymous: 'false)
    (send-message (bot-token)
                  chat_id: (message-chat-id update)
                  text: "Го! А куда?"
                  reply_to_message_id: (message-id update))))

(define (send-sxml-response sxml)
  (with-headers `((connection close))
                write-logged-response)
  (serialize-sxml sxml
                  method: 'html
                  output: (response-port (current-response))))

(define (handle-request continue)
  (let ((method (request-method (current-request)))
        (path (uri-path (request-uri (current-request)))))
    (cond ((and (equal? method 'POST)
                (equal? path '(/ "hook")))
           (let ((update (read-json (request-port (current-request))
                                    consume-trailing-whitespace: #f)))
             (send-response status: 'ok)
             (handle-update update)))
          ((equal? method 'GET)
           (send-sxml-response
             `(html
                (body
                  (p "Hello, World!")
                  (p "Chat with me on "
                     (a (@ (href ,(string-append "https://t.me/" (bot-name))))
                        "Telegram")
                     "!")))))
          (else
            (continue)))))

(define (abort-simple message)
  (print message)
  (exit 1))

(set-pseudo-random-seed! (random-bytes))

(bot-token (get-environment-variable "BOT_TOKEN"))
(unless (bot-token)
  (abort-simple "BOT_TOKEN environment variable not set"))

(bot-name (resolve-query '(result username)
                         (get-me (bot-token))))
(print "INFO: my name is " (bot-name))

(let ((quotes-file (get-environment-variable "QUOTES_FILE")))
  (if (and (string? quotes-file)
           (file-readable? quotes-file))
    (quotes-list (with-input-from-file quotes-file read-lines))
    (abort-simple "QUOTES_FILE environment variable is not set or is not a readable file")))

(let ((quotes-channel-id-str (get-environment-variable "QUOTES_CHANNEL_ID")))
  (if (string? quotes-channel-id-str)
    (begin
      (quotes-channel-id (string->number quotes-channel-id-str))
      (unless (number? (quotes-channel-id))
        (abort-simple "QUOTES_CHANNEL_ID environment variable is not a number")))
    (abort-simple "QUOTES_CHANNEL_ID environment variable is not set")))

(let* ((quotes-channel-info (get-chat (bot-token)
                                      chat_id: (quotes-channel-id)))
       (quotes-channel-title (resolve-query '(result title)
                                            quotes-channel-info)))
  (print "INFO: listening for quotes in channel " quotes-channel-title))

(let ((port (get-environment-variable "PORT")))
  (if (string? port)
    (let ((port-number (string->number port)))
      (if (and (integer? port-number)
               (<= 0 port-number 65535))
        (server-port port-number)
        (abort-simple "PORT environment variable is not a valid port number")))
    (print "WARNING: PORT environment variable not set. Defaulting to " (server-port))))

(access-log (current-error-port))
(vhost-map `((".*" . ,handle-request)))

(when (get-environment-variable "PRODUCTION")
  (handle-exception (lambda (exn chain))))

(print "INFO: listening on http://0.0.0.0:" (server-port))
(start-server)
