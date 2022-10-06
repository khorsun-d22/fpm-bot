(import (chicken condition)
        (chicken file)
        (chicken format)
        (chicken io)
        (chicken pretty-print)
        (chicken process-context)
        (chicken random)
        http-client
        intarweb        ;; http library
        medea           ;; JSON parser/serializer
        spiffy          ;; web server
        srfi-1          ;; lists
        srfi-133        ;; vectors
        srfi-152        ;; strings
        srfi-69         ;; hash tables
        sxml-serializer
        telebot         ;; telegram bot api
        uri-common)

(define (aborts? thunk)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler (lambda exn
                                (k #t))
                              (lambda ()
                                (thunk)
                                (k #f))))))

(define bot-token
  (make-parameter (get-environment-variable "BOT_TOKEN")
                  (lambda (token)
                    (if (aborts? (lambda () (get-me token)))
                      (abort (make-property-condition 'invalid-bot-token
                                                      'value token))
                      token))))

(define quotes
  (make-parameter (get-environment-variable "QUOTES_FILE")
                  (lambda (value)
                    (let ((value (if (and (string? value)
                                          (file-readable? value))
                                   (with-input-from-file value read-lines)
                                   value)))
                      (if (and (list? value)
                               (every string? value))
                        value
                        (abort (make-property-condition 'invalid-quotes
                                                        'value value)))))))

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
     (+ 1 (pseudo-random-integer 100)))
    ((end)
     (+ 1 (pseudo-random-integer end)))
    ((start end)
     (+ start
        (pseudo-random-integer (+ 1 (- end start)))))))

(define (extract-numbers str)
  (filter number?
          (map string->number
               (string-split str " "))))

(define (message-chat-id update)
  (resolve-query '(message chat id) update))

(define (message-id update)
  (resolve-query '(message message_id) update))

(define (send-reply text update)
  (send-message (bot-token)
                chat_id: (message-chat-id update)
                reply_to_message_id: (message-id update)
                text: text))

(define (char-index str char-or-charset)
  (string-index str
                (lambda (c)
                  (if (char? char-or-charset)
                    (char=? c char-or-charset)
                    (memv c char-or-charset)))))

(define (handle-text-message update)
  (let* ((text (resolve-query '(message text) update))
         (command (substring text 0 (char-index text (list #\space #\@))))
         (command-end (char-index text #\space))
         (command-args (if command-end
                         (substring text (add1 command-end))
                         #f)))
    (pp (list 'text text command command-args))
    (cond ((equal? command "/start")
           (send-reply "Ой всё…" update))
          ((equal? command "/quote")
           (send-reply (random-list-ref (quotes))
                       update))
          ((equal? command "/cat")
           (send-photo token
                       chat_id: (message-chat-id update)
                       photo: (cat-image-url)
                       reply_to_message_id: message-id))
          ((equal? command "/rand")
           (send-reply
             (number->string
               (apply random-number
                      (extract-numbers command-args)))
             update))
          ((equal? command "/go")
           (send-poll token
                      chat_id: (message-chat-id update)
                      question: (conc "Го " command-args)
                      options: #("Да" "Нет")
                      is_anonymous: 'false))
          (else
            (print "WARNING: unknown message type:")
            (pretty-print update)))))

(define (handle-update update)
  (cond ((string? (resolve-query '(message text) update))
         (handle-text-message update))
        (else
          (print "WARNING: unknown update type:")
          (pretty-print update))))

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
             (handle-update update)
             (send-response status: 'ok)))
          ((equal? method 'GET)
           (send-sxml-response
             `(html
                (body
                  (p "Hello, World!")
                  (p "Chat with me on "
                     (a (@ (href "https://t.me/dnu_fpm_name_provisional_bot"))
                        "Telegram")
                     "!")))))
          (else
            (continue)))))

(set-pseudo-random-seed! (random-bytes))

(access-log (current-error-port))
(vhost-map `((".*" . ,handle-request)))
(server-port 5000)
(start-server)
