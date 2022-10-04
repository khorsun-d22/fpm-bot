(import (chicken format)
        (chicken io)
        (chicken pretty-print)
        (chicken process-context)
        (chicken random)
        (chicken string)
        telebot                                ;; telegram bot api
        spiffy http-client intarweb uri-common ;; web server
        medea                                  ;; JSON parser/serializer
        srfi-1                                 ;; lists
        srfi-69                                ;; hash tables
        srfi-133                               ;; vectors
        sxml-serializer                        ;; XML serializer
        )

(define chuck-norris-quotes
  (call-with-input-file "quotes.txt" read-lines))

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

(define (make-conversation token chat_id)
  (define (send text . args)
    (apply send-message token
           chat_id: chat_id
           text: text
           args))
  (define (reply text to . args)
    (apply send text
           reply_to_message_id: to
           args))
  (lambda (update)
    (send-chat-action token
                      chat_id: chat_id
                      action: 'typing)
    (let* ((message-id (resolve-query '(message message_id) update))
           (text (resolve-query '(message text) update))
           (words (if (string? text)
                    (car (string-split text))
                    #f))
           (command (car words))
           (command-args (cdr words)))
      (cond ((equal? command "/start")
             (reply "Ой всё…"
                    message-id))
            ((equal? command "/chuck")
             (reply (random-list-ref chuck-norris-quotes)
                    message-id))
            ((equal? command "/cat")
             (send-photo token
                         chat_id: chat_id
                         photo: (cat-image-url)))
            ((equal? command "/rand")
             (reply (sprintf "~s"
                             (apply random-number
                                    (filter (lambda (x) (not (equal? #f x)))
                                            (map string->number
                                                 command-args))))
                    message-id))
            ((equal? command "/go")
             (let ((title (string-intersperse command-args)))
               (send-poll token
                          chat_id: chat_id
                          question: title
                          options: #("Да" "Нет")
                          is_anonymous: #f)))
            (else
              (print "WARNING: unsupported message:")
              (pretty-print update))))))

(define update-handler
  (let ((token (get-environment-variable "BOT_TOKEN")))
    (make-conversation-manager token make-conversation)))

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
             (update-handler update)
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
(start-server)
