(import (chicken format)
        (chicken pretty-print)
        (chicken process-context)
        telebot                    ;; telegram bot api
        spiffy intarweb uri-common ;; web server
        medea                      ;; JSON parser/serializer
        srfi-69                    ;; hash tables
        srfi-133                   ;; vectors
        sxml-serializer            ;; XML serializer
        )

(define (make-conversation token chat_id)
  (let ((send (make-sender token chat_id)))
    (lambda (update)
      (send-chat-action token
                        chat_id: chat_id
                        action: 'typing)
      (let ((text (resolve-query '(message text) update))
            (message-id (resolve-query '(message message_id) update)))
        (cond ((equal? text "/start")
               (send-message token
                             chat_id: chat_id
                             text: "Hi there!"))
              ((not (null? text))
               (send-message token
                             chat_id: chat_id
                             text: (sprintf "You said ~A!~%" text)))
              (else
                (send-message token
                              chat_id: chat_id
                              text: "Unsupported message"
                              reply_to_message_id: message-id)))))))

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
             `(html (@@ *DOCTYPE* "html")
                    (head)
                    (body (p "Hello, World!")
                          (p "Chat with me on "
                             (a (@ (href "https://t.me/dnu_fpm_name_provisional_bot"))
                                "Telegram")
                             "!")))))
          (else
            (continue)))))

(access-log (current-error-port))
(vhost-map `((".*" . ,handle-request)))
(start-server)
