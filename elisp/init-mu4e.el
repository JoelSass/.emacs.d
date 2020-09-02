(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e mu4e-compose-new
  :init
  (provide 'html2text)
  (setq mu4e-maildir "~/.mail"
          mu4e-user-mail-address-list nil)
  (setq mu4e-attachment-dir
        (lambda (&rest _)
          (expand-file-name ".attachments" (mu4e-root-maildir))))
  :config
     (setq mu4e-get-mail-command "mbsync -a"
           mu4e-change-filenames-when-moving t)
  (setq mu4e-update-interval nil
        mu4e-compose-format-flowed t ; visual-line-mode + auto-fill upon sending
        mu4e-view-show-addresses t
	user-mail-address "joelpaulsass@gmail.com"
	user-full-name  "Joel Sass"
        mu4e-sent-messages-behavior 'sent
        mu4e-hide-index-messages t
        ;; try to show images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; configuration for sending mail
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        message-kill-buffer-on-exit t ; close after sending
        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask-if-none
        ;; use helm/ivy
        mu4e-completing-read-function #'ivy-completing-read
        ;; no need to ask
        mu4e-confirm-quit nil
        ;; remove 'lists' column
        mu4e-headers-fields
        '((:account . 12)
          (:human-date . 12)
          (:flags . 4)
          (:from . 25)
          (:subject)))

  ;; set mail user agent
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Use fancy icons
  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark '("D" . "")
        mu4e-headers-flagged-mark '("F" . "")
        mu4e-headers-new-mark '("N" . "")
        mu4e-headers-passed-mark '("P" . "")
        mu4e-headers-replied-mark '("R" . "")
        mu4e-headers-seen-mark '("S" . "")
        mu4e-headers-trashed-mark '("T" . "")
        mu4e-headers-attach-mark '("a" . "")
        mu4e-headers-encrypted-mark '("x" . "")
        mu4e-headers-signed-mark '("s" . "")
        mu4e-headers-unread-mark '("u" . ""))

  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1))))))))


(defvar org-msg-currently-exporting nil
  "Helper variable to indicate whether org-msg is currently exporting the org buffer to HTML.
Usefull for affecting some of my HTML export config.")

(use-package org-msg
  :after mu4e
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt "\nHi %s,\n\n"
        org-msg-greeting-name-limit 3
        org-msg-text-plain-alternative t)
  (setq org-msg-enforce-css
        (let* ((font-family '(font-family . "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell,\
          \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";"))
               (monospace-font '(font-family . "SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;"))
               (font-size '(font-size . "11pt"))
               (font `(,font-family ,font-size))
               (line-height '(line-height . "1.2"))
               (theme-color "#2654BF")
               (bold '(font-weight . "bold"))
               (color `(color . ,theme-color))
               (table `((margin-top . "6px") (margin-bottom . "6px")
                        (border-left . "none") (border-right . "none")
                        (border-top . "2px solid #222222") (border-bottom . "2px solid #222222")
                        ))
               (ftl-number `(,color ,bold (text-align . "left")))
               (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
                                   fundamental ini json makefile man org plantuml
                                   python sh xml))
               (inline-src `((background-color . "rgba(27,31,35,.05)")
                             (border-radius . "3px")
                             (padding . ".2em .4em")
                             (font-size . "90%") ,monospace-font
                             (margin . 0)))
               (code-src
                (mapcar (lambda (mode)
                          `(code ,(intern (concat "src src-" (symbol-name mode)))
                                 ,inline-src))
                        inline-modes)))
          `((del nil ((color . "grey") (border-left . "none")
                      (text-decoration . "line-through") (margin-bottom . "0px")
                      (margin-top . "10px") (line-height . "11pt")))
            (a nil (,color))
            (a reply-header ((color . "black") (text-decoration . "none")))
            (div reply-header ((padding . "3.0pt 0in 0in 0in")
                               (border-top . "solid #e1e1e1 1.0pt")
                               (margin-bottom . "20px")))
            (span underline ((text-decoration . "underline")))
            (li nil (,line-height (margin-bottom . "0px")
                                  (margin-top . "2px")))
            (nil org-ul ((list-style-type . "square")))
            (nil org-ol (,@font ,line-height (margin-bottom . "0px")
                                (margin-top . "0px") (margin-left . "30px")
                                (padding-top . "0px") (padding-left . "5px")))
            (nil signature (,@font (margin-bottom . "20px")))
            (blockquote nil ((padding . "0px 10px") (margin-left . "10px")
                             (margin-top . "20px") (margin-bottom . "0")
                             (border-left . "3px solid #ccc") (font-style . "italic")
                             (background . "#f9f9f9")))
            (code nil (,font-size ,monospace-font (background . "#f9f9f9")))
            ,@code-src
            (nil linenr ((padding-right . "1em")
                         (color . "black")
                         (background-color . "#aaaaaa")))
            (pre nil ((line-height . "1.2")
                      (color . ,(doom-color 'fg))
                      (background-color . ,(doom-color 'bg))
                      (margin . "4px 0px 8px 0px")
                      (padding . "8px 12px")
                      (width . "95%")
                      (border-radius . "5px")
                      (font-weight . "500")
                      ,monospace-font))
            (div org-src-container ((margin-top . "10px")))
            (nil figure-number ,ftl-number)
            (nil table-number)
            (caption nil ((text-align . "left")
                          (background . ,theme-color)
                          (color . "white")
                          ,bold))
            (nil t-above ((caption-side . "top")))
            (nil t-bottom ((caption-side . "bottom")))
            (nil listing-number ,ftl-number)
            (nil figure ,ftl-number)
            (nil org-src-name ,ftl-number)
            (img nil ((vertical-align . "middle")
                      (max-width . "100%")))
            (img latex-fragment-inline ((transform . ,(format "translateY(-1px) scale(%.3f)"
                                                       (/ 1.0 (if (boundp 'preview-scale)
                                                                  preview-scale 1.4))))
                                 (margin . "0 -0.35em")))
            (table nil (,@table ,line-height (border-collapse . "collapse")))
            (th nil ((border . "none") (border-bottom . "1px solid #222222")
                     (background-color . "#EDEDED") (font-weight . "500")
                     (padding . "3px 10px")))
            (td nil (,@table (padding . "1px 10px")
                             (background-color . "#f9f9f9") (border . "none")))
            (td org-left ((text-align . "left")))
            (td org-right ((text-align . "right")))
            (td org-center ((text-align . "center")))
            (kbd nil ((border . "1px solid #d1d5da") (border-radius . "3px")
                      (box-shadow . "inset 0 -1px 0 #d1d5da") (background-color . "#fafbfc")
                      (color . "#444d56") (padding . "3px 5px") (display . "inline-block")))
            (div outline-text-4 ((margin-left . "15px")))
            (div outline-4 ((margin-left . "10px")))
            (h4 nil ((margin-bottom . "0px") (font-size . "11pt")))
            (h3 nil ((margin-bottom . "0px")
                     ,color (font-size . "14pt")))
            (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
                     ,color (font-size . "18pt")))
            (h1 nil ((margin-top . "20px")
                     (margin-bottom . "0px") ,color (font-size . "24pt")))
            (p nil ((text-decoration . "none") (margin-bottom . "0px")
                    (margin-top . "10px") (line-height . "11pt") ,font-size
                    (max-width . "100ch")))
            (b nil ((font-weight . "500") (color . ,theme-color)))
            (div nil (,@font (line-height . "12pt"))))))
  
  (org-msg-mode t))

(setq org-msg-enforce-css
      (let* ((font-family '(font-family . "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell,\
        \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";"))
             (monospace-font '(font-family . "SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;"))
             (font-size '(font-size . "11pt"))
             (font `(,font-family ,font-size))
             (line-height '(line-height . "1.2"))
             (theme-color "#2654BF")
             (bold '(font-weight . "bold"))
             (color `(color . ,theme-color))
             (table `((margin-top . "6px") (margin-bottom . "6px")
                      (border-left . "none") (border-right . "none")
                      (border-top . "2px solid #222222") (border-bottom . "2px solid #222222")
                      ))
             (ftl-number `(,color ,bold (text-align . "left")))
             (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
                                 fundamental ini json makefile man org plantuml
                                 python sh xml))
             (inline-src `((background-color . "rgba(27,31,35,.05)")
                           (border-radius . "3px")
                           (padding . ".2em .4em")
                           (font-size . "90%") ,monospace-font
                           (margin . 0)))
             (code-src
              (mapcar (lambda (mode)
                        `(code ,(intern (concat "src src-" (symbol-name mode)))
                               ,inline-src))
                      inline-modes)))
        `((del nil ((color . "grey") (border-left . "none")
                    (text-decoration . "line-through") (margin-bottom . "0px")
                    (margin-top . "10px") (line-height . "11pt")))
          (a nil (,color))
          (a reply-header ((color . "black") (text-decoration . "none")))
          (div reply-header ((padding . "3.0pt 0in 0in 0in")
                             (border-top . "solid #e1e1e1 1.0pt")
                             (margin-bottom . "20px")))
          (span underline ((text-decoration . "underline")))
          (li nil (,line-height (margin-bottom . "0px")
                                (margin-top . "2px")))
          (nil org-ul ((list-style-type . "square")))
          (nil org-ol (,@font ,line-height (margin-bottom . "0px")
                              (margin-top . "0px") (margin-left . "30px")
                              (padding-top . "0px") (padding-left . "5px")))
          (nil signature (,@font (margin-bottom . "20px")))
          (blockquote nil ((padding . "0px 10px") (margin-left . "10px")
                           (margin-top . "20px") (margin-bottom . "0")
                           (border-left . "3px solid #ccc") (font-style . "italic")
                           (background . "#f9f9f9")))
          (code nil (,font-size ,monospace-font (background . "#f9f9f9")))
          ,@code-src
          (nil linenr ((padding-right . "1em")
                       (color . "black")
                       (background-color . "#aaaaaa")))
          (pre nil ((line-height . "1.2")
                    (color . ,(doom-color 'fg))
                    (background-color . ,(doom-color 'bg))
                    (margin . "4px 0px 8px 0px")
                    (padding . "8px 12px")
                    (width . "95%")
                    (border-radius . "5px")
                    (font-weight . "500")
                    ,monospace-font))
          (div org-src-container ((margin-top . "10px")))
          (nil figure-number ,ftl-number)
          (nil table-number)
          (caption nil ((text-align . "left")
                        (background . ,theme-color)
                        (color . "white")
                        ,bold))
          (nil t-above ((caption-side . "top")))
          (nil t-bottom ((caption-side . "bottom")))
          (nil listing-number ,ftl-number)
          (nil figure ,ftl-number)
          (nil org-src-name ,ftl-number)
          (img nil ((vertical-align . "middle")
                    (max-width . "100%")))
          (img latex-fragment-inline ((transform . ,(format "translateY(-1px) scale(%.3f)"
                                                     (/ 1.0 (if (boundp 'preview-scale)
                                                                preview-scale 1.4))))
                               (margin . "0 -0.35em")))
          (table nil (,@table ,line-height (border-collapse . "collapse")))
          (th nil ((border . "none") (border-bottom . "1px solid #222222")
                   (background-color . "#EDEDED") (font-weight . "500")
                   (padding . "3px 10px")))
          (td nil (,@table (padding . "1px 10px")
                           (background-color . "#f9f9f9") (border . "none")))
          (td org-left ((text-align . "left")))
          (td org-right ((text-align . "right")))
          (td org-center ((text-align . "center")))
          (kbd nil ((border . "1px solid #d1d5da") (border-radius . "3px")
                    (box-shadow . "inset 0 -1px 0 #d1d5da") (background-color . "#fafbfc")
                    (color . "#444d56") (padding . "3px 5px") (display . "inline-block")))
          (div outline-text-4 ((margin-left . "15px")))
          (div outline-4 ((margin-left . "10px")))
          (h4 nil ((margin-bottom . "0px") (font-size . "11pt")))
          (h3 nil ((margin-bottom . "0px")
                   ,color (font-size . "14pt")))
          (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
                   ,color (font-size . "18pt")))
          (h1 nil ((margin-top . "20px")
                   (margin-bottom . "0px") ,color (font-size . "24pt")))
          (p nil ((text-decoration . "none") (margin-bottom . "0px")
                  (margin-top . "10px") (line-height . "11pt") ,font-size
                  (max-width . "100ch")))
          (b nil ((font-weight . "500") (color . ,theme-color)))
          (div nil (,@font (line-height . "12pt"))))))



(provide 'init-mu4e)
