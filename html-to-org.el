;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: html, org, conversion
;; URL: https://github.com/yourusername/html-to-org

;;; Commentary:
;; This package provides functions to convert HTML files to Org-mode format
;; using libxml for robust HTML parsing.  It handles headings, lists, links,
;; basic formatting, tables, and more.
;;
;; Major functionality:
;; - Convert HTML files to Org-mode
;; - Convert selected region of HTML to Org-mode
;; - Convert current buffer from HTML to Org-mode
;; - Convert HTML file under point in dired to Org-mode
;;
;; Usage:
;; - M-x html-to-org-file            ;; Convert HTML file to Org file
;; - M-x html-to-org-region          ;; Convert selected HTML region to Org
;; - M-x html-to-org-buffer          ;; Convert current buffer from HTML to Org
;; - M-x html-to-org-dired-file      ;; Convert HTML file under point in dired

;;; Code:

;; Additional helper to properly handle the case where <strong> is followed by text
(defun html-extract-inline-text (dom)
  "Extract all text from DOM, preserving inline formatting."
  (let ((result ""))
    (when (stringp dom)
      (setq result (concat result (string-trim dom))))
    
    (when (listp dom)
      (let ((tag (dom-tag dom))
            (children (dom-children dom)))
        ;; Handle specific formatting tags
        (cond
         ((memq tag '(strong b))
          (setq result (concat result " *" (string-trim (dom-texts dom)) "* ")))
         
         ((memq tag '(em i))
          (setq result (concat result " /" (string-trim (dom-texts dom)) "/ ")))
         
         ((memq tag '(u))
          (setq result (concat result " _" (string-trim (dom-texts dom)) "_ ")))
         
         ((memq tag '(code))
          (setq result (concat result " ~" (string-trim (dom-texts dom)) "~ ")))
         
         ;; For other tags, process children
         (t
          (dolist (child children)
            (setq result (concat result (html-extract-inline-text child))))))))
    
    result));;; html-to-org.el --- Convert HTML files to Org-mode format

;;; Commentary:
;; This package provides functions to convert HTML files to Org-mode format
;; using libxml for robust HTML parsing.  It handles headings, lists, links,
;; basic formatting, tables, and more.

;;; Code:

(require 'dom)  ;; DOM manipulation functions

;;;###autoload
(defun html-to-org-file (html-file org-file)
  "Convert HTML-FILE to ORG-FILE in Org-mode format."
  (interactive
   (list (read-file-name "HTML file: " nil nil t)
         (read-file-name "Output Org file: " nil nil nil
                         (concat (file-name-sans-extension
                                  (file-name-nondirectory
                                   (or (buffer-file-name) "output")))
                                 ".org"))))
  (with-temp-buffer
    (insert-file-contents html-file)
    (condition-case err
        (let ((org-content (html-to-org-convert (libxml-parse-html-region (point-min) (point-max)))))
          (with-temp-file org-file
            (insert org-content))
          (message "Converted %s to %s" html-file org-file))
      (error 
       (message "Error converting %s: %s" html-file (error-message-string err))
       nil))))

;;;###autoload
(defun html-to-org-region (start end)
  "Convert HTML in region to Org-mode."
  (interactive "r")
  (let ((html (buffer-substring-no-properties start end)))
    (condition-case err
        (progn
          (delete-region start end)
          (goto-char start)
          (insert (html-to-org-convert (with-temp-buffer
                                         (insert html)
                                         (libxml-parse-html-region (point-min) (point-max)))))
          (org-mode))
      (error
       (message "Conversion failed: %s" (error-message-string err))))))

;;;###autoload
(defun html-to-org-buffer ()
  "Convert HTML in current buffer to Org-mode."
  (interactive)
  (let ((html (buffer-string)))
    (condition-case err
        (progn
          (erase-buffer)
          (insert (html-to-org-convert (with-temp-buffer
                                         (insert html)
                                         (libxml-parse-html-region (point-min) (point-max)))))
          (org-mode))
      (error
       (message "Conversion failed: %s" (error-message-string err))))))

;;;###autoload
(defun html-to-org-dired-file ()
  "Convert HTML file under point in dired to Org-mode file."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (user-error "This command only works in dired mode"))
  
  (let* ((file (dired-get-filename))
         (file-ext (file-name-extension file))
         (org-file (concat (file-name-sans-extension file) ".org")))
    
    (unless (and file-ext (string-match-p "html?\\'" file-ext))
      (user-error "File does not appear to be an HTML file"))
    
    (when (file-exists-p org-file)
      (unless (yes-or-no-p (format "File %s already exists. Overwrite? " org-file))
        (user-error "Conversion aborted")))
    
    (let ((success (html-to-org-file file org-file)))
      (when success
        (find-file org-file)))))

;;;###autoload
(defun html-to-org-url (url)
  "Convert HTML from URL to Org format."
  (interactive "sURL to convert: ")
  (require 'url)
  (let ((buffer (url-retrieve-synchronously url)))
    (unless buffer
      (error "Failed to retrieve URL"))
    (with-current-buffer buffer
      (condition-case err
          (progn
            (goto-char (point-min))
            (re-search-forward "^$" nil t) ;; Move past headers
            (forward-char)
            (let ((dom (libxml-parse-html-region (point) (point-max)))
                  (output-buffer (generate-new-buffer (format "*Org: %s*" url))))
              (with-current-buffer output-buffer
                (insert (html-to-org-convert dom))
                (org-mode)
                (switch-to-buffer output-buffer))
              (kill-buffer buffer)))
        (error
         (message "Conversion failed: %s" (error-message-string err))
         (kill-buffer buffer))))))

;;;###autoload
(defun html-to-org-convert (dom)
  "Convert HTML DOM to Org-mode text."
  (let ((title (or (dom-text (car (dom-by-tag dom 'title))) "Untitled Document"))
        (body (or (dom-by-tag dom 'body) dom)))
    (concat "#+TITLE: " title "\n\n"
            (html-dom-to-org body 0))))

(defun html-dom-to-org (dom depth)
  "Convert DOM to Org-mode format with heading DEPTH."
  (let ((result ""))
    (cond
     ;; Text node
     ((stringp dom)
      (let ((text (string-trim dom)))
        (when (not (string-empty-p text))
          text)))
     
     ;; DOM node
     ((listp dom)
      (let ((tag (dom-tag dom))
            (attrs (dom-attributes dom))
            (children (dom-children dom)))
        (cond
         ;; Headings (h1-h6)
         ((and (symbolp tag) (string-match "^h\\([1-6]\\)$" (symbol-name tag)))
          (let* ((level (+ depth (string-to-number (match-string 1 (symbol-name tag)))))
                 (heading-text (string-trim (dom-texts dom))))
            (setq result (concat result 
                                 (make-string level ?*) 
                                 " " 
                                 heading-text 
                                 "\n\n"))))
         
         ;; Paragraphs
         ((eq tag 'p)
          (let ((p-text (html-dom-to-org-inline-content dom)))
            (when (not (string-empty-p p-text))
              (setq result (concat result p-text "\n\n")))))
         
         ;; Links
         ((eq tag 'a)
          (let ((href (dom-attr dom 'href))
                (link-text (html-dom-to-org-inline-content dom)))
            (if href
                (concat "[[" href "][" link-text "]]")
              link-text)))
         
         ;; Images
         ((eq tag 'img)
          (let ((src (dom-attr dom 'src))
                (alt (dom-attr dom 'alt)))
            (if src
                (concat "#+attr_org: :width 600px\n"
                        "#+attr_html: :width 100%\n"
                        "[[file:" src "]" (if alt (concat "[" alt "]") "]"))
              "")))
         
         ;; Lists
         ((eq tag 'ul)
          (let ((list-result ""))
            (dolist (child children)
              (when (and (listp child) (eq (dom-tag child) 'li))
                (setq list-result (concat list-result (html-dom-to-org-list-item child 0 "- ")))))
            (setq result (concat result list-result "\n"))))
         
         ((eq tag 'ol)
          (let ((list-result "")
                (counter 1))
            (dolist (child children)
              (when (and (listp child) (eq (dom-tag child) 'li))
                (setq list-result (concat list-result 
                                          (html-dom-to-org-list-item child 0 
                                                                     (format "%d. " counter))))
                (setq counter (1+ counter))))
            (setq result (concat result list-result "\n"))))
         
         ;; Tables
         ((eq tag 'table)
          (setq result (concat result (html-table-to-org dom) "\n\n")))
         
         ;; Pre/Code blocks
         ((eq tag 'pre)
          (let ((code-content (dom-texts dom))
                (language ""))
            ;; Try to determine language from class
            (when (dom-by-tag dom 'code)
              (let ((code-elem (car (dom-by-tag dom 'code))))
                (when (dom-attr code-elem 'class)
                  (let ((class (dom-attr code-elem 'class)))
                    (when (string-match "language-\\([^ ]+\\)" class)
                      (setq language (match-string 1 class)))))))
            (setq result (concat result 
                                 "#+begin_src " language "\n" 
                                 code-content 
                                 "#+end_src\n\n"))))
         
         ;; Block quotes
         ((eq tag 'blockquote)
          (let ((quote-text (html-dom-to-org dom (1+ depth))))
            (setq result (concat result 
                                 "#+begin_quote\n" 
                                 quote-text 
                                 "#+end_quote\n\n"))))
         
         ;; Default: process children
         (t
          (dolist (child children)
            (let ((child-text (html-dom-to-org child depth)))
              (when (and child-text (not (string-empty-p child-text)))
                (setq result (concat result child-text)))))))))
     )
    result))

(defun html-dom-to-org-list-item (dom depth prefix)
  "Convert DOM list item to Org format with DEPTH indentation and PREFIX."
  (let ((result "")
        (item-text "")
        (has-sublist nil)
        (sublist-result ""))
    
    ;; First, extract all direct text and formatted elements before any sublists
    (dolist (child (dom-children dom))
      (cond
       ;; Text node - add directly to item text
       ((stringp child)
        (let ((text (string-trim child)))
          (when (not (string-empty-p text))
            (setq item-text (concat item-text text)))))
       
       ;; Nested list - handle separately later
       ((and (listp child) (memq (dom-tag child) '(ul ol)))
        (setq has-sublist t)
        (let ((sublist-prefix (if (eq (dom-tag child) 'ol) 
                                  (lambda (n) (format "%d. " n)) 
                                "- ")))
          (setq sublist-result 
                (html-dom-to-org-sublist child (1+ depth) sublist-prefix))))
       
       ;; Handle inline elements - especially <strong> tags
       ((listp child)
        (let ((tag (dom-tag child)))
          (cond
           ;; For strong/bold tags, convert and add to item text
           ((memq tag '(strong b))
            (let ((strong-text (dom-texts child)))
              (setq item-text (concat item-text "*" (string-trim strong-text) "*"))))
           
           ;; For other inline elements, use formatter
           ((memq tag '(em i u code span a))
            (setq item-text (concat item-text (html-dom-to-org-inline-content child))))
           
           ;; For other elements, extract text
           (t
            (let ((element-text (dom-texts child)))
              (when (and element-text (not (string-empty-p element-text)))
                (setq item-text (concat item-text (string-trim element-text)))))))))))
    
    ;; Format the list item with the collected text
    (setq item-text (string-trim item-text))
    (setq result (concat result 
                         (make-string (* 2 depth) ? ) 
                         prefix 
                         item-text 
                         "\n"))
    
    ;; Add any sublists
    (when has-sublist
      (setq result (concat result sublist-result)))
    
    result))

(defun html-dom-to-org-sublist (dom depth prefix)
  "Convert DOM sublist to Org format with DEPTH and PREFIX."
  (let ((result ""))
    (let ((counter 1))
      (dolist (child (dom-children dom))
        (when (and (listp child) (eq (dom-tag child) 'li))
          (let ((item-prefix (if (stringp prefix) 
                                 prefix 
                               (funcall prefix counter))))
            (setq result (concat result 
                                 (html-dom-to-org-list-item child depth item-prefix)))
            (setq counter (1+ counter))))))
    result))

(defun html-dom-to-org-inline-content (dom)
  "Extract formatted text content from inline DOM elements."
  (let ((result ""))
    (dolist (child (dom-children dom))
      (cond
       ;; Text node
       ((stringp child)
        (let ((text (string-trim child)))
          (when (not (string-empty-p text))
            (setq result (concat result text)))))
       
       ;; DOM nodes with special formatting
       ((listp child)
        (let ((tag (dom-tag child))
              (content (html-dom-to-org-inline-content child)))
          (cond
           ;; Bold
           ((memq tag '(b strong))
            (setq result (concat result " *" content "* ")))
           
           ;; Italic
           ((memq tag '(i em))
            (setq result (concat result " /" content "/ ")))
           
           ;; Underline
           ((eq tag 'u)
            (setq result (concat result " _" content "_ ")))
           
           ;; Strike-through
           ((memq tag '(s strike del))
            (setq result (concat result "+" content "+")))
           
           ;; Code
           ((memq tag '(code tt kbd))
            (setq result (concat result " ~" content "~ ")))
           
           ;; Verbatim
           ((eq tag 'samp)
            (setq result (concat result " =" content "= ")))
           
           ;; Links
           ((eq tag 'a)
            (let ((href (dom-attr child 'href)))
              (if href
                  (setq result (concat result " [[" href "][" content "]] "))
                (setq result (concat result content)))))
           
           ;; Breaks
           ((eq tag 'br)
            (setq result (concat result "\n")))
           
           ;; Images
           ((eq tag 'img)
            (let ((src (dom-attr child 'src))
                  (alt (dom-attr child 'alt)))
              (if src
                  (setq result (concat
                                "#+attr_org: :width 600px\n"
                                "#+attr_html: :width 100%\n"
                                "[[file:" src "]" (if alt (concat "[" alt "]") "]")))
                (setq result result))))
           
           ;; Span (pass through)
           ((eq tag 'span)
            (setq result (concat result content)))
           
           ;; Default - just append content
           (t
            (setq result (concat result content))))))))
    result))

(defun html-dom-to-org-mixed-content (dom)
  "Process DOM with mixed content (text and elements)."
  (let ((result ""))
    (dolist (child (dom-children dom))
      (cond
       ;; Text node
       ((stringp child)
        (setq result (concat result (string-trim child))))
       
       ;; Element node
       ((listp child)
        (let ((tag (dom-tag child)))
          (cond
           ;; Skip lists as they're processed separately
           ((memq tag '(ul ol))
            nil)
           
           ;; For other elements, recursively process
           (t
            (let ((element-text (html-dom-to-org child 0)))
              (when (and element-text (not (string-empty-p element-text)))
                (setq result (concat result " " element-text))))))))))
    result))

(defun html-table-to-org (dom)
  "Convert HTML table DOM to Org table format."
  (let ((rows (dom-by-tag dom 'tr))
        (has-header nil)
        (org-lines '()))

    ;; Check if the first row is a header
    (when (and rows (dom-by-tag (car rows) 'th))
      (setq has-header t))

    ;; Convert each row to an org table row
    (dolist (row rows)
      (let ((cells (append (dom-by-tag row 'th) (dom-by-tag row 'td)))
            (line "|"))
        (dolist (cell cells)
          (setq line (concat line " " (string-trim (dom-texts cell)) " |")))
        (push line org-lines)))

    (setq org-lines (nreverse org-lines)) ;; restore original order

    ;; Insert header separator if needed
    (when has-header
      (let* ((header (car org-lines))
             (columns (length (split-string header "|")))
             (separator (concat "|"
                                (mapconcat (lambda (_) "---") (make-list (- columns 2) nil) "+")
                                "|")))
        (setq org-lines (append (list (car org-lines) separator) (cdr org-lines)))))

    ;; Join lines into a single string
    (mapconcat 'identity org-lines "\n")))

(defun html-to-org-direct-conversion (html-string)
  "Directly convert HTML-STRING to Org format without using DOM functions."
  (with-temp-buffer
    (insert html-string)
    
    ;; Basic conversion of HTML tags to Org format
    ;; Headers
    (goto-char (point-min))
    (while (re-search-forward "<h1>\\(.*?\\)</h1>" nil t)
      (replace-match "* \\1\n\n"))
    
    (goto-char (point-min))
    (while (re-search-forward "<h2>\\(.*?\\)</h2>" nil t)
      (replace-match "** \\1\n\n"))
    
    (goto-char (point-min))
    (while (re-search-forward "<h3>\\(.*?\\)</h3>" nil t)
      (replace-match "*** \\1\n\n"))
    
    ;; Strong/Bold
    (goto-char (point-min))
    (while (re-search-forward "<strong>\\(.*?\\)</strong>" nil t)
      (replace-match "*\\1*"))
    
    (goto-char (point-min))
    (while (re-search-forward "<b>\\(.*?\\)</b>" nil t)
      (replace-match "*\\1*"))
    
    ;; Italic
    (goto-char (point-min))
    (while (re-search-forward "<em>\\(.*?\\)</em>" nil t)
      (replace-match "/\\1/"))
    
    (goto-char (point-min))
    (while (re-search-forward "<i>\\(.*?\\)</i>" nil t)
      (replace-match "/\\1/"))
    
    ;; Paragraphs
    (goto-char (point-min))
    (while (re-search-forward "<p>\\(.*?\\)</p>" nil t)
      (replace-match "\\1\n\n"))
    
    ;; Lists
    (goto-char (point-min))
    (while (re-search-forward "<ul[^>]*>" nil t)
      (replace-match ""))
    
    (goto-char (point-min))
    (while (re-search-forward "</ul>" nil t)
      (replace-match ""))
    
    (goto-char (point-min))
    (while (re-search-forward "<ol[^>]*>" nil t)
      (replace-match ""))
    
    (goto-char (point-min))
    (while (re-search-forward "</ol>" nil t)
      (replace-match ""))
    
    (goto-char (point-min))
    (while (re-search-forward "<li>\\(.*?\\)</li>" nil t)
      (replace-match "- \\1\n"))
    
    ;; Links
    (goto-char (point-min))
    (while (re-search-forward "<a href=\"\\(.*?\\)\">\\(.*?\\)</a>" nil t)
      (replace-match "[[\\1][\\2]]"))
    
    ;; Images
    (goto-char (point-min))
    (while (re-search-forward "<img src=\"\\(.*?\\)\".*?alt=\"\\(.*?\\)\".*?>" nil t)
      (replace-match "[[\\1][\\2]]"))
    
    ;; Table tags (simple handling)
    (goto-char (point-min))
    (while (re-search-forward "<table[^>]*>" nil t)
      (replace-match ""))
    
    (goto-char (point-min))
    (while (re-search-forward "</table>" nil t)
      (replace-match ""))
    
    (goto-char (point-min))
    (while (re-search-forward "<tr[^>]*>" nil t)
      (replace-match "|"))
    
    (goto-char (point-min))
    (while (re-search-forward "</tr>" nil t)
      (replace-match "|\n"))
    
    (goto-char (point-min))
    (while (re-search-forward "<th[^>]*>\\(.*?\\)</th>" nil t)
      (replace-match " *\\1* |"))
    
    (goto-char (point-min))
    (while (re-search-forward "<td[^>]*>\\(.*?\\)</td>" nil t)
      (replace-match " \\1 |"))
    
    ;; Remove div tags
    (goto-char (point-min))
    (while (re-search-forward "<div[^>]*>" nil t)
      (replace-match ""))
    
    (goto-char (point-min))
    (while (re-search-forward "</div>" nil t)
      (replace-match ""))
    
    ;; Remove span tags
    (goto-char (point-min))
    (while (re-search-forward "<span[^>]*>" nil t)
      (replace-match ""))
    
    (goto-char (point-min))
    (while (re-search-forward "</span>" nil t)
      (replace-match ""))
    
    ;; Line breaks
    (goto-char (point-min))
    (while (re-search-forward "<br\\s-*/>" nil t)
      (replace-match "\n"))
    
    (goto-char (point-min))
    (while (re-search-forward "<br>" nil t)
      (replace-match "\n"))
    
    ;; Remove remaining HTML tags
    (goto-char (point-min))
    (while (re-search-forward "<[^>]*>" nil t)
      (replace-match ""))
    
    ;; Clean up whitespace
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))
    
    ;; Clean up HTML entities
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t)
      (replace-match "&"))
    
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t)
      (replace-match "<"))
    
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t)
      (replace-match ">"))
    
    (goto-char (point-min))
    (while (re-search-forward "&quot;" nil t)
      (replace-match "\""))
    
    (goto-char (point-min))
    (while (re-search-forward "&nbsp;" nil t)
      (replace-match " "))
    
    (buffer-string)))

(provide 'html-to-org)
;;; html-to-org.el ends here
