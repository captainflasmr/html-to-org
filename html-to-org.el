;; Test function to demonstrate the conversion with the specific example
(defun html-to-org-test-example ()
  "Test the converter with the example HTML."
  (interactive)
  (let ((html "<h1>Component Specifications</h1><div class=\"outline-3\"><h2>Image Generator Controller (IGC)</h2><div class=\"outline-text-3\"><p>The IGC serves as the central simulation computer and data marshaller for the visual system:</p><ul class=\"org-ul\"><li><strong>Functionality</strong>:<ul class=\"org-ul\"><li>Manages simulation state</li><li>Coordinates multiple image generation channels</li><li>Processes and transmits CIGI protocol messages</li><li>Orchestrates entity and environment data</li><li>Synchronizes simulation time</li><li>Manages communication to visuals hardware</li></ul></li><li><strong>Interfaces</strong>:<ul class=\"org-ul\"><li>CIGI 4.0 output to Image Generator</li><li>Network interface to MIMESIS core simulation</li><li>Direct communication with hardware visual components</li><li>Management interface for configuration</li></ul></li><li><strong>Performance Requirements</strong>:<ul class=\"org-ul\"><li>Support for minimum of two IG channels</li><li>Low-latency processing (&lt;20ms)</li><li>High-bandwidth network capability (10Gbps)</li></ul></li></ul></div></div><div class=\"outline-3\">"))
    (with-current-buffer (get-buffer-create "*HTML to Org Test*")
      (erase-buffer)
      (insert (html-to-org-convert (with-temp-buffer
                                     (insert html)
                                     (libxml-parse-html-region (point-min) (point-max)))))
      (org-mode)
      (switch-to-buffer (current-buffer)))))

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
          (setq result (concat result "*" (string-trim (dom-texts dom)) "*")))
         
         ((memq tag '(em i))
          (setq result (concat result "/" (string-trim (dom-texts dom)) "/")))
         
         ((memq tag '(u))
          (setq result (concat result "_" (string-trim (dom-texts dom)) "_")))
         
         ((memq tag '(code))
          (setq result (concat result "~" (string-trim (dom-texts dom)) "~")))
         
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
    (let ((org-content (html-to-org-convert (libxml-parse-html-region (point-min) (point-max)))))
      (with-temp-file org-file
        (insert org-content))
      (message "Converted %s to %s" html-file org-file))))

;;;###autoload
(defun html-to-org-buffer ()
  "Convert HTML in current buffer to Org-mode."
  (interactive)
  (let ((html (buffer-string)))
    (erase-buffer)
    (insert (html-to-org-convert (with-temp-buffer
                                   (insert html)
                                   (libxml-parse-html-region (point-min) (point-max)))))
    (org-mode)))

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
                (concat "[[" src "][" (or alt src) "]]")
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
                                "#+BEGIN_SRC " language "\n" 
                                code-content 
                                "#+END_SRC\n\n"))))
         
         ;; Block quotes
         ((eq tag 'blockquote)
          (let ((quote-text (html-dom-to-org dom (1+ depth))))
            (setq result (concat result 
                                "#+BEGIN_QUOTE\n" 
                                quote-text 
                                "#+END_QUOTE\n\n"))))
         
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
            (setq result (concat result "*" content "*")))
           
           ;; Italic
           ((memq tag '(i em))
            (setq result (concat result "/" content "/")))
           
           ;; Underline
           ((eq tag 'u)
            (setq result (concat result "_" content "_")))
           
           ;; Strike-through
           ((memq tag '(s strike del))
            (setq result (concat result "+" content "+")))
           
           ;; Code
           ((memq tag '(code tt kbd))
            (setq result (concat result "~" content "~")))
           
           ;; Verbatim
           ((eq tag 'samp)
            (setq result (concat result "=" content "=")))
           
           ;; Links
           ((eq tag 'a)
            (let ((href (dom-attr child 'href)))
              (if href
                  (setq result (concat result "[[" href "][" content "]]"))
                (setq result (concat result content)))))
           
           ;; Breaks
           ((eq tag 'br)
            (setq result (concat result "\n")))
           
           ;; Images
           ((eq tag 'img)
            (let ((src (dom-attr child 'src))
                  (alt (dom-attr child 'alt)))
              (if src
                  (setq result (concat result "[[" src "][" (or alt src) "]]"))
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
  (let ((result "|")
        (has-header nil)
        (rows (dom-by-tag dom 'tr)))
    
    ;; Check if we have a header row (th elements)
    (when (and rows (dom-by-tag (car rows) 'th))
      (setq has-header t))
    
    ;; Process rows
    (dolist (row rows)
      (let ((cells (append (dom-by-tag row 'th) (dom-by-tag row 'td))))
        (dolist (cell cells)
          (setq result (concat result " " (string-trim (dom-texts cell)) " |")))
        (setq result (concat result "\n|"))))
    
    ;; Add separator after header if needed
    (when has-header
      (let ((separator (concat "\n|-" (make-string (- (length (car (split-string result "\n"))) 3) ?-) "-|\n|")))
        (setq result (replace-regexp-in-string "\n|" separator result 1))))
    
    ;; Clean up trailing "|"
    (string-trim-right result "|\n")))

(provide 'html-to-org)
;;; html-to-org.el ends here
