(require 'ert)

(use-package html-to-org
  :load-path "~/source/repos/html-to-org")

(ert-deftest test-html-to-org-headings ()
  "Test conversion of HTML headings to Org headings."
  (should (string= (html-to-org-convert '(body (h1 "Title") (h2 "Subtitle") (h3 "Section")))
                   "#+TITLE: Untitled Document\n\n* Title\n\n** Subtitle\n\n*** Section\n\n")))

(ert-deftest test-html-to-org-paragraphs ()
  "Test conversion of HTML paragraphs to Org paragraphs."
  (should (string= (html-to-org-convert '(body (p "First paragraph") (p "Second paragraph")))
                   "#+TITLE: Untitled Document\n\nFirst paragraph\n\nSecond paragraph\n\n")))

(ert-deftest test-html-to-org-lists ()
  "Test conversion of HTML lists to Org lists."
  (should (string= (html-to-org-convert '(body (ul (li "Item 1") (li "Item 2")) (ol (li "First") (li "Second"))))
                   "#+TITLE: Untitled Document\n\n- Item 1\n- Item 2\n\n1. First\n2. Second\n\n")))

(ert-deftest test-html-to-org-nested-lists ()
  "Test conversion of nested HTML lists to nested Org lists."
  (should (string= (html-to-org-convert '(body (ul (li "Item 1" (ul (li "Subitem A") (li "Subitem B"))) (li "Item 2"))))
                   "#+TITLE: Untitled Document\n\n- Item 1\n  - Subitem A\n  - Subitem B\n- Item 2\n\n")))

(ert-deftest test-html-to-org-links ()
  "Test conversion of HTML links to Org links."
  (should (string= (html-to-org-convert '(body (p (a ((href . "https://example.com")) "Link text"))))
                   "#+TITLE: Untitled Document\n\n[[https://example.com][Link text]]\n\n")))

(ert-deftest test-html-to-org-images ()
  "Test conversion of HTML images to Org images."
  (should (string= (html-to-org-convert '(body (p (img ((src . "image.jpg") (alt . "Alt text"))))))
                   "#+TITLE: Untitled Document\n\n[[image.jpg][Alt text]]\n\n")))

(ert-deftest test-html-to-org-inline-formatting ()
  "Test conversion of HTML inline formatting to Org inline formatting."
  (should (string= (html-to-org-convert '(body (p "Normal " (strong "bold") " and " (em "italic") " text")))
                   "#+TITLE: Untitled Document\n\nNormal *bold* and /italic/ text\n\n")))

(ert-deftest test-html-to-org-code-blocks ()
  "Test conversion of HTML pre/code blocks to Org source blocks."
  (should (string= (html-to-org-convert '(body (pre ((class . "language-python")) "def hello():\n    print('Hello, World!')")))
                   "#+TITLE: Untitled Document\n\n#+BEGIN_SRC python\ndef hello():\n    print('Hello, World!')\n#+END_SRC\n\n")))

(ert-deftest test-html-to-org-tables ()
  "Test conversion of HTML tables to Org tables."
  (should (string= (html-to-org-convert '(body (table (tr (th "Header 1") (th "Header 2")) (tr (td "Cell 1") (td "Cell 2")))))
                   "#+TITLE: Untitled Document\n\n| Header 1 | Header 2 |\n|----------+----------|\n| Cell 1   | Cell 2   |\n\n")))

(ert-deftest test-html-to-org-blockquotes ()
  "Test conversion of HTML blockquotes to Org quotes."
  (should (string= (html-to-org-convert '(body (blockquote "Quoted text")))
                   "#+TITLE: Untitled Document\n\n#+BEGIN_QUOTE\nQuoted text\n#+END_QUOTE\n\n")))

(defun run-html-to-org-tests ()
  "Run all html-to-org tests."
  (interactive)
  (ert-run-tests-interactively "^test-html-to-org-"))
