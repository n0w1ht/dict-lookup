;;; dict-lookup.el --- Offline dictionary lookup with sdcv and consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Hormoz Zamanpour Siahkal

;; Author: Hormoz Zamanpour Siahkal <n0w1ht@users.noreply.github.com>
;; URL: https://github.com/n0w1ht/dict-lookup
;; Version: 0.4.3
;; Package-Requires: ((emacs "28.1") (quick-sdcv "1.0"))
;; Keywords: convenience, wp, i18n

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Looks words up in StarDict dictionaries kept in `dict-lookup-directory',
;; using sdcv through quick-sdcv.  `dict-lookup' offers every headword of
;; every dictionary for completion (through consult when it is installed),
;; so multi-word entries such as "heart attack" are easy to find.
;; `dict-lookup-at-point' looks up the region or the word at point at once.
;; `dict-lookup-dictionary-order' puts chosen dictionaries first, and
;; `dict-lookup-excluded-dictionaries' leaves some out;
;; `dict-lookup-list-dictionaries' shows the names they take.
;;
;; In the result buffer, `RET' (`dict-lookup-open-entry') looks up the
;; close match at point on its own, and `s' (`dict-lookup-save') saves
;; the entry at point as a flashcard in `dict-lookup-vocabulary-file', an
;; Org file that org-drill can drill as it is.  `dict-lookup-export-anki' sends the saved
;; words that Anki doesn't have yet to Anki.

;;; Code:

(require 'quick-sdcv)
(require 'thingatpt)
(require 'json)
(require 'org)
(require 'url)

(defgroup dict-lookup nil
  "Offline dictionary lookup."
  :group 'applications)

(defcustom dict-lookup-directory (locate-user-emacs-file "dict/")
  "Directory holding StarDict dictionaries, one sub-directory each."
  :type 'directory)

(defcustom dict-lookup-dictionary-order nil
  "Dictionary names to show first, in this order.
Dictionaries not named here follow in sdcv's order.  Names are the
bookname of each dictionary; `dict-lookup-list-dictionaries' shows them,
for example \"WordNet 3.0\" or \"Webster's 1913 (GCIDE)\"."
  :type '(repeat string))

(defcustom dict-lookup-excluded-dictionaries nil
  "Dictionary names to leave out of results and of completion.
Names are as in `dict-lookup-dictionary-order'."
  :type '(repeat string))

(defcustom dict-lookup-vocabulary-file
  (expand-file-name "vocabulary.org"
                    (if (boundp 'org-directory) org-directory "~/org"))
  "Org file where `dict-lookup-save' saves words as flashcards."
  :type 'file)

(defcustom dict-lookup-card-tags '("drill")
  "Tags of each saved word.  org-drill drills entries tagged \"drill\"."
  :type '(repeat string))

(defcustom dict-lookup-save-context t
  "When non-nil, save the sentence the word was looked up from.
It shows on the front of the card, so you recall the word in context."
  :type 'boolean)

(defcustom dict-lookup-save-to-anki nil
  "When non-nil, `dict-lookup-save' also sends the word to Anki."
  :type 'boolean)

(defcustom dict-lookup-anki-connect-url "http://127.0.0.1:8765"
  "Address of Anki's AnkiConnect add-on."
  :type 'string)

(defcustom dict-lookup-anki-deck "Vocabulary"
  "Anki deck receiving saved words."
  :type 'string)

(defcustom dict-lookup-anki-model '("Basic" "Front" "Back")
  "Anki note type for saved words: (NAME FRONT-FIELD BACK-FIELD)."
  :type '(list string string string))

(defcustom dict-lookup-anki-tags '("dict-lookup")
  "Tags given to the Anki notes of saved words."
  :type '(repeat string))

(defcustom dict-lookup-anki-export-file "~/dict-lookup-anki-import.txt"
  "Import file written when Anki can't be reached through AnkiConnect.
Import it in Anki with File > Import, then delete it."
  :type 'file)

(defvar-local dict-lookup--query nil
  "The word looked up to fill this result buffer.")

(defvar-local dict-lookup--context nil
  "The sentence the word of this result buffer was looked up from.")

(defvar dict-lookup--cache nil
  "Cons of (STAMP . HEADWORDS) for `dict-lookup-directory'.")

(defvar dict-lookup-history nil
  "Minibuffer history for `dict-lookup'.")

(defun dict-lookup--idx-files ()
  "Return the .idx files under `dict-lookup-directory'."
  (when (file-directory-p dict-lookup-directory)
    (directory-files-recursively dict-lookup-directory "\\.idx\\'")))

(defun dict-lookup--bookname (idx-file)
  "Return the bookname from the .ifo file next to IDX-FILE, or nil."
  (let ((ifo (concat (file-name-sans-extension idx-file) ".ifo")))
    (when (file-readable-p ifo)
      (with-temp-buffer
        (insert-file-contents ifo)
        (when (re-search-forward "^bookname=\\(.*?\\)\r?$" nil t)
          (match-string 1))))))

(defun dict-lookup--included-idx-files ()
  "Return the .idx files of dictionaries that are not excluded."
  (seq-remove (lambda (f)
                (member (dict-lookup--bookname f)
                        dict-lookup-excluded-dictionaries))
              (dict-lookup--idx-files)))

;;;###autoload
(defun dict-lookup-list-dictionaries ()
  "Show the names of the installed dictionaries, marking excluded ones."
  (interactive)
  (let ((names (delq nil (mapcar #'dict-lookup--bookname
                                 (dict-lookup--idx-files)))))
    (unless names
      (user-error "No dictionaries in %s" dict-lookup-directory))
    (message "Dictionaries: %s"
             (mapconcat (lambda (n)
                          (if (member n dict-lookup-excluded-dictionaries)
                              (format "%S (excluded)" n)
                            (format "%S" n)))
                        names ", "))))

(defun dict-lookup--read-idx (file table)
  "Add the headwords of StarDict index FILE to hash TABLE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (let ((start (point)))
      (while (search-forward "\0" nil t)
        (let ((word (decode-coding-string
                     (buffer-substring-no-properties start (1- (point)))
                     'utf-8)))
          (unless (gethash (downcase word) table)
            (puthash (downcase word) word table)))
        ;; Skip the 32-bit offset and size that follow each headword.
        (forward-char 8)
        (setq start (point))))))

(defun dict-lookup-headwords ()
  "Return the sorted headwords of all dictionaries, cached until they change."
  (let* ((files (dict-lookup--included-idx-files))
         (stamp (mapcar (lambda (f)
                          (cons f (file-attribute-modification-time
                                   (file-attributes f))))
                        files)))
    (unless (equal (car dict-lookup--cache) stamp)
      (let ((table (make-hash-table :test #'equal :size 400000))
            words)
        (dolist (f files)
          (dict-lookup--read-idx f table))
        (maphash (lambda (_ w) (push w words)) table)
        (setq dict-lookup--cache
              (cons stamp (sort words #'string-collate-lessp)))))
    (cdr dict-lookup--cache)))

(declare-function pdf-view-active-region-p "pdf-view")
(declare-function pdf-view-active-region-text "pdf-view")

(defun dict-lookup--word-at-point ()
  "Return the active region or the word at point, or nil.
In a pdf-tools buffer, return the text of the selected region."
  (cond
   ((and (derived-mode-p 'pdf-view-mode) (pdf-view-active-region-p))
    (string-trim (mapconcat #'identity (pdf-view-active-region-text) " ")))
   ((derived-mode-p 'pdf-view-mode) nil)
   ((use-region-p)
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end))))
   (t (thing-at-point 'word t))))

(defun dict-lookup--read-word ()
  "Read a headword, offering every dictionary entry for completion."
  (let ((default (dict-lookup--word-at-point))
        (words (dict-lookup-headwords))
        (prompt "Look up: "))
    (if (fboundp 'consult--read)
        (consult--read words
                       :prompt prompt
                       :default default
                       :initial default
                       :history 'dict-lookup-history
                       :require-match nil
                       :sort nil
                       :category 'dict-word)
      (completing-read prompt words nil nil default
                       'dict-lookup-history default))))

;; Older versions advised `quick-sdcv--call-process' with these.
(advice-remove 'quick-sdcv--call-process 'dict-lookup--call-utf-8)
(advice-remove 'quick-sdcv--call-process 'dict-lookup--filter-results)

(defvar dict-lookup--active nil
  "Non-nil while `dict-lookup' runs, so only its lookups are changed.")

(defun dict-lookup--arrange (results)
  "Drop excluded dictionaries from sdcv RESULTS and put ordered ones first."
  (let* ((kept (seq-uniq       ; sdcv repeats some close matches
                (seq-remove (lambda (r)
                              (member (alist-get 'dict r)
                                      dict-lookup-excluded-dictionaries))
                            results)))
         (rank (lambda (r)
                 (or (seq-position dict-lookup-dictionary-order
                                   (alist-get 'dict r))
                     (length dict-lookup-dictionary-order)))))
    (unless kept
      (user-error "Word not found"))
    ;; `sort' is stable, so entries keep sdcv's order within a rank.
    (sort kept
          (lambda (a b) (< (funcall rank a) (funcall rank b))))))

(defun dict-lookup--word-argument (word)
  "Return WORD as a command-line argument for sdcv, and whether it is UTF-8.
The result is (ARGUMENT . UTF-8-P).  Elsewhere the argument is plain
UTF-8.  Windows hands programs their arguments in the ANSI code page,
which has no room for letters such as Hungarian ő and ű; a word that
fits the code page goes as it is, and any other goes as raw UTF-8
bytes, which reach sdcv unchanged."
  (if (and (eq system-type 'windows-nt)
           (let ((coding (or locale-coding-system 'utf-8)))
             (not (unencodable-char-position 0 (length word) coding nil word))))
      (cons word nil)
    (cons (encode-coding-string word 'utf-8) t)))

(defun dict-lookup--sdcv (word &rest options)
  "Look WORD up with sdcv, passing OPTIONS, and return its parsed results."
  (unless (executable-find quick-sdcv-program)
    (user-error "Cannot find sdcv; set `quick-sdcv-program'"))
  (pcase-let ((`(,argument . ,utf-8) (dict-lookup--word-argument word))
              (errors (make-temp-file "dict-lookup-sdcv")))
    (unwind-protect
        (with-temp-buffer
          (let* ((coding-system-for-read 'utf-8)
                 (process-environment
                  (seq-remove (lambda (v) (string-prefix-p "SDCV_PAGER=" v))
                              process-environment))
                 ;; --non-interactive matters: without it, sdcv asks which
                 ;; of several close matches to show and waits forever.
                 (status (apply #'call-process quick-sdcv-program nil
                                (list t errors) nil
                                "--non-interactive" "--json-output"
                                "--utf8-output" "--only-data-dir" "--data-dir"
                                (expand-file-name dict-lookup-directory)
                                (append (when utf-8 '("--utf8-input"))
                                        (when quick-sdcv-exact-search
                                          '("--exact-search"))
                                        options
                                        (list argument)))))
            (cond
             ((eql status 2) nil)
             ((not (eql status 0))
              (let ((text (with-temp-buffer
                            (insert-file-contents errors)
                            (string-trim (buffer-string)))))
                (error "sdcv failed with exit code %s%s" status
                       (if (string-empty-p text) "" (concat ": " text)))))
             (t
              (goto-char (point-min))
              (unless (search-forward "[" nil t)
                (error "Unexpected sdcv output: %s"
                       (string-trim (buffer-string))))
              (backward-char)
              (append (json-read) nil)))))
      (delete-file errors))))

(defun dict-lookup--call-process (fn &rest args)
  "Run quick-sdcv's lookup FN with ARGS, the dict-lookup way when active."
  (if dict-lookup--active
      (dict-lookup--arrange (apply #'dict-lookup--sdcv args))
    (let ((coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
      (apply fn args))))

(advice-add 'quick-sdcv--call-process :around #'dict-lookup--call-process)

;;;###autoload
(defun dict-lookup (word)
  "Look up WORD in the offline dictionaries.
Interactively, pick WORD from every headword, starting from the
region or the word at point."
  (interactive (list (dict-lookup--read-word)))
  (when (string-empty-p (string-trim word))
    (user-error "No word given"))
  (let ((context (and dict-lookup-save-context
                      (dict-lookup--sentence-at-point word)))
        (dict-lookup--active t))
    (quick-sdcv-search-input word)
    (when-let* ((buf (get-buffer (quick-sdcv--get-buffer-name word))))
      (with-current-buffer buf
        (setq-local dict-lookup--query word)
        (setq-local dict-lookup--context context)))))

;;;###autoload
(defun dict-lookup-at-point ()
  "Look up the region or the word at point without prompting."
  (interactive)
  (dict-lookup (or (dict-lookup--word-at-point)
                   (user-error "No word at point"))))

;;;; Saving words as flashcards

(defun dict-lookup--sentence-at-point (word)
  "Return the sentence at point when it contains WORD, else nil."
  (unless (derived-mode-p 'pdf-view-mode 'minibuffer-mode)
    (when-let* ((sentence (let ((sentence-end-double-space nil))
                           (ignore-errors (thing-at-point 'sentence t)))))
      (let ((sentence (string-trim (replace-regexp-in-string
                                    "[ \t\n]+" " " sentence))))
        (when (and (string-match-p (regexp-quote (downcase word))
                                   (downcase sentence))
                   (< (length word) (length sentence) 400))
          sentence)))))

(defun dict-lookup--entries ()
  "Return the entries of the current result buffer.
Each is a plist (:dict :word :definition :start)."
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (while (re-search-forward "^-->\\(.*\\)\n=>\\(.*\\)\n" nil t)
        (let ((dict (match-string-no-properties 1))
              (word (match-string-no-properties 2))
              (start (match-beginning 0))
              (body (point)))
          (push (list :dict dict :word word :start start
                      :definition
                      (string-trim
                       (buffer-substring-no-properties
                        body (if (re-search-forward "^-->" nil t)
                                 (goto-char (match-beginning 0))
                               (point-max)))))
                entries)))
      (nreverse entries))))

(defun dict-lookup--entry-at-point ()
  "Return the entry of the result buffer that point is in."
  (let ((entries (dict-lookup--entries)))
    (or (car (last (seq-filter (lambda (e) (<= (plist-get e :start) (point)))
                               entries)))
        (car entries))))

(defun dict-lookup--result-buffer ()
  "Return the most recent dict-lookup result buffer."
  (seq-find (lambda (b)
              (with-current-buffer b
                (and (derived-mode-p 'quick-sdcv-mode) dict-lookup--query)))
            (buffer-list)))

(defun dict-lookup--org-text (text)
  "Return TEXT safe to put in the body of an Org entry."
  (replace-regexp-in-string "^\\*" " *" text))

(defconst dict-lookup--no-context "What does it mean?"
  "Body of a card saved without context.
org-drill skips cards whose body is empty, so they get this line.")

(defun dict-lookup--card (entry query context)
  "Return the Org entry saving ENTRY, looked up as QUERY in CONTEXT."
  (concat "* " (plist-get entry :word)
          (if dict-lookup-card-tags
              (concat " :" (string-join dict-lookup-card-tags ":") ":")
            "")
          "\n:PROPERTIES:\n"
          ":DICT_SOURCE: " (plist-get entry :dict) "\n"
          (if (and query (not (string= (downcase query)
                                       (downcase (plist-get entry :word)))))
              (concat ":DICT_QUERY: " query "\n")
            "")
          ":DICT_ADDED: " (format-time-string "[%Y-%m-%d %a]") "\n"
          ":END:\n"
          (dict-lookup--org-text (or context dict-lookup--no-context)) "\n"
          "** Definition\n"
          (dict-lookup--org-text (plist-get entry :definition)) "\n"))

(defun dict-lookup--saved-p (word dict)
  "Return non-nil when WORD from DICT is in the vocabulary file."
  (with-current-buffer (find-file-noselect dict-lookup-vocabulary-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let ((case-fold-search nil) found)
       (while (and (not found)
                   (re-search-forward
                    (concat "^\\* " (regexp-quote word) "\\(?:[ \t]+:.*:\\)?[ \t]*$")
                    nil t))
         (setq found (equal (org-entry-get nil "DICT_SOURCE") dict)))
       found))))

;;;###autoload
(defun dict-lookup-save (&optional choose)
  "Save a looked-up word and its definition as a flashcard.
In a result buffer, save the dictionary entry at point; elsewhere,
the first entry of the last lookup.  With prefix argument CHOOSE,
pick the dictionary entry to save.  The card goes to
`dict-lookup-vocabulary-file', and to Anki too when
`dict-lookup-save-to-anki' is non-nil."
  (interactive "P")
  (let* ((in-result (derived-mode-p 'quick-sdcv-mode))
         (buf (if in-result (current-buffer) (dict-lookup--result-buffer))))
    (unless buf
      (user-error "Look a word up first"))
    (with-current-buffer buf
      (let* ((entries (or (dict-lookup--entries)
                          (user-error "Nothing to save in %s" (buffer-name))))
             (entry
              (cond
               (choose
                (let* ((names (mapcar (lambda (e)
                                        (cons (format "%s (%s)"
                                                      (plist-get e :word)
                                                      (plist-get e :dict))
                                              e))
                                      entries)))
                  (cdr (assoc (completing-read "Save entry: " names nil t)
                              names))))
               (in-result (dict-lookup--entry-at-point))
               (t (car entries))))
             (word (plist-get entry :word))
             (file dict-lookup-vocabulary-file))
        (when (dict-lookup--saved-p word (plist-get entry :dict))
          (user-error "%s from %s is already saved" word (plist-get entry :dict)))
        (let ((card (dict-lookup--card entry dict-lookup--query
                                       dict-lookup--context)))
          (with-current-buffer (find-file-noselect file)
            (org-with-wide-buffer
             (when (= (buffer-size) 0)
               ;; Windows would otherwise pick a code page without ő.
               (set-buffer-file-coding-system 'utf-8-unix)
               (insert "# -*- coding: utf-8 -*-\n"
                       "#+TITLE: Vocabulary\n#+STARTUP: overview\n\n"))
             (goto-char (point-max))
             (insert (cond ((bobp) "")
                           ((looking-back "\n\n" 2) "")
                           ((bolp) "\n")
                           (t "\n\n"))
                     card))
            (let ((save-silently t)) (save-buffer))
            (when dict-lookup-save-to-anki
              (org-with-wide-buffer
               (goto-char (point-max))
               (re-search-backward "^\\* " nil t)
               (dict-lookup--export-entry-to-anki)))))
        (message "Saved %s to %s%s" word (abbreviate-file-name file)
                 (if dict-lookup-save-to-anki " and Anki" ""))))))

(defun dict-lookup-open-entry ()
  "Look up the word of the result entry at point on its own.
After a misspelling, the result buffer lists close matches; this
shows the one at point, keeping the sentence it was looked up from."
  (interactive)
  (unless (derived-mode-p 'quick-sdcv-mode)
    (user-error "Not in a dictionary result buffer"))
  (let* ((entry (or (dict-lookup--entry-at-point)
                    (user-error "No entry at point")))
         (word (plist-get entry :word))
         (context dict-lookup--context))
    (let ((dict-lookup-save-context nil))
      (dict-lookup word))
    (when-let* ((buf (get-buffer (quick-sdcv--get-buffer-name word))))
      (with-current-buffer buf
        (setq-local dict-lookup--context context)))))

(with-eval-after-load 'quick-sdcv
  (define-key quick-sdcv-mode-map (kbd "s") #'dict-lookup-save)
  (define-key quick-sdcv-mode-map (kbd "RET") #'dict-lookup-open-entry))

;;;; Anki

(defun dict-lookup--anki (action &optional params)
  "Call the AnkiConnect ACTION with PARAMS (an alist) and return its result."
  (url-do-setup)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode `((action . ,action) (version . 6)
                          ,@(and params `((params . ,params)))))
           'utf-8))
         ;; Anki runs on this computer, never behind a proxy.
         (url-proxy-services nil)
         (buf (url-retrieve-synchronously dict-lookup-anki-connect-url t t 15)))
    (unless buf
      (error "No answer from AnkiConnect at %s" dict-lookup-anki-connect-url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (or (bound-and-true-p url-http-end-of-headers) (point-min)))
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (response (json-read-from-string
                            (decode-coding-string
                             (buffer-substring-no-properties (point) (point-max))
                             'utf-8))))
            (when (cdr (assq 'error response))
              (error "AnkiConnect: %s" (cdr (assq 'error response))))
            (cdr (assq 'result response))))
      (kill-buffer buf))))

(defun dict-lookup--anki-reachable-p ()
  "Return non-nil when AnkiConnect answers."
  (condition-case nil (progn (dict-lookup--anki "version") t) (error nil)))

(defun dict-lookup--html (text)
  "Return TEXT as HTML that keeps its line breaks and indentation."
  (concat "<div style=\"text-align:left; white-space:pre-wrap\">"
          (replace-regexp-in-string
           ">" "&gt;" (replace-regexp-in-string
                       "<" "&lt;" (replace-regexp-in-string "&" "&amp;" text)))
          "</div>"))

(defun dict-lookup--entry-fields ()
  "Return (FRONT . BACK) as HTML for the saved word at point."
  (org-back-to-heading t)
  (let* ((word (org-get-heading t t t t))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (context (save-excursion
                    (org-end-of-meta-data t)
                    (string-trim
                     (buffer-substring-no-properties
                      (point) (if (re-search-forward "^\\*+ " end t)
                                  (match-beginning 0)
                                end)))))
         (definition (save-excursion
                       (if (re-search-forward "^\\*\\* Definition[ \t]*\n" end t)
                           (string-trim
                            (buffer-substring-no-properties
                             (point) (if (re-search-forward "^\\*+ " end t)
                                         (match-beginning 0)
                                       end)))
                         ""))))
    (cons (concat (dict-lookup--html word)
                  (if (member context (list "" dict-lookup--no-context)) ""
                    (concat "<div style=\"font-size:small\"><i>"
                            (dict-lookup--html context) "</i></div>")))
          (concat (dict-lookup--html definition)
                  "<div style=\"font-size:small\">"
                  (dict-lookup--html (or (org-entry-get nil "DICT_SOURCE") ""))
                  "</div>"))))

(defun dict-lookup--export-entry-to-anki ()
  "Add the saved word at point to Anki and record that on the entry."
  (pcase-let ((`(,model ,front-field ,back-field) dict-lookup-anki-model)
              (`(,front . ,back) (dict-lookup--entry-fields)))
    (dict-lookup--anki "createDeck" `((deck . ,dict-lookup-anki-deck)))
    (let ((id (condition-case err
                  (dict-lookup--anki
                   "addNote"
                   `((note . ((deckName . ,dict-lookup-anki-deck)
                              (modelName . ,model)
                              (fields . ((,(intern front-field) . ,front)
                                         (,(intern back-field) . ,back)))
                              (tags . ,(vconcat dict-lookup-anki-tags))
                              (options . ((allowDuplicate . :json-false)))))))
                (error (if (string-match-p "duplicate" (error-message-string err))
                           "duplicate"
                         (signal (car err) (cdr err)))))))
      (org-entry-put nil "ANKI_NOTE_ID" (format "%s" id))
      (org-entry-put nil "ANKI_EXPORTED" (format-time-string "[%Y-%m-%d %a]")))))

(defun dict-lookup--tsv-field (string)
  "Quote STRING as a field of Anki's import file."
  (concat "\"" (replace-regexp-in-string
                "\"" "\"\"" (replace-regexp-in-string "[\t\n\r]+" " " string))
          "\""))

(defun dict-lookup--export-entry-to-file (file)
  "Append the saved word at point to the Anki import FILE."
  (pcase-let ((`(,model . ,_) dict-lookup-anki-model)
              (`(,front . ,back) (dict-lookup--entry-fields)))
    (let ((new (not (and (file-exists-p file)
                         (> (file-attribute-size (file-attributes file)) 0)))))
      (with-temp-buffer
        (when new
          (insert "#separator:Tab\n#html:true\n#notetype column:1\n"
                  "#deck column:2\n#tags column:5\n"))
        ;; Fields are one line each, so line breaks become <br> tags.
        (insert (mapconcat #'dict-lookup--tsv-field
                           (list model dict-lookup-anki-deck
                                 (replace-regexp-in-string "\n" "<br>" front)
                                 (replace-regexp-in-string "\n" "<br>" back)
                                 (string-join dict-lookup-anki-tags " "))
                           "\t")
                "\n")
        (make-directory (file-name-directory file) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (point-min) (point-max) file (not new) 'silent))))
    (org-entry-put nil "ANKI_EXPORTED" (format-time-string "[%Y-%m-%d %a]"))))

;;;###autoload
(defun dict-lookup-export-anki ()
  "Send the saved words that Anki doesn't have yet to Anki.
Uses the AnkiConnect add-on when Anki is running; otherwise offers to
write `dict-lookup-anki-export-file' for Anki's File > Import."
  (interactive)
  (let* ((file (expand-file-name dict-lookup-anki-export-file))
         (method (cond ((dict-lookup--anki-reachable-p) 'anki)
                       ((y-or-n-p "Cannot reach Anki (is it running, with \
AnkiConnect?).  Write an import file instead? ")
                        'file)
                       (t (user-error "Nothing exported"))))
         (count 0))
    (with-current-buffer (find-file-noselect dict-lookup-vocabulary-file)
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (unless (org-entry-get nil "ANKI_EXPORTED")
            (if (eq method 'anki)
                (dict-lookup--export-entry-to-anki)
              (dict-lookup--export-entry-to-file file))
            (setq count (1+ count))))
        "LEVEL=1+DICT_SOURCE={.}" 'file))
      (let ((save-silently t)) (save-buffer)))
    (message (if (eq method 'anki)
                 "Sent %d word%s to Anki"
               (concat "Wrote %d word%s to " (abbreviate-file-name file)
                       "; import it in Anki with File > Import"))
             count (if (= count 1) "" "s"))))

(provide 'dict-lookup)
;;; dict-lookup.el ends here
