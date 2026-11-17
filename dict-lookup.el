;;; dict-lookup.el --- Offline dictionary lookup with sdcv and consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Hormoz Zamanpour Siahkal

;; Author: Hormoz Zamanpour Siahkal <n0w1ht@users.noreply.github.com>
;; URL: https://github.com/n0w1ht/dict-lookup
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1") (quick-sdcv "1.0") (card-export "0.1.0"))
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
;; `dict-lookup-at-point' looks up the region or the word at point at once;
;; in pdf-tools and pdf-canvas buffers it takes the selected text.
;; `dict-lookup-dictionary-order' puts chosen dictionaries first, and
;; `dict-lookup-excluded-dictionaries' leaves some out;
;; `dict-lookup-list-dictionaries' shows the names they take.
;;
;; In the result buffer, `RET' (`dict-lookup-open-entry') looks up the
;; close match at point on its own, and `s' (`dict-lookup-save') saves
;; the entry at point as a flashcard in `dict-lookup-vocabulary-file', an
;; Org file that org-srs can review as it is (`org-srs-review-start'),
;; with the sentence it came from (PDFs too) and the book and page; `p'
;; (`dict-lookup-pronounce') says the word aloud with kokoro.el.
;; `dict-lookup-export-anki' sends the saved words that Anki doesn't have
;; yet to Anki.

;;; Code:

(require 'cl-lib)
(require 'char-fold)
(require 'quick-sdcv)
(require 'thingatpt)
(require 'json)
(require 'org)
(require 'card-export)

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

(defcustom dict-lookup-fold-diacritics t
  "When non-nil, plain letters in `dict-lookup' completion match accented ones.
Typing \"o\" then also finds ó, ö and ő, so \"szolo\" finds \"szőlő\".
An accented letter still matches only itself."
  :type 'boolean)

(defcustom dict-lookup-vocabulary-file
  (expand-file-name "vocabulary.org"
                    (if (boundp 'org-directory) org-directory "~/org"))
  "Org file where `dict-lookup-save' saves words as flashcards."
  :type 'file)

(defcustom dict-lookup-card-tags nil
  "Tags of each saved word."
  :type '(repeat string))

(defcustom dict-lookup-save-context t
  "When non-nil, save the sentence the word was looked up from.
It shows on the front of the card, so you recall the word in context."
  :type 'boolean)

(defcustom dict-lookup-save-source t
  "When non-nil, save where the word was looked up with its card.
That is the book or file and, in a PDF, the page, such as
\"guyton-physiology, p. 112\".  It shows under the sentence on the
front of the card and in the DICT_FROM property."
  :type 'boolean)

(defcustom dict-lookup-speak-function #'dict-lookup-speak-kokoro
  "Function that reads a string aloud, for `dict-lookup-pronounce'.
It is called with the string.  The default uses kokoro.el."
  :type 'function)

(defcustom dict-lookup-save-to-anki nil
  "When non-nil, `dict-lookup-save' also sends the word to Anki."
  :type 'boolean)

(define-obsolete-variable-alias 'dict-lookup-anki-connect-url
  'card-export-anki-connect-url "0.5.0")

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

(defvar-local dict-lookup--source nil
  "Where the word of this result buffer was looked up, such as a book and page.")

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

(declare-function pdf-canvas-selections "pdf-canvas")

(defun dict-lookup--pdf-canvas-text ()
  "Return the text selected in a pdf-canvas buffer, or nil.
Several selections are joined with spaces; areas selected as figures
\(with \`M-drag') are left out."
  (when-let* (((fboundp 'pdf-canvas-selections))
              (texts (delq nil
                           (mapcar (lambda (selection)
                                     (unless (plist-get selection :figure)
                                       (plist-get selection :text)))
                                   (ignore-errors (pdf-canvas-selections)))))
              (text (string-trim (replace-regexp-in-string
                                  "[ \t\n]+" " "
                                  (mapconcat #'identity texts " ")))))
    (unless (string-empty-p text) text)))

(declare-function pdf-view-active-region "pdf-view")
(declare-function pdf-view-current-page "pdf-view")
(declare-function pdf-info-gettext "pdf-info")
(declare-function pdf-canvas--text-in "ext:pdf-canvas-module")
(defvar pdf-canvas--doc)
(defvar nov-metadata)
(defvar nov-file-name)
(defvar eww-data)

(defun dict-lookup--pdf-band (edges)
  "Return EDGES (LEFT TOP RIGHT BOTTOM), widened to whole lines around it.
The result reaches a few lines above and below, across the page, so it
holds the sentence the selection is in."
  (list 0.0 (max 0.0 (- (float (nth 1 edges)) 0.08))
        1.0 (min 1.0 (+ (float (nth 3 edges)) 0.08))))

(defun dict-lookup--pdf-canvas-around ()
  "Return (TEXT . PAGE) around the first text selection in pdf-canvas.
TEXT is the text of a few lines around it; PAGE counts from 1."
  (when-let* (((fboundp 'pdf-canvas-selections))
              (selection (seq-find (lambda (s) (not (plist-get s :figure)))
                                   (ignore-errors (pdf-canvas-selections))))
              (page (plist-get selection :page)))
    (cons (ignore-errors
            (apply #'pdf-canvas--text-in pdf-canvas--doc (1- page)
                   (dict-lookup--pdf-band (plist-get selection :edges))))
          page)))

(defun dict-lookup--pdf-tools-around ()
  "Return (TEXT . PAGE) around the active region in pdf-tools."
  (when-let* (((pdf-view-active-region-p))
              (regions (ignore-errors (pdf-view-active-region)))
              (first (car regions))
              ((numberp (car-safe first)))
              (page (pdf-view-current-page)))
    (cons (ignore-errors
            (pdf-info-gettext page
                              (dict-lookup--pdf-band
                               (list 0 (nth 1 first) 1
                                     (nth 3 (car (last regions)))))))
          page)))

(defun dict-lookup--sentence-in (text word)
  "Return the sentence of TEXT that holds WORD, or nil.
Words split over two lines with a hyphen are joined first."
  (when (and (stringp text) (not (string-empty-p word)))
    (let* ((case-fold-search t)
           (text (replace-regexp-in-string
                  "[ \t\n\r]+" " "
                  (replace-regexp-in-string
                   "\\([[:alpha:]]\\)-[ \t]*\n[ \t]*\\([[:lower:]]\\)" "\\1\\2" text)))
           (word-re (replace-regexp-in-string
                     " " "[ \t\n]+" (regexp-quote (string-trim word)))))
      (when (string-match word-re text)
        (let* ((wbeg (match-beginning 0))
               (wend (match-end 0))
               (beg (save-match-data
                      (let ((pos 0) (last 0))
                        (while (and (string-match "[.!?][])\"'”’]*\\(?: \\|$\\)"
                                                  text pos)
                                    (<= (match-end 0) wbeg))
                          (setq last (match-end 0) pos (match-end 0)))
                        last)))
               (end (if (string-match "[.!?][])\"'”’]*\\(?: \\|$\\)" text wend)
                        (match-end 0)
                      (length text)))
               (sentence (string-trim (substring text beg end))))
          (when (> (length sentence) 400)
            ;; Probably not a sentence at all (a table or a caption run
            ;; together): keep the words around WORD.
            (let ((from (max 0 (- wbeg beg 150)))
                  (to (min (length sentence) (+ (- wend beg) 150))))
              (setq sentence (concat (if (> from 0) "…" "")
                                     (string-trim (substring sentence from to))
                                     (if (< to (length sentence)) "…" "")))))
          (and (< (length word) (length sentence)) sentence))))))

(defun dict-lookup--book-name ()
  "Return the name of the book or file of the current buffer, or nil."
  (cond
   ((and (derived-mode-p 'nov-mode) (boundp 'nov-metadata))
    (or (alist-get 'title nov-metadata)
        (and (bound-and-true-p nov-file-name) (file-name-base nov-file-name))))
   ((and (derived-mode-p 'eww-mode) (boundp 'eww-data))
    (let ((title (plist-get eww-data :title)))
      (if (and title (not (string-empty-p title))) title (plist-get eww-data :url))))
   ((buffer-file-name) (file-name-base (buffer-file-name)))))

(defun dict-lookup--source (&optional page)
  "Return where the current buffer is, such as \"book, p. 12\", or nil.
PAGE is the page in a PDF, counting from 1."
  (when-let* ((book (dict-lookup--book-name)))
    (if page (format "%s, p. %d" book page) book)))

(defun dict-lookup--context-at-point (word)
  "Return (SENTENCE . SOURCE) for WORD looked up in the current buffer.
Either can be nil: SENTENCE is the sentence WORD came from, SOURCE
says where (see `dict-lookup--source')."
  (let ((around (cond ((derived-mode-p 'pdf-canvas-mode)
                       (dict-lookup--pdf-canvas-around))
                      ((derived-mode-p 'pdf-view-mode)
                       (dict-lookup--pdf-tools-around)))))
    (cons (if (derived-mode-p 'pdf-canvas-mode 'pdf-view-mode)
              (dict-lookup--sentence-in (car around) word)
            (dict-lookup--sentence-at-point word))
          (and (not (derived-mode-p 'minibuffer-mode))
               (dict-lookup--source
                (or (cdr around)
                    (and (derived-mode-p 'pdf-view-mode)
                         (pdf-view-current-page))))))))

(defun dict-lookup--word-at-point ()
  "Return the active region or the word at point, or nil.
In a pdf-tools or pdf-canvas buffer, return the selected text (in
pdf-canvas a double click selects a word)."
  (cond
   ((and (derived-mode-p 'pdf-view-mode) (pdf-view-active-region-p))
    (string-trim (mapconcat #'identity (pdf-view-active-region-text) " ")))
   ((derived-mode-p 'pdf-view-mode) nil)
   ((derived-mode-p 'pdf-canvas-mode) (dict-lookup--pdf-canvas-text))
   ((use-region-p)
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end))))
   (t (thing-at-point 'word t))))

(defun dict-lookup--fold-highlight (regexp candidate)
  "Return CANDIDATE with the first match of REGEXP highlighted."
  (if (string-match regexp candidate)
      (let ((candidate (copy-sequence candidate)))
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'completions-common-part nil candidate)
        candidate)
    candidate))

(defun dict-lookup--fold-all-completions (string table pred _point)
  "Return the candidates of TABLE and PRED containing STRING, accents folded.
This is the `all-completions' function of the `dict-lookup-fold' style."
  (let* ((regexp (char-fold-to-regexp string))
         (case-fold-search completion-ignore-case)
         (matches (let ((completion-regexp-list (list regexp)))
                    (all-completions "" table pred))))
    (when matches
      (if (bound-and-true-p completion-lazy-hilit)
          (setq completion-lazy-hilit-fn
                (lambda (c) (dict-lookup--fold-highlight regexp c)))
        (setq matches (mapcar (lambda (c) (dict-lookup--fold-highlight regexp c))
                              matches)))
      (nconc matches 0))))

(defun dict-lookup--fold-try-completion (string table pred point)
  "Return the `try-completion' result of the `dict-lookup-fold' style.
STRING is left as it is, since its matches needn't share a prefix.
TABLE, PRED and POINT are as in `completion-try-completion'."
  (let ((matches (dict-lookup--fold-all-completions string table pred point)))
    (cond ((null matches) nil)
          ((and (null (cdr matches)) (string= (car matches) string)) t)
          (t (cons string point)))))

(add-to-list 'completion-styles-alist
             '(dict-lookup-fold
               dict-lookup--fold-try-completion dict-lookup--fold-all-completions
               "Substring completion where plain letters match accented ones."))

(defvar orderless-matching-styles)

(defmacro dict-lookup--with-folding (&rest body)
  "Run BODY with completion folding diacritics, per `dict-lookup-fold-diacritics'.
With orderless, each component also matches with its accents folded;
with other styles, the `dict-lookup-fold' style is tried first."
  (declare (indent 0))
  `(if (not dict-lookup-fold-diacritics)
       (progn ,@body)
     (let ((orderless-matching-styles
            (if (boundp 'orderless-matching-styles)
                (append orderless-matching-styles '(char-fold-to-regexp))
              '(orderless-literal orderless-regexp char-fold-to-regexp)))
           (completion-styles
            (if (memq 'orderless completion-styles)
                completion-styles
              (cons 'dict-lookup-fold completion-styles))))
       ,@body)))

(defun dict-lookup--read-word ()
  "Read a headword, offering every dictionary entry for completion."
  (let ((default (dict-lookup--word-at-point))
        (words (dict-lookup-headwords))
        (prompt "Look up: "))
    (dict-lookup--with-folding
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
                        'dict-lookup-history default)))))

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
  (let ((context (and (or dict-lookup-save-context dict-lookup-save-source)
                      (dict-lookup--context-at-point word)))
        (dict-lookup--active t))
    (quick-sdcv-search-input word)
    (when-let* ((buf (get-buffer (quick-sdcv--get-buffer-name word))))
      (with-current-buffer buf
        (setq-local dict-lookup--query word)
        (setq-local dict-lookup--context
                    (and dict-lookup-save-context (car context)))
        (setq-local dict-lookup--source
                    (and dict-lookup-save-source (cdr context)))))))

;;;###autoload
(defun dict-lookup-at-point ()
  "Look up the region or the word at point without prompting."
  (interactive)
  (dict-lookup (or (dict-lookup--word-at-point)
                   (user-error "No word at point"))))

;;;; Saving words as flashcards

(defun dict-lookup--sentence-at-point (word)
  "Return the sentence at point when it contains WORD, else nil."
  (unless (derived-mode-p 'pdf-view-mode 'pdf-canvas-mode 'minibuffer-mode)
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

(defconst dict-lookup--no-context "What does it mean?"
  "Body of a card saved without context, so its front asks something.")

(defun dict-lookup--card (entry query context &optional source)
  "Return the Org entry saving ENTRY, looked up as QUERY in CONTEXT.
SOURCE says where, such as a book and page."
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
          (if source (concat ":DICT_FROM: " source "\n") "")
          ":DICT_ADDED: " (format-time-string "[%Y-%m-%d %a]") "\n"
          ":END:\n"
          (card-export-org-body (or context dict-lookup--no-context)) "\n"
          (if source (concat "— " (card-export-org-body source) "\n") "")
          "** Back\n"
          (card-export-org-body (plist-get entry :definition)) "\n"))

(defun dict-lookup--make-srs-card ()
  "Make the last saved word an org-srs flashcard, when org-srs is installed.
Its heading and sentence are the front, and Back is the answer."
  (when (card-export-srs-available-p)
    (save-excursion
      (goto-char (point-max))
      (re-search-backward "^\\* ")
      (card-export-srs-make-card))))

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
             (file dict-lookup-vocabulary-file)
             (sent nil))
        (when (dict-lookup--saved-p word (plist-get entry :dict))
          (user-error "%s from %s is already saved" word (plist-get entry :dict)))
        (let ((card (dict-lookup--card entry dict-lookup--query
                                       dict-lookup--context
                                       dict-lookup--source)))
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
                     card)
             (dict-lookup--make-srs-card))
            ;; Anki down: the word goes with the next export instead.
            (when (and dict-lookup-save-to-anki (card-export-anki-reachable-p))
              (org-with-wide-buffer
               (goto-char (point-max))
               (re-search-backward "^\\* " nil t)
               (setq sent (card-export-anki-org-export
                           (list (point-marker)) #'dict-lookup--note
                           (expand-file-name dict-lookup-anki-export-file)))
               (dict-lookup--report-failed (plist-get sent :failed))))
            (let ((save-silently t)) (save-buffer))))
        (message "Saved %s to %s%s" word (abbreviate-file-name file)
                 (cond ((not dict-lookup-save-to-anki) "")
                       ((eql (plist-get sent :added) 1) " and Anki")
                       (t " (not to Anki: run M-x dict-lookup-export-anki later)")))))))

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
         (context dict-lookup--context)
         (source dict-lookup--source))
    (let ((dict-lookup-save-context nil)
          (dict-lookup-save-source nil))
      (dict-lookup word))
    (when-let* ((buf (get-buffer (quick-sdcv--get-buffer-name word))))
      (with-current-buffer buf
        (setq-local dict-lookup--context context)
        (setq-local dict-lookup--source source)))))

;;;; Pronunciation

(declare-function kokoro-read-string "kokoro" (string &optional display))

(defun dict-lookup-speak-kokoro (string)
  "Read STRING aloud with kokoro.el."
  (unless (require 'kokoro nil t)
    (user-error "Pronouncing needs kokoro.el (or set `dict-lookup-speak-function')"))
  (kokoro-read-string string))

(defun dict-lookup--spoken-definition (definition)
  "Return DEFINITION cleaned for reading aloud.
Phonetic spellings, bracketed notes and markup are left out."
  (let ((text definition))
    (dolist (re '("<[^>]+>" "^/[^/\n]*/$" "\\[[^]\n]*\\]" "^<<[^>]*>>$"))
      (setq text (replace-regexp-in-string re "" text)))
    (string-trim (replace-regexp-in-string "[ \t\n]+" " " text))))

;;;###autoload
(defun dict-lookup-pronounce (&optional definition)
  "Say the word of the entry at point aloud.
Outside a result buffer, say the selected word or the word at point.
With prefix argument DEFINITION, read the entry's definition aloud
too.  Uses `dict-lookup-speak-function' (kokoro.el by default)."
  (interactive "P")
  (let* ((entry (and (derived-mode-p 'quick-sdcv-mode)
                     (dict-lookup--entry-at-point)))
         (word (if (derived-mode-p 'quick-sdcv-mode)
                   (or (plist-get entry :word) dict-lookup--query)
                 (dict-lookup--word-at-point))))
    (unless word
      (user-error "No word to say"))
    (funcall dict-lookup-speak-function
             (if (and definition entry)
                 (concat word ". "
                         (dict-lookup--spoken-definition
                          (plist-get entry :definition)))
               word))))

(with-eval-after-load 'quick-sdcv
  (define-key quick-sdcv-mode-map (kbd "s") #'dict-lookup-save)
  (define-key quick-sdcv-mode-map (kbd "RET") #'dict-lookup-open-entry)
  (define-key quick-sdcv-mode-map (kbd "p") #'dict-lookup-pronounce))

;;;; Anki

(defun dict-lookup--html (text)
  "Return TEXT as HTML that keeps its line breaks and indentation."
  (concat "<div style=\"text-align:left; white-space:pre-wrap\">"
          (card-export-html-escape text)
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
                       ;; Cards saved for org-drill call it Definition.
                       (if (re-search-forward
                            "^\\*\\* \\(?:Back\\|Definition\\)[ \t]*\n" end t)
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

(defun dict-lookup--note ()
  "Return the Anki note, a card-export plist, for the saved word at point.
Line breaks in its fields are <br>, so they survive the import file."
  (pcase-let ((`(,model ,front-field ,back-field) dict-lookup-anki-model)
              (`(,front . ,back) (dict-lookup--entry-fields)))
    (list :deck dict-lookup-anki-deck :model model
          :fields `((,front-field . ,(replace-regexp-in-string "\n" "<br>" front))
                    (,back-field . ,(replace-regexp-in-string "\n" "<br>" back)))
          :tags dict-lookup-anki-tags)))

(defun dict-lookup--saved-words ()
  "Return markers to the saved words of the vocabulary file (current buffer)."
  (org-map-entries #'point-marker "LEVEL=1+DICT_SOURCE={.}" 'file))

(defun dict-lookup--report-failed (failed)
  "Warn about the words Anki refused: FAILED is a list of (WORD . MESSAGE)."
  (when failed
    (display-warning 'dict-lookup
                     (concat "Some words could not be sent to Anki:\n"
                             (mapconcat (lambda (f) (format "- %s: %s" (car f) (cdr f)))
                                        failed "\n")))))

;;;###autoload
(defun dict-lookup-export-anki ()
  "Send the saved words that Anki doesn't have yet to Anki.
Uses the AnkiConnect add-on when Anki is running; then the words you
edited since sending them are updated in Anki too.  Otherwise offers
to write `dict-lookup-anki-export-file' for Anki's File > Import."
  (interactive)
  (let* ((file (expand-file-name dict-lookup-anki-export-file))
         (result (with-current-buffer (find-file-noselect dict-lookup-vocabulary-file)
                   (prog1 (org-with-wide-buffer
                           (card-export-anki-org-export (dict-lookup--saved-words)
                                                        #'dict-lookup--note file))
                     (let ((save-silently t)) (save-buffer)))))
         (added (plist-get result :added))
         (updated (plist-get result :updated)))
    (dict-lookup--report-failed (plist-get result :failed))
    (pcase (plist-get result :method)
      ('file (message "Wrote %d word%s to %s; import it in Anki with File > Import"
                      added (if (= added 1) "" "s") (abbreviate-file-name file)))
      ('anki (message "Sent %d new word%s to Anki%s" added (if (= added 1) "" "s")
                      (if (> updated 0) (format ", updated %d" updated) "")))
      (_ (message "Nothing sent to Anki")))))

(provide 'dict-lookup)
;;; dict-lookup.el ends here
