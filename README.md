# Offline dictionary in Emacs

Offline dictionaries shown inside Emacs. English, in [`dictionaries/english.zip`](dictionaries/english.zip):

- **WordNet 3.0**: modern definitions, synonyms, multi-word entries ("heart attack").
- **Webster's 1913 (GCIDE)**: rich older definitions with etymology and quotations.

Hungarian, in [`dictionaries/hungarian.zip`](dictionaries/hungarian.zip):

- **Hungarian-English Wiktionary**: the best of the three, with numbered
  senses. It knows about 1.5 million inflected forms, so `házakban` or
  `erőművek` finds `ház` or `erőmű`.
- **FreeDict Hungarian-English**: more headwords, but only short lists of
  translations.
- **FreeDict English-Hungarian**: the only free English-to-Hungarian one.
  Its translation lists are rough and not ordered by how common they are.

Lookups go through `sdcv` (the StarDict command-line reader) and the
`quick-sdcv` package. `dict-lookup.el` adds a consult picker over every
headword in all dictionaries (about 464,000 with Hungarian), so you can type part of a
phrase and pick it.

## Install (Windows)

1. Install MSYS2, which is how sdcv is packaged for Windows:

   ```
   winget install MSYS2.MSYS2
   ```

2. Open **MSYS2 UCRT64** from the Start menu and run:

   ```
   pacman -Syu
   pacman -S mingw-w64-ucrt-x86_64-sdcv
   ```

   If the first command closes the window, reopen it and run it again
   before the second one. sdcv ends up at `C:\msys64\ucrt64\bin\sdcv.exe`.
   You don't need to add it to PATH.

3. Download the zips you want from [`dictionaries/`](dictionaries/) and
   unzip them into a `dict` folder in
   your Emacs directory, so you get `.../.emacs.d/dict/wordnet/`,
   `.../.emacs.d/dict/wiktionary-hu-en/` and so on.
   `M-: (locate-user-emacs-file "dict/")` shows the exact folder.

4. Copy `dict-lookup.el` to `.emacs.d/lisp/` and add to your init file:

   ```elisp
   (use-package quick-sdcv
     :ensure t
     :custom
     (quick-sdcv-program "C:/msys64/ucrt64/bin/sdcv.exe"))

   (use-package dict-lookup
     :load-path "lisp/"
     :bind (("C-c d" . dict-lookup)
            ("C-c D" . dict-lookup-at-point)))
   ```

## Use

- `C-c d`: pick a word from every headword, starting from the word at
  point or the selected text. You can also type a word that isn't in the
  list and press `RET`; sdcv then shows its closest matches.
- `C-c D`: look up the word at point (or the selection) straight away.
  It works in text, Org, EPUB (nov), eww, and in pdf-tools when you
  select text with the mouse.
- After a misspelling, the result buffer lists close matches. Press `RET`
  on one to look that word up on its own.
- In the result buffer, `q` closes it.

## Saving words as flashcards

In the result buffer, press `s` to save the dictionary entry the cursor
is in. Each word becomes an entry in `vocabulary.org`, in your
`org-directory`:

```org
* ház :drill:
:PROPERTIES:
:DICT_SOURCE: Hungarian-English Wiktionary dictionary (hu-en)
:DICT_QUERY: házakban
:DICT_ADDED: [2026-10-20 Tue]
:END:
A nagy házakban sokan laknak.
** Definition
noun
  1. house, building (closed structure with walls and a roof)
  ...
```

- The sentence you looked the word up from is saved under it, so the
  card shows the word in context. Without a sentence, the card says
  "What does it mean?".
- `M-x dict-lookup-save` works from anywhere: it saves the first entry of
  your last lookup, and with `C-u` it lets you pick which dictionary's
  entry to save.
- Saving the same word from the same dictionary twice is refused.

**org-drill:** run `M-x org-drill` in `vocabulary.org`. The word and
sentence are the question, and the definition is the answer.

**Anki:** `M-x dict-lookup-export-anki` sends every saved word that Anki
doesn't have yet to the "Vocabulary" deck as Basic cards, through
AnkiConnect (the add-on readq already uses). If Anki isn't running, it
offers to write an import file for File > Import instead. Sent words get
an `ANKI_EXPORTED` property, so they are never sent twice. To send each
word to Anki as you save it:

```elisp
(setq dict-lookup-save-to-anki t)
```

Other settings: `dict-lookup-vocabulary-file`, `dict-lookup-anki-deck`,
`dict-lookup-anki-model` (if your note type isn't "Basic" with Front and
Back), `dict-lookup-card-tags` and `dict-lookup-save-context`.

## Choosing which dictionary comes first

`M-x dict-lookup-list-dictionaries` shows the names of your dictionaries.
Use those names in your init file, for example:

```elisp
;; Show WordNet before Webster's
(setq dict-lookup-dictionary-order '("WordNet 3.0"))

;; Or hide Webster's from results and from the picker
(setq dict-lookup-excluded-dictionaries '("Webster's 1913 (GCIDE)"))
```

For Hungarian, putting Wiktionary ahead of FreeDict reads best:

```elisp
(setq dict-lookup-dictionary-order
      '("Hungarian-English Wiktionary dictionary (hu-en)"
        "WordNet 3.0"))
```

Dictionaries left out of the order list follow the ones you named. You
can also set both options with `M-x customize-group RET dict-lookup`.

## Adding more dictionaries

Any StarDict dictionary (a folder with `.ifo`, `.idx` and `.dict` or
`.dict.dz` files) works: drop its folder into `.emacs.d/dict/` and it is
picked up on the next lookup.

## Rebuilding the dictionaries

The zips were made with the scripts in [`tools/`](tools/), which need only
Python 3:

- `dictd2stardict.py INDEX DICT.DZ OUTDIR NAME BOOKNAME [hu-accents]`
  converts a dictd dictionary (Debian's `dict-wn`, `dict-gcide`,
  `dict-freedict-*`) to plain-text StarDict.  `hu-accents` repairs the
  ô/û that FreeDict English-Hungarian writes for ő/ű.
- `stardict-html2text.py IFO OUTDIR NAME` turns an HTML StarDict
  dictionary (such as Vuizur's Wiktionary ones) into plain text, keeping
  its inflected forms.

Compress each `.dict` with `dictzip` afterwards.

## Licences

`dict-lookup.el` and the scripts are under the GPL, version 3 or later.

The dictionaries keep their own licences: WordNet the Princeton WordNet
licence, GCIDE and FreeDict the GPL, and the Hungarian Wiktionary
CC BY-SA 3.0 and the GFDL. Each dictionary folder in the zips has its
`LICENSE.txt`.
