;;; test-ox-latex.el --- tests for ox-latex.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ihor Radchenko

;; Author: Ihor Radchenko <yantar92@posteo.net>

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

;; Tests checking validity of Org LaTeX export output.

;;; Code:

(require 'org-test "../testing/org-test")

(require 'ox-latex nil t)
(unless (featurep 'ox-latex)
  (signal 'missing-test-dependency '("org-export-latex")))



(ert-deftest test-ox-latex/protect-square-brackets ()
  "Test [foo] being interpreted as plain text even after LaTeX commands."
  (org-test-with-exported-text
      'latex
      "* This is test
lorem @@latex:\\pagebreak@@ [ipsum]

#+begin_figure
[lorem] figure
#+end_figure

| [foo] | 2 |
| [bar] | 3 |

- [bax]
- [aur]
"
    (goto-char (point-min))
    (should (search-forward "lorem \\pagebreak {[}ipsum]"))
    (should (search-forward "{[}lorem] figure"))
    (should (search-forward "{[}foo]"))
    (should (search-forward "\\item {[}bax]"))))

(ert-deftest test-ox-latex/verse ()
  "Test verse blocks."
  (org-test-with-exported-text
      'latex
      "#+begin_verse
lorem ipsum dolor
lorem ipsum dolor

lorem ipsum dolor
lorem ipsum dolor

lorem ipsum dolor
lorem ipsum dolor
#+end_verse
"
    (goto-char (point-min))
    (should
     (search-forward
      "\\begin{verse}
lorem ipsum dolor\\\\
lorem ipsum dolor

lorem ipsum dolor\\\\
lorem ipsum dolor

lorem ipsum dolor\\\\
lorem ipsum dolor\\\\
\\end{verse}")))
  ;; Footnotes inside verse blocks

  (org-test-with-exported-text
      'latex
      "#+begin_verse
lorem
ipsum[fn::Foo

bar]
dolor
#+end_verse

[fn:1] Lorem ipsum dolor sit amet, consectetuer adipiscing elit.
Donec hendrerit tempor.

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec
hendrerit tempor tellus.
"
    (goto-char (point-min))
    (should
     (search-forward
      "\\begin{verse}
lorem\\\\
ipsum\\footnote{Foo

bar}\\\\
dolor\\\\
\\end{verse}"))))

(ert-deftest test-ox-latex/longtable ()
  "Test table export with longtable environment."
  (org-test-with-exported-text
      'latex
      "#+attr_latex: :environment longtable
| First        | Second |
| Column       | Column |
|--------------+--------|
| a            |      1 |
| b            |      2 |
| \\pagebreak c |      3 |
| d            |      4 |
"
    (goto-char (point-min))
    (should
     (search-forward
      "\\begin{longtable}{lr}
First & Second\\\\
Column & Column\\\\
\\hline
\\endfirsthead"))
    (goto-char (point-min))
    (should
     (search-forward
      "First & Second\\\\
Column & Column \\\\

\\hline
\\endhead"))
    (goto-char (point-min))
    (should
     (search-forward
      "\\hline\\multicolumn{2}{r}{Continued on next page} \\\\
\\endfoot"))))

(ert-deftest test-ox-latex/table-el-table ()
  "Test table export with table.el table and :rmlines."
  (org-test-with-exported-text
      'latex
      "#+attr_latex: :rmlines yes
+--------------------------+-----------+
|   ... better than ...    | ... times |
+--------------+-----------+-----------+
| PostgreSQL   | MySQL     |     2     |
+--------------+-----------+-----------+
| PostgreSQL   | MongoDB   |     2     |
+--------------+-----------+-----------+
| MongoDB      | MySQL     |     2     |
+--------------+-----------+-----------+
"
    (goto-char (point-min))
    (should
     (search-forward
      "\\begin{tabular}{|l|l|l|}
\\multicolumn{2}{|l|}{... better than ...} & ... times \\\\
\\hline
PostgreSQL & MySQL & 2 \\\\
PostgreSQL & MongoDB & 2 \\\\
MongoDB & MySQL & 2 \\\\
\\end{tabular}"
      ))))

(ert-deftest test-ox-latex/inline-image ()
  "Test inline images."
  (org-test-with-exported-text
      'latex
      "#+caption: Schematic
[[https://orgmode.org/worg/images/orgmode/org-mode-unicorn.svg][file:/wallpaper.png]]"
    (goto-char (point-min))
    (should
     (search-forward
      "\\href{https://orgmode.org/worg/images/orgmode/org-mode-unicorn.svg}{\\includegraphics[width=.9\\linewidth]{/wallpaper.png}}"))))

(ert-deftest test-ox-latex/num-t ()
  "Test toc treatment for fixed num:t."
  (org-test-with-exported-text
   'latex
   "#+TITLE: num: fix
#+OPTIONS: toc:t H:3 num:t

* Section

** Subsection 1
:PROPERTIES:
:UNNUMBERED: t
:END:
is suppressed
** Subsection 2
:PROPERTIES:
:UNNUMBERED: toc
:END:

** Subsection 3
:PROPERTIES:
:UNNUMBERED: toc
:ALT_TITLE: Alternative
:END:

* Section 2[fn::Test]
:PROPERTIES:
:ALT_TITLE: SECTION 2
:END:
"
   (goto-char (point-min))
   (should
    (search-forward "\\begin{document}

\\maketitle
\\tableofcontents

\\section{Section}
\\label{"))
   (should (search-forward "}

\\subsection*{Subsection 1}
\\label{"))
   (should (search-forward "}
is suppressed
\\subsection*{Subsection 2}
\\label{"))
  (should (search-forward "}
\\addcontentsline{toc}{subsection}{Subsection 2}
\\subsection*{Subsection 3}
\\label{"))
  (should (search-forward "}
\\addcontentsline{toc}{subsection}{Alternative}
\\section[SECTION 2]{Section 2\\footnote{Test}}
\\label{"))
  (should (search-forward "}
\\end{document}"))))

(ert-deftest test-ox-latex/new-toc-as-org ()
  "Test toc treatment with `org-latex-toc-include-unnumbered' set to t."
  (let ((org-latex-toc-include-unnumbered t))
    (org-test-with-exported-text 'latex
        "#+TITLE: num: fix
#+OPTIONS: toc:t H:3 num:nil

* Section

** Subsection 1

** Subsection 2
:PROPERTIES:
:UNNUMBERED: notoc
:END:
is suppressed

** Subsection 3
:PROPERTIES:
:ALT_TITLE: Alternative
:END:

* Section 2[fn::Test]
:PROPERTIES:
:ALT_TITLE: SECTION 2
:END:

* Section 3[fn::Test]
"
      (goto-char (point-min))
      (should (search-forward "\\begin{document}

\\maketitle
\\tableofcontents

\\section*{Section}
\\label{"))
      (should (search-forward "}
\\addcontentsline{toc}{section}{Section}

\\subsection*{Subsection 1}
\\label{"))
      (should (search-forward "}
\\addcontentsline{toc}{subsection}{Subsection 1}

\\subsection*{Subsection 2}
\\label{"))
      (should (search-forward "}
is suppressed
\\subsection*{Subsection 3}
\\label{"))
      (should (search-forward "}
\\addcontentsline{toc}{subsection}{Alternative}
\\section*{Section 2\\footnote{Test}}
\\label{"))
      (should (search-forward "}
\\addcontentsline{toc}{section}{SECTION 2}"))
      (should (search-forward "}
\\addcontentsline{toc}{section}{Section 3}")))))

(ert-deftest test-ox-latex/use-sans ()
  "Test `org-latex-use-sans' set to t."
  (let ((org-latex-use-sans t))
    (org-test-with-exported-text 'latex
        "#+TITLE: Test sans fonts
* Test

Fake test document
"
      (goto-char (point-min))
      (should (search-forward "\\renewcommand*\\familydefault{\\sfdefault}" nil t))
      (should (search-forward "\\begin{document}" nil t)))))

(ert-deftest test-ox-latex/use-sans-option ()
  "Test latex-use-sans in OPTIONS set to t."
  (org-test-with-exported-text 'latex
"#+TITLE: Test sans fonts
#+OPTIONS: latex-use-sans:t

* Test

Fake test document
"
      (goto-char (point-min))
      (should (search-forward "\\renewcommand*\\familydefault{\\sfdefault}" nil t))
      (should (search-forward "\\begin{document}" nil t))))

(ert-deftest test-ox-latex/use-sans-default ()
  "Test `org-latex-use-sans' default setting."
  (org-test-with-exported-text 'latex
                               "#+TITLE: Test no sans fonts
* Test

Fake test document
"
      (goto-char (point-min))
      (should-not (search-forward "\\renewcommand*\\familydefault{\\sfdefault}" nil t))
      (goto-char (point-min))
      (should (search-forward "\\begin{document}" nil t))))

(ert-deftest test-ox-latex/use-sans-override ()
  "Test `org-latex-use-sans' overriding variable."
  (let ((org-latex-use-sans t))
    (org-test-with-exported-text 'latex
                                 "#+TITLE: Test no sans fonts
#+OPTIONS: latex-use-sans:nil

* Test

Fake test document
"
      (goto-char (point-min))
      (should-not (search-forward "\\renewcommand*\\familydefault{\\sfdefault}" nil t))
      (goto-char (point-min))
      (should (search-forward "\\begin{document}" nil t)))))

(ert-deftest test-ox-latex/latex-class-pre ()
  "Test #+LATEX_CLASS_PRE."
  (org-test-with-exported-text 'latex
                               "#+LATEX_CLASS_PRE: \\PassOptionsToPackage{dvipsnames}{xcolor}
#+TITLE: Test prepending LaTeX before the preamble

* Test

Fake test document
"
      (goto-char (point-min))
      (should (search-forward "\\PassOptionsToPackage{dvipsnames}{xcolor}" nil t))
      ;; And after this
      (should (search-forward "\\documentclass" nil t))
      ;; And after this
      (should (search-forward "\\begin{document}" nil t))))

(ert-deftest test-ox-latex/latex-class-options1 ()
  "Test #+LATEX_CLASS_OPTIONS with square brackets."
  (org-test-with-exported-text 'latex
                               "#+LATEX_CLASS: article
#+LATEX_CLASS_OPTIONS: [a4paper,12pt]
#+TITLE: Confirm legagy class options

* Test

Fake test document
"
      (goto-char (point-min))
      (should (search-forward "\\documentclass[a4paper,12pt]{article}" nil t))))

(ert-deftest test-ox-latex/latex-class-options2 ()
  "Test #+LATEX_CLASS_OPTIONS without square brackets."
  (org-test-with-exported-text 'latex
                               "#+LATEX_CLASS: article
#+LATEX_CLASS_OPTIONS: a4paper,12pt
#+TITLE: Confirm class options without square brackets

* Test

Fake test document
"
      (goto-char (point-min))
      (should (search-forward "\\documentclass[a4paper,12pt]{article}" nil t))))

(ert-deftest test-ox-latex/latex-class-options3 ()
  "Don't overwrite class options in class template"
  (let ((org-latex-classes '(("my-letter" "\\documentclass[a4paper,12pt]{letter}"))))
      (org-test-with-exported-text
       'latex
       "#+LATEX_CLASS: my-letter

Fake test letter
"
      (goto-char (point-min))
      (should (search-forward "\\documentclass[a4paper,12pt]{letter}" nil t)))))


(ert-deftest test-ox-latex/latex-default-example-with-options ()
  "Test #+ATTR_LATEX: :options with custom environment."
  (let ((org-latex-default-example-environment "Verbatim"))
    (org-test-with-exported-text
     'latex
     "#+TITLE: Test adding options to EXAMPLE

* Test

#+ATTR_LATEX: :options [frame=double]
#+BEGIN_EXAMPLE
How do you do?
#+END_EXAMPLE
"
      (goto-char (point-min))
      (should (search-forward "\\begin{document}\n" nil t))
      (should (search-forward "\\begin{Verbatim}[frame=double]\n" nil t)))))
 (ert-deftest test-ox-latex/math-in-alt-title ()
  "Test math wrapping in ALT_TITLE properties."
  (org-test-with-exported-text
      'latex
      "* \\phi wraps
:PROPERTIES:
:ALT_TITLE: \\psi wraps too
:END:"
    (goto-char (point-min))
    (should (search-forward
             "\\section[\\(\\psi\\) wraps too]{\\(\\phi\\) wraps}"))))

(ert-deftest test-ox-latex/numeric-priority-headline ()
  "Test numeric priorities in headlines."
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t
* [#3] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#3}")))
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t
* [#42] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#42}")))
  ;; Test inline task (level >= org-inlinetask-min-level, default 15)
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t inline:t
***************** [#42] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#42}"))))

(ert-deftest test-ox-latex/alphabetical-priority-headline ()
  "Test numeric priorities in headlines."
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t
* [#C] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#C}")))
  ;; Test inline task (level >= org-inlinetask-min-level, default 15)
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t inline:t
***************** [#C] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#C}"))))

(ert-deftest test-ox-latex/change-descriptive-environment ()
  "Test numeric priorities in headlines."
  (let ((org-latex-descriptive-environment "itemize"))
    (org-test-with-exported-text
     'latex
   "* Acronyms
- SDN :: Software Defined Networks
"
   (goto-char (point-min))
   (should (search-forward "\\section{Acronyms}"))
   (should (search-forward "\\begin{itemize}
\\item[{SDN}] Software Defined Networks
\\end{itemize}
")))))

(ert-deftest test-ox-latex/subtree-export-with-language ()
  "Test export of subtrees with language detection."
  ;; We can't use `org-test-with-exported-text' because we need a subtree export
  (let ((export-buffer (generate-new-buffer "Org temporary export")))
    (org-test-with-temp-text
     "* subtree
:PROPERTIES:
:EXPORT_LATEX_HEADER: \\usepackage[utf8]{inputenc}
:EXPORT_LATEX_HEADER+: \\usepackage[french]{babel}
:END:

<point>"
     (org-export-to-buffer 'latex export-buffer nil t)
     (with-current-buffer export-buffer
       (goto-char (point-min))
       ;; This is somewhat redundant since the reported issue triggers an error on export
       (should (search-forward "\\usepackage[utf8]{inputenc} \\usepackage[french, english]{babel}")))
     (kill-buffer export-buffer))))

(ert-deftest test-ox-latex/pdf-metadata ()
  "Test that DocumentMetadata are inserted *before* LATEX_CLASS_PRE."
  (org-test-with-exported-text
   'latex
   "#+TITLE: PDF Metadata
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: pdflatex
#+LATEX_DOC_METADATA: tagging = on
#+LATEX_CLASS_PRE: \\PassOptionsToPackage{dvipsnames}{xcolor}
#+LATEX_CLASS: report
* Testing

Just to see that DocumentMetadata comes before PassOptions and documentclass
"
   ;; (message "pdf-metadata: %s" (buffer-string))
   (goto-char (point-min))
   (should (search-forward "\\DocumentMetadata{tagging = on}" nil t))
   (should (search-forward "\\PassOptionsToPackage{dvipsnames}{xcolor}" nil t))
   (should (re-search-forward "^\\\\documentclass\\[.+?]{report}" nil t))))

(ert-deftest test-ox-latex/lualatex-fontspec-recognised ()
  "Test that org-latex-fontspec-config is recognised for lualatex.
Since org-latex-fontspec-default-features is nil,
make sure that \\defaultfontfeatures{} is NOT included in the preamble.
"
  (let ((org-latex-fontspec-config
         '(("main" :font "FreeSerif")
           ("sans" :font "FreeSans"))))
   (org-test-with-exported-text
   'latex
   "#+TITLE: LuaLaTeX fonts
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_CLASS: report
* Testing

Just to see that I get the fonts Iwant...
"
   ;; (message "simple fontspec: %s" (buffer-string))
   (goto-char (point-min))
   (should (search-forward "\\usepackage{fontspec}" nil t))
   (save-excursion
     (should-not (search-forward "\\defaultfontfeatures{" nil t)))
   (should (search-forward "\\setmainfont{FreeSerif}" nil t))
   (should (search-forward "\\setsansfont{FreeSans}" nil t))
   (should (search-forward "\\begin{document}" nil t)))))

(ert-deftest test-ox-latex/lualatex-fontspec-default-features ()
  "Test that fontspec default features are generated
when fontspec is used in LaTeX document."
  (let ((org-latex-compiler "lualatex")
        (org-latex-fontspec-config '(("main" :font "FreeSerif")))
        (org-latex-fontspec-default-features "Scale=MatchLowercase"))
    (org-test-with-exported-text
     'latex
     "#+TITLE: fontspec
#+OPTIONS: toc:nil H:3 num:nil

* Heading

A random text without emojis.
"
     ;; (message "--> %s" (buffer-string))
     (goto-char (point-min))
     (should (search-forward "\\usepackage{fontspec}\n" nil t))
     (should (search-forward "\\setmainfont{FreeSerif}\n" nil t))
     (should (search-forward "\\defaultfontfeatures{Scale=MatchLowercase}\n" nil t))
     (should (search-forward "\\begin{document}" nil t)))))

(ert-deftest test-ox-latex/lualatex-fontspec-fallback ()
  "Test that org-latex-fontspec-config is recognised for lualatex.
Emojis are added."
  (let ((org-latex-fontspec-config
         '(("main" :font "FreeSerif"
            :fallback (("emoji" . "Noto Color Emoji:mode=harf")))
           ("sans" :font "FreeSans"))))
   (org-test-with-exported-text
   'latex
   "#+TITLE: LuaLaTeX fonts with emojis
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_CLASS: report
* Testing

Just to see that I get the fonts I want...

And my emojis too, 😀
"
   ;; (message "lualatex fallback: %s" (buffer-string))
   (goto-char (point-min))
   (should (search-forward "\\usepackage{fontspec}" nil t))
   (should (search-forward "\\directlua{" nil t))
   (should (search-forward "\\setmainfont{FreeSerif}[RawFeature={fallback=" nil t))
   (should (search-forward "\\setsansfont{FreeSans}" nil t)))))

(ert-deftest test-ox-latex/lualatex-fontspec-latex-header-not-lost ()
  "Test that org-latex-fontspec-config is recognised for lualatex.

It will be placed *before* LATEX_HEADER, so that any font configuration
there will prevail. Additionally test multiple languages in LANGUAGE.
This anticipates the changes for multi-lang."
  (let ((org-latex-fontspec-config
         '(("main" :font "FreeSerif")
           ("sans" :font "FreeSans")))
        (org-latex-packages-alist '(("AUTO" "babel"))))
   (org-test-with-exported-text
   'latex
   "#+TITLE: LuaLaTeX fonts
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_HEADER: \\setsansfont{TeX Gyre Heros}
#+LATEX_CLASS: report
* Testing

Just to see that I get the fonts Iwant...
"
   ;; (message "fontspec: latex-header\n%s" (buffer-string))
   (goto-char (point-min))
   (should (search-forward "\\usepackage{fontspec}" nil t))
   (should (search-forward "\\setmainfont{FreeSerif}" nil t))
   (should (search-forward "\\setsansfont{FreeSans}" nil t))
   ;; This comes from `org-latex-packages-alist'
   (should (search-forward "\\usepackage[british]{babel}" nil t))
   ;; And this from LATEX_HEADER
   (should (search-forward "\\setsansfont{TeX Gyre Heros}" nil t)))))

(ert-deftest test-ox-latex/sanity-check1 ()
  "Test that you can't select pdflatex and polyglossia."
  :expected-result :failed
  (let ((org-latex-packages-alist '(("AUTO" "polyglossia"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: The first sanity check
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: pdflatex

* Testing

What are you trying to do with pdflatex??
"
     (message "==> %s" (buffer-string)))))

(ert-deftest test-ox-latex/sanity-check2 ()
  "Test that you can't select babel and polyglossia at the same time."
  :expected-result :failed
  (let ((org-latex-default-packages-alist '(("AUTO" "babel")))
        (org-latex-packages-alist '(("AUTO" "polyglossia"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: The first sanity check
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex

* Testing

C'on, what d'you wanna do??
"
     (message "==> %s" (buffer-string)))))

(ert-deftest test-ox-latex/sanity-check3 ()
  "Test that you can't select babel and polyglossia at the same time."
  :expected-result :failed
  (let ((org-latex-packages-alist '(("AUTO" "polyglossia"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: The first sanity check
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_HEADER: \\usepackage[spanish]{babel}
* Testing

C'on, what d'you wanna do??
"
     (message "==> %s" (buffer-string)))))

(ert-deftest test-ox-latex/sanity-check4 ()
  "Test that you can't select babel as multi-lang and polyglossia in the packages."
  :expected-result :failed
  (let ((org-latex-packages-alist '(("AUTO" "polyglossia"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: The first sanity check
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_MULTI_LANG: babel
* Testing

C'on, what d'you wanna do??
"
     (message "==> %s" (buffer-string)))))

(ert-deftest test-ox-latex/multi-lang1 ()
  "Test that selecting babel as multi-lang, will replace fontspec."
  (org-test-with-exported-text
   'latex
   "#+TITLE: The first sanity check
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_MULTI_LANG: babel
* Testing

OK, so we are in business
"
     ;; (message "==> %s" (buffer-string))
     (goto-char (point-min))
     (save-excursion
       (should-not (re-search-forward "\\\\usepackage{fontspec}" nil t)))
     (should (search-forward "\\usepackage[bidi=basic]{babel}\n" nil t))
     (should (search-forward "\\babelprovide[import,main]{british}\n" nil t))
     (should (search-forward "\\babelprovide[import]{spanish}\n" nil t))))

(ert-deftest test-ox-latex/multi-lang2 ()
  "Test that selecting babel as multi-lang, the fontspec configuration
is appended to the babel configuration."
  (let ((org-latex-fontspec-config '(("main" :font "FreeSerif"))))
   (org-test-with-exported-text
   'latex
   "#+TITLE: The first sanity check
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_MULTI_LANG: babel
* Testing

OK, so we are in business
"
     ;; (message "==> %s" (buffer-string))
     (goto-char (point-min))
     (save-excursion
       (should-not (re-search-forward "\\\\usepackage{fontspec}" nil t)))
     (should (search-forward "\\usepackage[bidi=basic]{babel}\n" nil t))
     (should (search-forward "\\babelprovide[import,main]{british}\n" nil t))
     (should (search-forward "\\babelprovide[import]{spanish}\n" nil t))
     (should (search-forward "\\RequirePackage{fontspec}\n" nil t))
     (should (search-forward "\\setmainfont{FreeSerif}\n" nil t)))))

(ert-deftest test-ox-latex/multi-lang3 ()
  "Test that selecting polyglossia as multi-lang, the fontspec configuration
is appended to the polyglossia configuration."
  (let ((org-latex-fontspec-config '(("main" :font "FreeSerif"))))
   (org-test-with-exported-text
   'latex
   "#+TITLE: The first sanity check
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_MULTI_LANG: polyglossia

* Testing

OK, so we are in business
"
   ;; (message "--> %s" (buffer-string))
   (goto-char (point-min))
   (save-excursion
     (should-not (re-search-forward "\\\\usepackage{fontspec}" nil t)))
   (should (search-forward "\\usepackage{polyglossia}\n" nil t))
   (should (search-forward "\\setmainlanguage[variant=uk]{english}\n" nil t))
   (should (search-forward "\\setotherlanguage{spanish}\n" nil t))
   (should (search-forward "\\RequirePackage{fontspec}\n" nil t))
   (should (search-forward "\\setmainfont{FreeSerif}\n" nil t)))))

(ert-deftest test-ox-latex/multi-lang4 ()
  "Test SELECT_LANG"
  (org-test-with-exported-text
   'latex
   "#+TITLE: Playing with languages
#+LANGUAGE: en-gb es
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_MULTI_LANG: babel

* Testing

OK, so we are in business.

#+select_lang: es

Y esto va en español.

#+select_lang: en-gb

Back to British English.
"
   ;; (message "--> %s" (buffer-string))
   (goto-char (point-min))
   (save-excursion
     (should-not (re-search-forward "\\\\usepackage{fontspec}" nil t)))
   (should (re-search-forward "\\\\usepackage\\[.+?]{babel}\n" nil t))
   (should (search-forward "\\selectlanguage{spanish}\n" nil t))
   (should (search-forward "\\selectlanguage{british}\n" nil t))))

(ert-deftest test-ox-latex/polyglossia-standalone-fonts ()
  (let ((org-latex-fontspec-config nil)
        (org-latex-polyglossia-font-config
         '(;; The next one should not appear
           ("es" :font "FreeSerif" :variant "main")
           ("es" :font "FreeSans" :variant "sans")
           ("es" :font "FreeMono" :variant "mono" :props "Scale=MatchLowercase")
           ("de" :font "FreeMono" :variant "mono" :props "Scale=MatchLowercase")
           ("en-gb" :variant "rm" :font "DejaVu Serif")
           ("en-gb" :variant "sf" :font "DejaVu Sans")
           ("en-gb" :variant "tt" :font "DejaVu Sans Mono" :props "Scale=MatchLowercase"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: pure polyglossia
#+LANGUAGE: es en-gb
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_MULTI_LANG: polyglossia

#+select-lang: en-gb
* Testing

Polyglossia managing fonts. Document ready for multi-lang
"
     ;; (message "babel: %s" (buffer-string))
     (goto-char (point-min))
     ;; Language part
     (should (search-forward "\\usepackage{polyglossia}" nil t))
     (should (search-forward "\\setmainlanguage{spanish}" nil t))
     (should (search-forward "\\setotherlanguage[variant=uk]{english}" nil t))
     ;; Font part, "rm" is suppressed, variant is translated, props are added
     (should (search-forward "\\newfontfamily\\spanishfont{FreeSerif}" nil t))
     (should (search-forward "\\newfontfamily\\spanishfontsf{FreeSans}" nil t))
     (should (search-forward "\\newfontfamily\\spanishfonttt[Scale=MatchLowercase]{FreeMono}" nil t))
     ;; Font part, "rm" is suppressed, short variant is kept, props are added
     (should (search-forward "\\newfontfamily\\englishfont{DejaVu Serif}" nil t))
     (should (search-forward "\\newfontfamily\\englishfontsf{DejaVu Sans}" nil t))
     (should (search-forward "\\newfontfamily\\englishfonttt[Scale=MatchLowercase]{DejaVu Sans Mono}" nil t))
     (goto-char (point-min))
     ;; Not included in LANGUAGES
     (should-not (search-forward "\\newfontfamily\\germanfon" nil t)))))

  (ert-deftest test-ox-latex/babel-standalone-fonts ()
  "Test that babel can manage fonts standalone."
  (let ((org-latex-fontspec-config nil)
        (org-latex-babel-font-config '((nil :variant "main" :font "FreeSerif")
                                       (nil :variant "sans" :font "FreeSans")
                                       ;; The next one should not appear
                                       ("es" :variant "sans" :font "OpenSans")
                                       ("en-gb" :variant "main" :font "DejaVu Serif")
                                       ("en-gb" :variant "sans" :font "DejaVu Sans"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: pure Babel
#+LANGUAGE: es en-gb
#+OPTIONS: toc:nil H:3 num:nil
#+LATEX_COMPILER: lualatex
#+LATEX_MULTI_LANG: babel
* Testing

Babel managing fonts. Document ready for spanish text.
"
     ;; (message "babel: %s" (buffer-string))
     (goto-char (point-min))
     (should (re-search-forward "\\usepackage\\[bidi=basic]{babel}" nil t))
     (should (search-forward "\\babelprovide[import,main]{spanish}\n" nil t))
     (should (search-forward "\\babelprovide[import]{british}\n" nil t))
     (should (search-forward "\\babelfont{rm}{FreeSerif}" nil t))
     (should (search-forward "\\babelfont{sf}{FreeSans}" nil t))
     (should (search-forward "\\babelfont[british]{rm}{DejaVu Serif}" nil t))
     (should (search-forward "\\babelfont[british]{sf}{DejaVu Sans}" nil t))
     (goto-char (point-min))
     ;; OpenSans would be for Spanish as secondary language
     (should-not (search-forward "\\babelfont[spanish]{sf}{OpenSans}" nil t)))))

(ert-deftest test-ox-latex/lualatex-babel-cjk ()
  "Test that Chinese text are handled correctly.
In this test we default to Fandol font for Chinese."
  (let ((org-latex-compiler "lualatex"))
    (org-test-with-exported-text
     'latex
     "#+TITLE: CJK
#+OPTIONS: toc:nil H:3 num:nil
#+LANGUAGE: zh
#+LATEX_MULTI_LANG: babel

* 标题

正文。
"
     (message "== zh ==>\n%s" (buffer-string))
     (goto-char (point-min))
     (save-excursion
       (should (search-forward "\\usepackage{indentfirst}")))
     (save-excursion
       (should (search-forward "\\catcode`\\^^^^200b=\\active\\let^^^^200b\\relax")))
     (save-excursion
       (should (search-forward "\\parindent=2\\zw")))
     (save-excursion
       (should (search-forward "\\linespread{1.333}")))
     ;; Not wrapped in `save-excursion' since they must follow this specific sequence
     (should (search-forward "\\setCJKmainfont{FandolSong}"))
     (should (search-forward "\\setCJKsansfont{FandolHei}"))
     (should (search-forward "\\def\\ltj@stdyokojfm{quanjiao}"))
     (should (search-forward "\\usepackage{luatexja}"))
     (should (search-forward "\\babelprovide[main,import]{chinese}")))))


(provide 'test-ox-latex)
;;; test-ox-latex.el ends here
