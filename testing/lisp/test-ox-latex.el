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
  "Test toc treatment for fixed num:t"
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
  "test toc treatment with `org-latex-toc-include-unnumbered' set to `t'"
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

(ert-deftest test-ox-latex/lualatex-fontspec-plain ()
  "Test that neither defaultfontfeatures nor directlua block is not created
when no fallbacks in fontspec configuration"
  (let ((org-latex-compiler "lualatex")
        (org-latex-multi-lang "fontspec")
        (org-latex-fontspec-config '(("main" :font "FreeSans"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: fontspec
#+OPTIONS: toc:nil H:3 num:nil

* Heading

A random text.
"
     ;; (message "--> %s" (buffer-string))
     (goto-char (point-min))
     (should-not (search-forward "\\defaultfontfeatures{" nil t))
     (goto-char (point-min))
     (should-not (search-forward "\\directlua" nil t))
     (goto-char (point-min))
     (should (search-forward "\\setmainfont{FreeSans}" nil t)))))

(ert-deftest test-ox-latex/lualatex-fontspec-features ()
  "Test that font features are generated when declared."
  (let ((org-latex-compiler "lualatex")
        (org-latex-multi-lang "fontspec")
        (org-latex-fontspec-config '(("main" :font "FreeSans")
                                     ("mono" :font "FreeMono" :props "Scale=MatchLowercase"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: fontspec-features
#+OPTIONS: toc:nil H:3 num:nil

* Heading

A random text.
"
     (goto-char (point-min))
     (should (search-forward "\\setmainfont{FreeSans}" nil t))
     (goto-char (point-min))
     (should (search-forward "\\setmonofont{FreeMono}[Scale=MatchLowercase]" nil t)))))

(ert-deftest test-ox-latex/lualatex-fontspec-default-features ()
  "Test that defaultfontfeatures is generated
when org-latex-fontspec-default-features are defined."
  (let ((org-latex-compiler "lualatex")
        (org-latex-multi-lang "fontspec")
        (org-latex-fontspec-default-features "Numbers=OldStyle")
        (org-latex-fontspec-config '(("main" :font "FreeSans"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: fontspec-default-features
#+OPTIONS: toc:nil H:3 num:nil

* Heading

A random text.
"
     (goto-char (point-min))
     (should (search-forward "\\defaultfontfeatures{Numbers=OldStyle}" nil t))
     (should (search-forward "\\setmainfont{FreeSans}" nil t)))))

(ert-deftest test-ox-latex/lualatex-fontspec-directlua ()
  "Test that directlua block is created"
  (let ((org-latex-compiler "lualatex")
        (org-latex-multi-lang "fontspec")
        (org-latex-fontspec-config '(("main"
                                      :font "FreeSans"
                                      :fallback (("emoji" . "Noto Color Emoji:mode=harf"))))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: fontspec
#+OPTIONS: toc:nil H:3 num:nil

* Heading

A random text with emoji: 👍
"
     ;; (message "--> %s" (buffer-string))
     (goto-char (point-min))
     (should (search-forward "\\directlua" nil t))
     (should (search-forward "Noto Color Emoji" nil t))
     (should (search-forward "\\setmainfont{FreeSans}[RawFeature={fallback=fallback_main}]" nil t)))))

(ert-deftest test-ox-latex/lualatex-fontspec-fallback-plist ()
  "Test that directlua block is created"
  (let ((org-latex-compiler "lualatex")
        (org-latex-multi-lang "fontspec")
        (org-latex-fontspec-config '(("main"
                                      :font "FreeSans"
                                      :fallback (("emoji" :font "Noto Color Emoji:mode=harf"))))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: fontspec
#+OPTIONS: toc:nil H:3 num:nil

* Heading

A random text with emoji: 👍
"
     ;; (message "--> %s" (buffer-string))
     (goto-char (point-min))
     (should (search-forward "\\directlua" nil t))
     (should (search-forward "Noto Color Emoji" nil t))
     (should (search-forward "\\setmainfont{FreeSans}[RawFeature={fallback=fallback_main}]" nil t)))))

(ert-deftest test-ox-latex/lualatex-fontspec-noemoji ()
  "Test that directlua block is not created because it is not needed
no emojis detected"
  (let ((org-latex-compiler "lualatex")
        (org-latex-multi-lang "fontspec")
        (org-latex-fontspec-config '(("main"
                                      :font "FreeSans"
                                      :fallback (("emoji" . "Noto Color Emoji:mode=harf"))))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: fontspec
#+OPTIONS: toc:nil H:3 num:nil

* Heading

A random text without emojis.
"
     ;; (message "--> %s" (buffer-string))
     (goto-char (point-min))
     (should-not (search-forward "\\directlua" nil t))
     (goto-char (point-min))
     (should-not (search-forward "Noto Color Emoji" nil t))
     (goto-char (point-min))
     (should (search-forward "\\setmainfont{FreeSans}" nil t)))))

(ert-deftest test-ox-latex/lualatex-unicode-math-config ()
  "Test that the unicode-math package can be passed options using
org-latex-unicode-math-options."
  (let ((org-latex-compiler "lualatex")
        (org-latex-multi-lang "fontspec")
        (org-latex-unicode-math-options "math-style=upright")
        (org-latex-fontspec-config '(("main"
                                      :font "FreeSans"))))
    ;; (message "--> %s" (buffer-string))
    (org-test-with-exported-text
     'latex
     "#+TITLE: fontspec
#+OPTIONS: toc:nil H:3 num:nil

* Heading

A random text without emojis.
"
     ;; (message "--> %s" (buffer-string))
     (goto-char (point-min))
     (should (search-forward "\\usepackage[math-style=upright]{unicode-math}" nil t)))))

(ert-deftest test-ox-latex/lualatex-babel-langs ()
  "Test that babel is handled correctly.
In this test we set org-latex-multi-lang to \"babel\"
in the document header and test that the default can be overwritten.
We are NOT including anything in org-latex-babel-provides.
Test that the default provides are generated."
  (let ((org-latex-compiler "lualatex")
        (org-latex-multi-lang "fontspec"))
    (org-test-with-exported-text
     'latex
     "#+TITLE: fontspec
#+OPTIONS: toc:nil H:3 num:nil
#+LANGUAGE: de el
#+LATEX_MULTI_LANG: babel

* Einleitung

Irgend etwas.
"
     (goto-char (point-min))
     (should (re-search-forward "\\\\usepackage\\[[^]]+\\]{babel}" nil t))
     (save-excursion
       (should  (search-forward "\\babelprovide[main,import]{german}" nil t)))
     (save-excursion
       (should (search-forward "\\babelprovide[import]{greek}" nil t))))))

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
     (goto-char (point-min))
     (save-excursion
       (should (search-forward "\\usepackage{indentfirst}")))
     (save-excursion
       (should (search-forward "\\catcode`\\^^^^200b=\\active\\let^^^^200b\\relax")))
     (save-excursion
       (should (search-forward "\\parindent=2em")))
     (save-excursion
       (should (search-forward "\\linespread{1.333}")))
     ;; Not wrapped in `save-excursion' since they must follow this specific sequence
     (should (search-forward "\\setCJKmainfont{FandolSong}"))
     (should (search-forward "\\setCJKsansfont{FandolHei}"))
     (should (search-forward "\\def\\ltj@stdyokojfm{quanjiao}"))
     (should (search-forward "\\usepackage{luatexja}"))
     (should (search-forward "\\babelprovide[main,import]{chinese}")))))

(ert-deftest test-ox-latex/lualatex-babel-fonts ()
  "Test that babelfont is handled correctly.
The font specs for the main (nil) and the secondary language (el) are generated correctly.
The font spec for a secondary language that is being used as primary is ignored."
  (let ((org-latex-compiler "lualatex")
        (org-latex-babel-font-config '((nil :variant "rm" :font "DejaVu Serif")
                                       ;; This one should be ignored because "de" is the main language
                                       ("de" :variant "rm" :font "CMU Serif")
                                       ("el" :variant "rm" :font "FreeSerif")
                                       ("el" :variant "sf" :font "FreeSans" :props "Scale=MatchLowercase")))
        (org-latex-babel-provides-alist '(("de" :provide "onchar=ids fonts")
                                          ("el" :provide "onchar=ids fonts"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: babel provides
#+OPTIONS: toc:nil H:3 num:nil
#+LANGUAGE: de el
#+LATEX_MULTI_LANG: babel

* Einleitung

Irgend etwas.
"
     (goto-char (point-min))
     (should (re-search-forward "\\\\usepackage\\[[^]]+\\]{babel}" nil t))
     (save-excursion
       (should  (search-forward "\\babelfont{rm}{DejaVu Serif}" nil t)))
     (save-excursion
       (should (search-forward "\\babelfont[greek]{rm}{FreeSerif}" nil t)))
     (save-excursion
       (should (search-forward "\\babelfont[greek]{sf}[Scale=MatchLowercase]{FreeSans}" nil t))))))

(ert-deftest test-ox-latex/lualatex-babel-provides ()
  "Test that babelprovides is handled correctly.
In this test we set org-latex-multi-lang to \"babel\"
and add some babelprovides. Test that the main language is \"import,main\"
despite settings and that the settings for the secondary language are taken.
We are NOT including anything in org-latex-babel-provides.
Test that the default provides are generated."
  (let ((org-latex-compiler "lualatex")
        (org-latex-babel-provides-alist '(("de" :provide "onchar=ids fonts")
                                          ("el" :provide "onchar=ids fonts"))))
    (org-test-with-exported-text
     'latex
     "#+TITLE: babel provides
#+OPTIONS: toc:nil H:3 num:nil
#+LANGUAGE: de el
#+LATEX_MULTI_LANG: babel

* Einleitung

Irgend etwas.
"
     (goto-char (point-min))
     (should (re-search-forward "\\\\usepackage\\[[^]]+\\]{babel}" nil t))
     (save-excursion
       (should  (search-forward "\\babelprovide[main,import]{german}" nil t)))
     (save-excursion
       (should (search-forward "\\babelprovide[onchar=ids fonts]{greek}" nil t))))))

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
   (should (search-forward "\\framebox{\\#42}"))))

(ert-deftest test-ox-latex/alphabetical-priority-headline ()
  "Test numeric priorities in headlines."
  (org-test-with-exported-text
   'latex
   "#+OPTIONS: pri:t
* [#C] Test
"
   (goto-char (point-min))
   (should (search-forward "\\framebox{\\#C}"))))

(provide 'test-ox-latex)
;;; test-ox-latex.el ends here
