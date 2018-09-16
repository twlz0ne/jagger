[![Build Status](https://travis-ci.com/twlz0ne/jagger.svg?branch=master)](https://travis-ci.com/twlz0ne/jagger)

## Jagger

Move/swap things (including regions, sexps, lines, words) more conveniently in Emacs.

## Installation

Copy file `jagger*.el` to directory `~/.emacs.d/site-lisp/jagger/`, for example, and add this to your .emacs

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/jagger"))
(require 'jagger)
```

## Usage

### Swap two regions

- Select region1
- `jagger-swap-regions-mark-region` set mark and highlight it with overlay
- Select region2
- `jagger-swap-regions` apply swap, clean overlay

A more natural way is to mark regions automatically when yanking. There is an example for evil:

```elisp
(defun evil-yank@set-mark (begin end &rest argv)
  (jagger-swap-regions-mark-region-1 begin end))

(advice-add 'evil-yank :before 'evil-yank@set-mark)
(define-key evil-visual-state-map (kbd "M-p") 'jagger-swap-regions)
```

Then you can use it like this:

- `y` yank region1 as usual, there is no highlight
- Select region2
- `M-p` to apply swap

### Swap things (sexps/words/lines) around point

```
(foo| bar) -> (|bar foo)  ;; jagger-swap-sexps
```

### Swap things (sexps/words/lines) surround region

```
(foo [(qux "quux")] bar) -> (bar (qux "quux")| foo) ;; jagger-swap-sexps
```

### Move things at point backward/forward

```
(|foo bar) -> (bar |foo) ;; jagger-move-sexp-forward
```

### Move sexp backward/forward

```
([foo] (qux "quux") bar) -> ((qux "quux") [foo] bar) ;; jagger-move-sexp-forward
([foo (qux "quux")] bar) -> (bar [foo (qux "quux")])
```

### Move line up/down

```
|foo  =>  bar   ;; jagger-move-line-down
bar       |foo
```

### Move region up/down

```
[foo      qux   ;; jagger-move-line-down
bar]  =>  [foo
qux       bar]
```

### Sort sexps at point in temp buffer

Original bffer:

```
(foo (qux "quux") bar)
```

`M-x jagger-sort-sexps-at-point-in-temp-buffer` switch to temp buffer in other window:

```
foo               (qux "quux")   ;; M-j/k to move one line at point \
(qux "quux")  =>  bar            ;; or multiple lines in region \down/down
bar               foo
```

`C-c C-c` commit changes to original buffer:

```
((qux "quux") bar foo)
```

or `C-c C-k` discard changes.
