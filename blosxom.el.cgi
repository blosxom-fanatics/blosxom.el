#!/bin/sh
#@63
LANG=C exec /usr/bin/emacs -Q --batch --no-unibyte --kill -l $0
; set LANG for format-time-string
; vim:expandtab:
; I'm a vimmer.
; License:: Creative Commons by

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'cl)

(setf output-buffer (get-buffer-create "*BlosxomElisp"))
(set-buffer output-buffer)
(erase-buffer)

; http://subtech.g.hatena.ne.jp/antipop/20071023/1193150099
(defun apply-template (place flavour)
  (setf file (concat place flavour))
  (save-current-buffer
    (let ((buffer (get-buffer-create "*TemplateProcessing*")))
      (set-buffer buffer)
      (erase-buffer)
      (insert-file-contents file)
      (while (re-search-forward "\\$\\([0-9a-zA-Z_-]+\\)" nil t)
             (replace-match (symbol-value (intern (match-string 1))) nil nil))
      (prog1 (buffer-string)
        (kill-buffer nil)))))

(defun replace (str match-str replace-str)
  (save-current-buffer
    (let ((buffer (get-buffer-create "*ReplaceString*")))
      (set-buffer buffer)
      (erase-buffer)
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward match-str nil t)
             (replace-match replace-str nil nil))
      (prog1 (buffer-string)
        (kill-buffer nil)))))

(defun split-entry-body (file)
  (save-current-buffer
    (let ((buffer (get-buffer-create "*TempEntry*")))
      (set-buffer buffer)
      (erase-buffer)
      (insert-file-contents file)
      (goto-char (point-min))
      (kill-line)
      (setf title (car kill-ring))
      (setf body  (buffer-string))
      (prog1 (values title body)
        (setf kill-ring '())
        (kill-buffer nil)))))

(defun list-entries (dir base)
  (setf base (expand-file-name base))
  (let ((ret '()))
    (loop for f in (directory-files dir t) do
          (let* ((attr (file-attributes f))
                 (dir?  (eq (car attr) t))
                 (file? (null (car attr)))
                 (mtime (nth 5 attr)))
            ;(print (list f dir? file?))
            (if (null (string-match "\\(\\.\\|\\.\\.\\)$" f))
              (progn
                (if dir?  (setf ret (append ret (list-entries f base))))
                (if file? (setf ret (append ret (list
                                                  `((path . ,f)
                                                    (name . ,(replace f (concat (regexp-quote base) "\\|\\..+$") ""))
                                                    (time . ,mtime))
                                                  )))))
              )
            )
          )
    ret)
  )

(defun matches (regexp str)
  (if (string-match regexp str)
    (progn
      (setf m (match-data))
      (loop for x below 10
            if (match-beginning x)
            collect
            (substring str (match-beginning x) (match-end x))
            ))
    nil
    ))

(setf title "blosxom.el !")
(setf author "Joe")

(setf servername (concat "http://" (or (getenv "SERVER_NAME") "")))
(setf home (or (getenv "SCRIPT_NAME") ""))
(setf pathinfo (or (getenv "PATH_INFO") ""))
(setf pathname (replace pathinfo "\\(index\\)?\\..+$" ""))
(setf version (emacs-version))

(setf flavour (or (nth 1 (matches "\\(\\..+\\)$" pathinfo)) ".html"))
(setf splitted-pathinfo (matches "^/\\([0-9]+\\)\\(/[0-9][0-9]\\)?\\(/[0-9][0-9]\\)?" pathinfo))
;(print system-configuration)
;(print system-name)

;(print (getenv "PATH_INFO"))
;(print invocation-name)
;(print process-environment)


(setf entries (list-entries "data" "data"))

; sort by mtime
(sort entries '(lambda (a b)
                 (setf timea (cdr (assoc 'time a)))
                 (setf timeb (cdr (assoc 'time b)))

                 (if
                   (= (car  timea) (car  timeb))
                   (< (cadr timea) (cadr timeb))
                   (< (car  timea) (car  timeb))
                   )))

; filter
(setf entries (loop for e in entries
                    if (progn
                         (setf name (cdr (assoc 'name e)))
                         (setf time (cdr (assoc 'time e)))
                         ; (string= "2007" (format-time-string "%Y" time))
                         (cond ((nth 3 splitted-pathinfo)
                                (string= (format-time-string "%Y/%m/%d" time) (apply 'concat (cdr splitted-pathinfo)))
                                )
                               ((nth 2 splitted-pathinfo)
                                (string= (format-time-string "%Y/%m" time) (apply 'concat (cdr splitted-pathinfo)))
                                )
                               ((nth 1 splitted-pathinfo)
                                (string= (format-time-string "%Y" time) (nth 1 splitted-pathinfo))
                                )
                               (t
                                 (string-match (concat "^" pathname) name)
                                 ))
                         )
                    collect e))

(setf entries (last entries 7))
(setf entries (nreverse entries))

(setf lastupdate (format-time-string "%Y-%m-%dT%H:%M:%SZ" (cdr (assoc 'time (car entries)))))

(insert (concat "Content-Type: " (apply-template "content_type" flavour) "\n"))
(insert (apply-template "head" flavour))

(loop for e in entries do
      (multiple-value-bind (title body) (split-entry-body (cdr (assoc 'path e))))
      (setf name   (cdr (assoc 'name e)))
      (setf path   (cdr (assoc 'name e)))
      (setf time   (cdr (assoc 'time e)))
      (setf w3cdtf (format-time-string "%Y-%m-%dT%H:%M:%SZ" time))
      (setf yr     (format-time-string "%Y"  time))
      (setf mo     (format-time-string "%b"  time))
      (setf mo_num (format-time-string "%m"  time))
      (setf da     (format-time-string "%d"  time))
      (setf dw     (format-time-string "%a"  time))
      (setf hr     (format-time-string "%H"  time))
      (setf min    (format-time-string "%M"  time))
      (setf hr12   (format-time-string "%I"  time))
      (setf ampm   (format-time-string "%p"  time))
      (setf ti     (format-time-string "%X"  time))
      (insert (apply-template "story" flavour))
      )
;(print entries)

(insert (apply-template "foot" flavour))

(princ (buffer-string))
