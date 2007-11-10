#!/bin/sh
#@63
LANG=C exec /usr/bin/emacs -Q --batch --no-unibyte --kill -l $0


(require 'cl)

; http://subtech.g.hatena.ne.jp/antipop/20071023/1193150099
(defun apply-template (file)
  (save-current-buffer
    (let ((buffer (get-buffer-create "*TemplateProcessing*")))
      (set-buffer buffer)
      (erase-buffer)
      (insert-file-contents file)
      (while (re-search-forward "\\$\\([a-zA-Z]+\\)" nil t)
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

(setf title "blosxom.el !")
(setf author "Joe")

(setf home (or (getenv "SCRIPT_NAME") ""))
(setf version (emacs-version))
;(print system-configuration)
;(print system-name)

;(print (getenv "PATH_INFO"))
;(print invocation-name)
;(print process-environment)


(princ (concat "Content-Type: " (apply-template "content_type.html") "\n"))
(princ (apply-template "head.html"))


(setf entries (list-entries "data" "data"))
(sort entries '(lambda (a b)
                 (setf timea (cdr (assoc 'time a)))
                 (setf timeb (cdr (assoc 'time b)))

                 ; どうすればいいだろう……
                 (<  (+ (* 65535 (car timea)) (cadr timea))
                     (+ (* 65535 (car timeb)) (cadr timeb))
                     )
                 ))
(setf entries (last entries 7))
(setf entries (nreverse entries))
(loop for e in entries do
      (multiple-value-bind (title body) (split-entry-body (cdr (assoc 'path e))))
      (setf name (cdr (assoc 'name e)))
      (setf path (cdr (assoc 'name e)))
      (setf time (cdr (assoc 'time e)))
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
      (princ (apply-template "story.html"))
      )
;(print entries)

(princ (apply-template "foot.html"))


