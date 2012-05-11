;;; keymap-utils.el --- keymap pretty-printing

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: WIP
;; Homepage: https://github.com/tarsius/keymap-utils
;; Keywords: convenience, extensions

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Keymap pretty-printing.  This is very much work in progress.

;;; Code:

(require 'cl)
(require 'keymap-utils)

(defvar kmu-pp-modifier-order nil)
(defvar kmu-pp-event-order nil)
(defvar kmu-pp-event-no-indent nil)
(defvar kmu-pp-command-indent 24)


(defmacro kmu-with-mapvar-file (mapvar file &rest body)
  (declare (indent 2))
  `(with-temp-file
      (expand-file-name (or ,file (concat (symbol-name ,mapvar) ".el"))
			kmu-mapvar-directory)
    (let ((standard-output (current-buffer)))
      ,@body)))

(defun kmu-pp-mapvar (mapvar)
  (let* ((keymap (if (symbolp mapvar)
		     (symbol-value mapvar)
		   (symbol-function (caddr mapvar))))
	 (parent (kmu-keymap-parent keymap))
	 (fullp  (kmu-full-keymap-p keymap))
	 (doc (documentation-property
	       (if (symbolp mapvar) mapvar (caddr mapvar))
	       'variable-documentation t))
	 (case-fold-search nil)
	 (indent (or kmu-pp-command-indent 0))
	 bindings prefix-commands prefix-keymap)
    (princ "(kmu-define-keys ")
    (prin1 mapvar)
    (princ "\n")
    (when doc
      (princ "  ;; ")
      (princ (substring doc 0 (string-match "\n" doc)))
      (princ "\n"))
    (when parent
      (princ "  :parent")
      (indent-to (or kmu-pp-command-indent 0) 1)
      (prin1 parent)
      (princ "\n"))
    (when fullp
      (princ "  :full-keymap")
      (indent-to (or kmu-pp-command-indent 0) 1)
      (prin1 fullp)
      (princ "\n"))
    (kmu-map-keymap (lambda (key def)
		      (push (list key def) bindings))
		    keymap t)
    (dolist (bind (kmu--sort-bindings bindings))
      (destructuring-bind (key def) bind
	(cond
	 ;; ((setq prefix-keymap (kmu-prefix-command-p def))
	 ;;  ;; TODO don't save prefix-cmds with seperate mapvar?
	 ;;  ;; they are also saved in their own files
	 ;;  ;; (unless (and (listp mapvar) (nth 3 mapvar))
	 ;;  (push (append (list (if (listp mapvar)
	 ;; 			  (or (nth 3 mapvar)
	 ;; 			      (nth 2 mapvar))
	 ;; 			mapvar)
	 ;; 		      key def)
	 ;; 		(when (symbolp prefix-keymap)
	 ;; 		  (list prefix-keymap)))
	 ;; 	prefix-commands))
	 ((consp def)
	  (princ "  ;; ")
	  (prin1 key)
	  (indent-to indent 1)
	  (princ "anonymous menu\n"))
	 (t
	  (if (or (consp key)
		  (eq kmu-pp-event-no-indent t)
		  (member (car (split-string key " "))
			  kmu-pp-event-no-indent))
	      (princ "  ")
	    (string-match "^\\(<\\)?\\([CMSHs]-\\)?\\([CMSHs]-\\)?" key)
	    (unless (match-string 1 key)
	      (princ " "))
	    (princ (cond ((match-string 3 key) "  ")
			 ((match-string 2 key) "    ")
			 (t                    "      "))))
	  (prin1 key)
	  (indent-to indent 1)
	  (prin1 def)
	  (princ "\n")))))
    (princ "  )\n")
    (dolist (pc prefix-commands)
      (princ "\n")
      (kmu-pp-mapvar pc))))

(defun kmu--sort-bindings (bindings)
  (mapcar
   'car
   (sort
    (mapcar
     (lambda (binding)
       (destructuring-bind (key def) binding
	 (let (r k m)
	   (if (consp key)
	       (list binding)
	     (dolist (p (split-string key " "))
	       (string-match
		"^\\(<\\)?\\(\\(?:[CMSHs]-\\)+\\)?\\(.+?\\)\\(>\\)?$" p)
	       (push (list (setq k (concat (match-string 1 p)
					   (match-string 3 p)
					   (match-string 4 p)))
			   (length (if (save-match-data
					 (string-match "^[A-Z]$" k))
				       (member (downcase k) kmu-pp-event-order)
				     (member k kmu-pp-event-order)))
			   (setq m (match-string 2 p))
			   (length (member m kmu-pp-modifier-order)))
		     r)))
	   (cons binding (nreverse r)))))
     bindings)
    'kmu--sort-bindings-test)))

(defun* kmu--sort-bindings-test (a b &optional (i 1))
  (cond ((consp (caar a)) t)
	((consp (caar b)) nil)
	(t (destructuring-bind
	       (ak ako am amo
		bk bko bm bmo)
	       (append (nth i a)
		       (nth i b))
	     (cond ((<       ako bko)    nil)
		   ((<       bko ako)      t)
		   ((string< ak  bk)     nil)
		   ((string< bk  ak)       t)
		   ((<       amo bmo)    nil)
		   ((<       bmo amo)      t)
		   ((not (nth (1+ i) b)) nil)
		   ((not (nth (1+ i) a))   t)
		   (t
		    (kmu--sort-bindings-test a b (1+ i))))))))

(provide 'keymap-pp)
;;; keymap-pp.el ends here
