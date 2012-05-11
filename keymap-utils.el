;;; keymap-utils.el --- keymap utilities

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: 0.4.0-git
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

;; This library provides additional functions to work with keymaps.
;;
;; * keymap predicates (e.g. `kmu-keymap-variable-p')
;; * key lookup (e.g. `kmu-lookup-parent-key')
;;
;; Versions prior to 0.4.0 also contained the following.
;;
;; * replace keymap in place (e.g. `kmu-set-mapvar')
;; * keymap pretty printing (`kmu-pp-keymap'; VERY experimental)

;;; Code:

(require 'cl)

;;; Predicates.

(defun kmu-keymap-variable-p (object)
  "Return t if OBJECT is a symbol whose variable definition is a keymap."
  (and (symbolp object)
       (boundp  object)
       (keymapp (symbol-value object))))

(defun kmu-keymap-list-p (object)
  "Return t if OBJECT is a list whose first element is the symbol `keymap'."
  (and (listp   object)
       (keymapp object)))

(defun kmu-prefix-command-p (object &optional boundp)
  "Return t if OBJECT is a symbol whose function definition is a keymap.
The value returned is the keymap stored as OBJECTS variable definition or
else the mapvar which holds the keymap."
  (and (symbolp object)
       (fboundp object)
       (keymapp (symbol-function object))
       (if (and (boundp  object)
		(keymapp (symbol-value object)))
	   (symbol-value object)
	 (kmu-keymap-variable (symbol-function object)))))

(defun kmu-full-keymap-p (object)
  "Return t if OBJECT is a full keymap.
A full keymap is a keymap whose second element is a char-table."
  (if (kmu-prefix-command-p object)
      (char-table-p (cadr (symbol-function object)))
    (and (keymapp object)
	 (char-table-p (cadr object)))))

(defun kmu-sparse-keymap-p (object)
  "Return t if OBJECT is a sparse keymap.
A sparse keymap is a keymap whose second element is not a char-table."
  (if (kmu-prefix-command-p object)
      (not (char-table-p (cadr (symbol-function object))))
    (and (keymapp object)
	 (not (char-table-p (cadr object))))))

;;; Key Lookup.

(defun kmu-lookup-local-key (keymap key &optional accept-default)
  "In KEYMAP, look up key sequence KEY.  Return the definition.

Unlike `lookup-key' (which see) this doesn't consider bindings made
in KEYMAP's parent keymap."
  (lookup-key (kmu--strip-keymap keymap) key accept-default))

(defun kmu-lookup-parent-key (keymap key &optional accept-default)
  "In KEYMAP's parent keymap, look up key sequence KEY.
Return the definition.

Unlike `lookup-key' (which see) this only conciders bindings made in
KEYMAP's parent keymap and recursivly all parent keymaps of keymaps
events in KEYMAP are bound to."
  (lookup-key (kmu--collect-parmaps keymap) key accept-default))

(defun kmu--strip-keymap (keymap)
  "Return a copy of KEYMAP with all parent keymaps removed."
  (flet ((strip-keymap (keymap)
	   (set-keymap-parent keymap nil)
	   (loop for key being the key-code of keymap
		 using (key-binding binding) do
		 (and (keymapp binding)
		      (not (kmu-prefix-command-p binding))
		      (strip-keymap binding)))
	   keymap))
    (strip-keymap (copy-keymap keymap))))

(defun kmu--collect-parmaps (keymap)
  "Return a copy of KEYMAP with all local bindings removed."
  (flet ((collect-parmaps (keymap)
	   (let ((new-keymap (make-sparse-keymap)))
	     (set-keymap-parent new-keymap (keymap-parent keymap))
	     (set-keymap-parent keymap nil)
	     (loop for key being the key-code of keymap
		   using (key-binding binding) do
		   (and (keymapp binding)
			(not (kmu-prefix-command-p binding))
			(define-key new-keymap (vector key)
			  (collect-parmaps binding))))
	     new-keymap)))
    (collect-parmaps (copy-keymap keymap))))

;;; Keymap Variables.

(defun kmu-keymap-variable (--keymap-- &rest exclude)
  "Return a symbol whose value is KEYMAP.

Comparison is done with `eq'.  If there are multiple variables
whose value is KEYMAP it is undefined which is returned.

Ignore symbols listed in optional EXCLUDE.  Use this to prevent
symbols from being returned which are dynamically bound to KEYMAP."
  (setq exclude (append '(--keymap-- --match-- --symbol--) exclude))
  (let (--match--)
    (do-symbols (--symbol--)
      (and (not (memq --symbol-- exclude))
	   (boundp --symbol--)
	   (eq (symbol-value --symbol--) --keymap--)
	   (setq --match-- --symbol--)
	   (return nil)))
    --match--))

(defun kmu-keymap-parent (keymap &optional need-symbol &rest exclude)
  "Return the parent keymap of KEYMAP.

If a variable exists whose value is KEYMAP's parent keymap return that.
Otherwise if KEYMAP does not have a parent keymap return nil.  Otherwise
if KEYMAP has a parent keymap but no variable is bound to it return the
parent keymap, unless optional NEED-SYMBOL is non-nil in which case nil
is returned.

Also see `kmu-keymap-variable'."
  (let ((--parmap-- (keymap-parent keymap)))
    (when --parmap--
      (or (kmu-keymap-variable --parmap-- '--parmap--)
	  (unless need-symbol --parmap--)))))

(provide 'keymap-utils)
;;; keymap-utils.el ends here
