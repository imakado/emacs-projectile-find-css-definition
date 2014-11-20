;;; projectile-find-css-definition.el --- find css definition at point in the project

;; Copyright (C) 2014 by IMAKADO


;; Prefix: find-css-definition:
;; Author: Kenji Imakado <ken.imakado -at- gmail.com>
;; Maintainer: imakado
;; Created: :2014-11-20
;; Keywords: 
;; URL:
;; Version: 0.0.1
;; Package-Requires: ((imakado "0.12"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To find css definition at point,
;; M-x find-css-definition:popwin-matched-css-file-at-point

;;; Code:


(require 'imakado)
(require 'projectile)
(require 'popwin)
(require 'css-mode)
(require 'anything-config)

(defvar find-css-definition:css-regexps '("\\.css$"))
(defvar find-css-definition:js-regexps '("\\.js$"))


(defun find-css-definition:projectile-current-project-files-full-path ()
  (let ((root (projectile-project-root)))
    (imakado-aand (projectile-current-project-files)
                  (mapcar (lambda (f)
                            (expand-file-name f root))
                          it))))

(defun find-css-definition:matched-css-files-at-point ()
  (let* ((w (with-syntax-table css-mode-syntax-table
              (thing-at-point 'symbol)))
         (re (rx-to-string `(seq symbol-start ,w symbol-end)))
         (css-files (let ((files (find-css-definition:projectile-current-project-files-full-path)))
                      (imakado-remove-if-not-some-match find-css-definition:css-regexps files)))
         (ret nil))
    (loop for f in css-files
          when (and (file-exists-p f)
                    (file-regular-p f)
                    (file-readable-p f))
          do (imakado-with-temp-buffer-file f
               (css-mode)
               (font-lock-mode t)
               (save-excursion (font-lock-fontify-region (point-min) (point-max)))
               (loop initially (goto-char (point-min))
                     while (re-search-forward re nil t)
                     do (when (find-css-definition:check-faces '(css-selector))
                          (push (list f (point))
                                ret)))))
    (reverse ret)))

(defun find-css-definition:matched-js-files-at-point ()
  (let* ((w (with-syntax-table css-mode-syntax-table
              (thing-at-point 'symbol)))
         (re (rx-to-string `(seq symbol-start ,w symbol-end)))
         (css-files (let ((files (find-css-definition:projectile-current-project-files-full-path)))
                      (imakado-remove-if-not-some-match find-css-definition:js-regexps files)))
         (ret nil))
    (loop for f in css-files
          when (and (file-exists-p f)
                    (file-regular-p f)
                    (file-readable-p f))
          do (imakado-with-temp-buffer-file f
               (with-syntax-table css-mode-syntax-table
                 (loop initially (goto-char (point-min))
                       while 
                       (re-search-forward re nil t)
                       do (push (list f (point))
                                ret)))))
    (reverse ret)))

(defun* find-css-definition:check-faces (face-names &optional (point (point)))
  (let* ((face (get-text-property point 'face))
         (faces (if (listp face) face (list face))))
    (imakado-some (lambda (face-sym)
                    (memq face-sym faces))
                  face-names)))

(defvar find-css-definition:matched-css-files nil)
(defun find-css-definition:popwin-matched-css-file-at-point ()
  (interactive)
  (imakado-aand (setq find-css-definition:matched-css-files (find-css-definition:matched-css-files-at-point))
                (cond
                 ((= (length it) 1)
                  (imakado-dbind (f p) (first it)
                    (select-window (popwin:popup-buffer (find-file-noselect f)))
                    (goto-char p)))
                 (t (find-css-definition:anything-matched-css-file)))))

(defvar find-css-definition:matched-js-files nil)
(defun find-css-definition:popwin-matched-js-file-at-point ()
  (interactive)
  (imakado-aand (setq find-css-definition:matched-js-files (find-css-definition:matched-js-files-at-point))
                (cond
                 ((= (length it) 1)
                  (imakado-dbind (f p) (first it)
                    (select-window (popwin:popup-buffer (find-file-noselect f)))
                    (goto-char p)))
                 (t (find-css-definition:anything-matched-js-file)))))

(defun find-css-definition:matched-css-default-action (elm)
  (imakado-dbind (f p) elm
    (select-window (popwin:popup-buffer (find-file-noselect f)))
    (goto-char p)))

(defun find-css-definition:anything-matched-css-cands ()
  (with-anything-current-buffer
    (let ((lol find-css-definition:matched-css-files))
      (loop for l in lol
            collect
            (imakado-dbind (f p) l
              `(,(format "%s %s" f p) . ,l))))))

(defun find-css-definition:anything-matched-js-cands ()
  (with-anything-current-buffer
    (let ((lol find-css-definition:matched-js-files))
      (loop for l in lol
            collect
            (imakado-dbind (f p) l
              `(,(format "%s %s" f p) . ,l))))))

(defvar find-css-definition:anything-matched-css-source
  `((name . "js -> css")
    (candidates . find-css-definition:anything-matched-css-cands)
    (persistent-action . find-css-definition:anything-matched-css-persistent-action)
    (persistent-help . "Show this entry")
    (action . find-css-definition:matched-css-default-action)
    ))

(defvar find-css-definition:anything-matched-js-source
  `((name . "js -> css")
    (candidates . find-css-definition:anything-matched-js-cands)
    (persistent-action . find-css-definition:anything-matched-css-persistent-action)
    (persistent-help . "Show this entry")
    (action . find-css-definition:matched-css-default-action)
    ))

(defun find-css-definition:anything-matched-css-persistent-action (elm)
  (imakado-dbind (f p) elm
    (pop-to-buffer (find-file-noselect f))
    (with-current-buffer (find-file-noselect f)
      (goto-char p)
      (anything-match-line-color-current-line))))

(defun find-css-definition:anything-matched-css-file ()
  "Preconfigured `anything' for `imenu'."
  (interactive)
  (anything :sources 'find-css-definition:anything-matched-css-source
            :buffer "*js util*"
            :keymap (find-css-definition:anything-matched-css-map)
            ))

(defun find-css-definition:anything-matched-js-file ()
  "Preconfigured `anything' for `imenu'."
  (interactive)
  (anything :sources 'find-css-definition:anything-matched-js-source
            :buffer "*js util*"
            :keymap (find-css-definition:anything-matched-css-map)
            ))

(defun find-css-definition:anything-matched-css-prev ()
  (interactive)
  (anything-previous-line)
  (anything-execute-persistent-action)
  (let ((anything-scroll-amount 3))
    (anything-scroll-other-window-base 'recenter)))

(defun find-css-definition:anything-matched-css-next ()
  (interactive)
  (anything-next-line)
  (anything-execute-persistent-action)
  (let ((anything-scroll-amount 3))
    (anything-scroll-other-window-base 'recenter)))

(defun find-css-definition:anything-matched-css-map ()
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "C-p") 'find-css-definition:anything-matched-css-prev)
    (define-key map (kbd "C-n") 'find-css-definition:anything-matched-css-next)
    map))

(provide 'projectile-find-css-definition)

;;; projectile-find-css-definition.el ends here
