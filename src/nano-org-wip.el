;;; nano-org-wip.el --- N Λ N O org mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Nicolas P. Rougier
;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

(require 'org)
(require 'org-indent)
(require 'nano-theme)


(defun nano-org--edit (_win position direction)
  "This function toggle font-lock at position, depending on
direction."

  (let ((beg (if (eq direction 'entered)
                 (previous-property-change (+ (point) 1))
               (previous-property-change (+ position 1))))
        (end (if (eq direction 'entered)
                 (next-property-change (point))
               (next-property-change position))))
    (if (eq direction 'left)
        (font-lock-flush beg (1+ end) )
      (if (and (not view-read-only) (not buffer-read-only))
          (font-lock-unfontify-region beg (1+ end))))))


(defun nano-org-archived-p ()
  "Returns non-nil if point is on an archived header."

  (member org-archive-tag (org-get-tags nil t)))


(defun nano-org-folded-p (&optional types)
  "Returns non-nil if point is on a folded element whose type is
specified by TYPES that defaults to '(heading drawer item block)."

  (let ((types (or types '(heading drawer item block))))
    (and (or (when (memq 'heading types) (org-at-heading-p))
             (when (memq 'drawer types) (org-at-drawer-p))
             (when (memq 'item types) (org-at-item-p))
             (when (memq 'block types) (org-at-block-p)))
         (invisible-p (point-at-eol)))))


(defun nano-org--timestamp ()
  "Prettify timestamps."

  (let* ((beg (match-beginning 1))
         (end (match-end 1))
         (keyword (match-string 2))
         (keyword (when (stringp keyword)
               (string-trim (substring-no-properties keyword))))
         (is-archived (nano-org-archived-p))
         (is-todo (string= keyword "TODO"))
         (is-done (string= keyword "DONE"))
         (is-deadline (string= keyword "DEADLINE:"))
         (tbeg (match-beginning 4))
         (tend (match-end 4))
         (active t)
         (keymap (define-keymap
                   "S-<up>"   (lambda ()
                                (interactive)
                                (let ((org-time-stamp-rounding-minutes '(0 15 30 45)))
                                  (org-timestamp-change +15 'minute)))
                   "S-<down>" (lambda ()
                                (interactive)
                                (let ((org-time-stamp-rounding-minutes '(0 15 30 45)))
                                  (org-timestamp-change -15 'minute)))))
         (date-face (cond (is-archived  '(:inherit (nano-faded nano-subtle) :overline "white"))
                          (active       '(:inherit (nano-default bold nano-subtle) :overline "white"))
                          (t            '(:inherit (nano-faded bold nano-subtle) :overline "white"))))
         (time-face (cond (is-archived  '(:inherit (nano-faded nano-subtle) :overline "white"))
                          (is-todo      '(:inherit (nano-salient-i bold) :overline "white"))
                          (is-done      '(:inherit (nano-faded-i) :overline "white"))
                          (is-deadline  '(:inherit (nano-critical-i) :overline "white"))
                          (t            '(:inherit (nano-default-i bold) :overline "white")))))
    (remove-list-of-text-properties beg end '(display))
    (add-text-properties beg end `(keymap ,keymap))
    (if t
        (let* ((time (save-match-data
                       (encode-time
                        (org-fix-decoded-time
                         (org-parse-time-string
                          (buffer-substring beg end))))))
               (date-str (format-time-string " %^b %d " time))
               (time-str (cond (is-todo " TODO ")
                               ;; (is-deadline " TODO ")
                               (is-done " DONE ")
                               (t (format-time-string " %H:%M " time)))))
          ;; year-month-day
          (add-text-properties beg (if (eq tbeg tend) end tbeg)
                               `(face ,date-face display ,date-str))
          ;; hour:minute
          (unless (eq tbeg tend)
            (add-text-properties tbeg end
                                 `(face ,time-face display ,time-str))))
        (put-text-property beg (1+ beg) 'display " ")
        (put-text-property (1- end) end 'display " ")
        ;; year-month-day
        (put-text-property beg (if (eq tbeg tend) end tbeg) 'face date-face)
        ;; hour:minute
        (unless (eq tbeg tend)
          (put-text-property (1- tbeg) tbeg 'display
                             (string (char-before tbeg) ?\s))
          (put-text-property tbeg end 'face time-face)))))

(defun nano-org--properties ()
  "Properties drawer prefix depending on folding state"

  (if (nano-org-folded-p) " " "┌ "))

(defun nano-org--logbook ()
  "Logbook drawer prefix depending on folding state"

  (if (nano-org-folded-p) " " "┌ "))

(defun nano-org--ul-list ()
  "Unordered list prefix depending on folding state"

  (if (nano-org-folded-p) "  " nil))

(defun nano-org--ol-list ()
  "Orered list prefix depending on folding state"

  (if (nano-org-folded-p) " " nil))

(defun nano-org--stars ()
  "Header prefix depending on folding state"

  (let* ((prefix (substring-no-properties (match-string 0)))
         (n (max 0 (- (length prefix) 3))))
     (concat (make-string n ? )
             (cond ((nano-org-archived-p) (propertize " " 'face 'org-archived))
                   ((nano-org-folded-p)   " ")
                   (t                     " ")))))

(defun nano-org--user ()
  "Pretty format for user"

  (let* ((user (substring-no-properties (match-string 1)))
         (user (string-replace "@" " " user)))
    (propertize user 'face (if (nano-org-archived-p)
                              'nano-faded
                             'nano-salient)
                     'pointer 'hand
                     'mouse-face (when (not (nano-org-archived-p))
                                   '(:inherit (nano-subtle bold))))))

(defvar nano-org--timestamp-re
  (concat "^\\*+[\\\t ]+"                             ;; Context: Header stars (mandatory, anonymous)
          "\\("                                       ;; Group 1: whole timestamp
          "\\("                                       ;; Group 2: TODO / DEADLINE (optional)
            "\\(?:TODO\\|DONE\\|DEADLINE:\\)[\\\t ]+"             ;;
          "\\)?"                                      ;;
          "\\(?:<\\|\\[\\)"                           ;; Anonymous group for < or [
          "\\("                                       ;; Group 3 start: date (mandatory)
            "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"  ;;  YYYY-MM-DD (mandatory)
            "\\(?: [[:word:]]+\\.?\\)?"                   ;;  day name (optional)
            "\\(?: [.+-]+[0-9ymwdh/]+\\)*"            ;;  repeater (optional)
          "\\)"                                       ;; Group 3 end
          "\\("                                       ;; Group 4 start (optional): time
            "\\(?: [0-9:-]+\\)?"                      ;;   HH:MM (optional)
            "\\(?: [.+-]+[0-9ymwdh/]+\\)*"            ;;   repeater (optional)
          "\\)"                                       ;; Group 4 end
          "\\(?:>\\|\\]\\)"                           ;; Anonynous group for > or ]
          "\\)"))                                     ;; Group 1 end

(defvar nano-org--drawer-properties-re
  "^\\(:\\)PROPERTIES:")                              ;; Group 1 for :[PROPERTIES:]

(defvar nano-org--drawer-logbook-re
  "^\\(:\\)LOGBOOK:")                                 ;; Group 1 for :[LOGBOOK:]

(defvar nano-org--drawer-closed-re
  "^\\(CLOSED:\\)")                                   ;; Group 1 for CLOSED:

(defvar nano-org--drawer-content-re
  "^\\(:\\)[a-zA-Z]+:")                               ;; Group 1 for :[XXX:]

(defvar nano-org--drawer-clock-re
  "^\\(CLOCK:\\)")                                    ;; Group 1 for CLOCK:

(defvar nano-org--drawer-end-re
  "^\\(:\\)END:")                                     ;; Group 1 for :[END:]

(defvar nano-org--stars-re
  "^\\(\\*\\{2,\\}\\) ")                              ;; Group 1 for **...

(defvar nano-org--ul-list-re
  "^\\(- \\)")                                        ;; Group 1 for -

(defvar nano-org--ol-list-re
  "^\\([0-9].\\)")                                    ;; Group 1 for #.

(defvar nano-org--user-re
  "\\(@[a-zA-Z]+\\)")                                 ;; Group 1 for @XXX

(defun org-nano--cycle-hook (&rest args)
   (font-lock-update))


(defun nano-org-wip ()
  "NANO org mode (WIP)"

  (interactive)
  (org-mode)
  (org-indent-mode)
  (font-lock-add-keywords nil
     `((,nano-org--timestamp-re         1 (nano-org--timestamp) nil t)
       (,nano-org--drawer-content-re    1 `(face nil display "│ "))
       (,nano-org--drawer-end-re        1 `(face nil display "└ "))
       (,nano-org--drawer-clock-re      1 `(face nil display "│  "))
       (,nano-org--drawer-properties-re 1 `(face nil display ,(nano-org--properties)))
       (,nano-org--drawer-logbook-re    1 `(face nil display ,(nano-org--logbook)))
       (,nano-org--drawer-closed-re     1 `(face nil display " "))
       (,nano-org--user-re              1 `(face nil display ,(nano-org--user)))
       (,nano-org--ul-list-re           1 `(face nil display ,(nano-org--ul-list)))
       (,nano-org--ol-list-re           1 `(face nil display ,(nano-org--ol-list)))
       (,nano-org--stars-re             1 `(face nil display ,(nano-org--stars))))
    'append)

  (add-hook 'org-cycle-hook #'org-nano--cycle-hook)
  (advice-add 'org-fold-hide-drawer-toggle :after
              #'org-nano--cycle-hook)
  (setq org-time-stamp-formats '("%Y-%m-%d" . "%Y-%m-%d %H:%M"))
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (face-remap-add-relative 'org-level-1 'bold)
  (face-remap-add-relative 'org-level-2 'bold)
  (face-remap-add-relative 'org-level-3 'default)
  (face-remap-add-relative 'org-tag '(nano-popout bold))
  (face-remap-add-relative 'org-date 'nano-faded)
  (cursor-sensor-mode -1)
  (font-lock-update))
