;;; orgdiff.el --- Intelligently generate Org document diffs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 TEC
;;
;; Author: TEC <https://github.com/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: June 24, 2021
;; Modified: June 24, 2021
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/tec/orgdiff
;; Package-Requires: ((emacs "26.3") (transient "0.2.0") (magit "3.1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Intelligently generate Org document diffs
;;
;;; Code:

(defclass orgdiff--transient-lisp-variable-formatted (transient-variable)
  ((reader :initform #'transient-lisp-variable--reader)
   (always-read :initform t)
   (set-value :initarg :set-value :initform #'set))
  "Class used for Lisp variables, modified version of `transient-lisp-variable'.")

(cl-defmethod transient-init-value ((obj orgdiff--transient-lisp-variable-formatted))
  (oset obj value (symbol-value (oref obj variable))))

(cl-defmethod transient-infix-set ((obj orgdiff--transient-lisp-variable-formatted) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)))

(cl-defmethod transient-format-description ((obj orgdiff--transient-lisp-variable-formatted))
  (or (oref obj description)
      (symbol-name (oref obj variable))))

(cl-defmethod transient-format-value ((obj orgdiff--transient-lisp-variable-formatted))
  (let ((val (oref obj value)))
    (pcase val
      ('t (propertize "yes" 'face 'transient-argument))
      ('nil (propertize "no" 'face 'transient-unreachable))
      (_ (propertize (prin1-to-string val) 'face 'transient-value)))))

(cl-defmethod transient-prompt ((obj orgdiff--transient-lisp-variable-formatted))
  (format "Set %s: " (oref obj variable)))

(defun orgdiff--transient-lisp-variable-formatted--reader (prompt initial-input _history)
  (read--expression prompt initial-input))

(eval-when-compile
  (defmacro orgdiff--define-infix (key name description type default
                                       &rest reader)
    "Define infix with KEY, NAME, DESCRIPTION, TYPE, DEFAULT and READER as arguments."
    `(progn
       (defcustom ,(intern (concat "orgdiff-" name)) ,default
         ,description
         :type ,type
         :group 'orgdiff)
       (transient-define-infix ,(intern (concat "orgdiff--set-" name)) ()
         "Set `orgdiff--theme' from a popup buffer."
         :class 'orgdiff--transient-lisp-variable-formatted
         :variable ',(intern (concat "orgdiff-" name))
         :key ,key
         :description ,description
         :argument ,(concat "--" name)
         :reader (lambda (&rest _) ,@reader))))

  (orgdiff--define-infix
   "-1" "file-1" "First file"
   '(choice string boolean) nil
   (read-file-name "First file: " nil nil t nil
                   (lambda (f)
                     (or (equal (file-name-extension f) "org")
                         (string-match-p "/$" f)))))

  (orgdiff--define-infix
   "-2" "file-2" "Second file"
   '(choice string boolean) nil
   (read-file-name "Second file: " nil orgdiff-file-2 t nil
                   (lambda (f)
                     (or (and (equal (file-name-extension f) "org")
                              (message f)
                              (not (string= f orgdiff-file-1)))
                         (string-match-p "/$" f)))))

  (orgdiff--define-infix
   "-r" "git-revisions" "Git revision"
   '(choice string boolean) nil
   (run-at-time
    nil nil
    (lambda ()
      (transient--suspend-override)
      (magit-log-select
        (lambda (r1)
          (setq orgdiff--rev1 r1)
          (magit-log-select
            (lambda (r2)
              (setq orgdiff-git-revisions (substring-no-properties (concat orgdiff--rev1 ".." r2)))
              (transient--resume-override)
              (orgdiff-transient))
            (concat (propertize "Second revision (" 'face 'transient-heading)
                    (propertize "newer" 'face 'transient-argument)
                    (propertize "): " 'face 'transient-heading)
                    "type %p to select commit at point, %q to use the current working state instead")
            (lambda ()
              (setq orgdiff-git-revisions (substring-no-properties orgdiff--rev1))
              (transient--resume-override)
              (orgdiff-transient)
              )))
        (concat (propertize "First revision (" 'face 'transient-heading)
                (propertize "older" 'face 'transient-argument)
                (propertize "): " 'face 'transient-heading)
                "type %p to select commit at point, %q to not compare any revisions")
        (lambda ()
          (transient--resume-override)
          (orgdiff-transient)))))
   nil))

(defvar orgdiff--base-dir nil "The current directory for diffing files.")
(defvar orgdiff--rev1 nil "The first revision.")

;;;###autoload
(defun orgdiff ()
  "Generate a diff of two Org files."
  (interactive)

  (setq orgdiff--base-dir
        (with-temp-buffer
          (if (= 0 (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
              (string-trim (buffer-string))
            default-directory)))

  (unless (and orgdiff-file-1 (string-prefix-p orgdiff--base-dir
                                               (expand-file-name orgdiff-file-1)))
    (setq orgdiff-file-1
          (if (and (stringp buffer-file-name) (string= ".org" (file-name-extension buffer-file-name)))
              buffer-file-name
            (car (directory-files orgdiff--base-dir t "\\.org$" nil 1)))))

  (when (and orgdiff-file-2
             (or (string= orgdiff-file-1 orgdiff-file-2)
                 (not (string-prefix-p orgdiff--base-dir
                                       (expand-file-name orgdiff-file-2)))))
    (setq orgdiff-file-2 nil))

  (if (= 0 (call-process "git" nil nil nil "rev-parse" "--git-dir")) ; are we in a git dir?
      (if (memq major-mode '(magit-status-mode magit-log-mode))
          ;; If in a magit buffer, set git revisions from state
          (setq orgdiff-git-revisions
                (substring-no-properties
                 (or (--when-let (magit-region-values '(commit branch) t)
                       (concat (car (last it)) ".." (car it)))
                     (magit-branch-or-commit-at-point))))
        ;; Otherwise, just verify that the refs are valid
        (unless (or (null orgdiff-git-revisions)
                    (cl-every #'magit-rev-verify-commit
                              (split-string orgdiff-git-revisions "\\.\\.")))
          (setq orgdiff-git-revisions nil)))
    (setq orgdiff-git-revisions nil))

  (orgdiff-transient))

(transient-define-prefix orgdiff-transient ()
  ["Files"
   (orgdiff--set-file-1)
   (orgdiff--set-file-2)]
  ["Git"
   :if (lambda () (= 0 (call-process "git" nil nil nil "rev-parse" "--git-dir")))
   (orgdiff--set-git-revisions)]
  ["Action"
   ("s" "swap files" orgdiff--swap-files)
   ("e" "ediff" orgdiff--ediff)
   ("l" "latexdiff" orgdiff--latexdiff)])

(defun orgdiff--swap-files (&optional no-transient)
  (interactive)
  (let ((old1 orgdiff-file-1))
    (setq orgdiff-file-1 orgdiff-file-2
          orgdiff-file-2 old1))
  (unless no-transient
    (orgdiff-transient)))

;;; simple diff

(defun orgdiff--ediff ()
  (interactive)
  (when (and orgdiff-file-2 (not orgdiff-file-1))
    (orgdiff--swap-files t))
  (orgdiff-extract-revisions)
  (ediff-files orgdiff--rev1file orgdiff--rev2file))

;;; latexdiff

(defun orgdiff--latexdiff ()
  (interactive)
  (when orgdiff--export-processes
    (user-error "A latexdiff is currently in progress. Try `orgdiff-latexdiff-abort' if something seems wrong."))
  (when (and orgdiff-file-2 (not orgdiff-file-1))
    (orgdiff--swap-files t))
  (setq orgdiff-latexdiff-file-1 (when orgdiff-file-1 (concat (file-name-sans-extension orgdiff-file-1) ".tex"))
        orgdiff-latexdiff-file-2 (when orgdiff-file-2 (concat (file-name-sans-extension orgdiff-file-2) ".tex"))
        orgdiff-latexdiff-flatten (when (or orgdiff-git-revisions
                                            (not (and orgdiff-file-1 orgdiff-file-2
                                                      (string= (file-name-directory orgdiff-file-1)
                                                               (file-name-directory orgdiff-file-2)))))
                                    t))
  (orgdiff-latexdiff-transient))

(defface orgdiff-latexdiff-red
  '((t :foreground "#cd3d3e"))
  "Used to preview latexdiff's red.")

(defface orgdiff-latexdiff-blue
  '((t :foreground "#4078f2"))
  "Used to preview latexdiff's red.")

(setq orgdiff-latexdiff-flags
      `(("--type"
         (UNDERLINE .
                    ,(concat (propertize "discarded" 'face '((:strike-through t) orgdiff-latexdiff-red))
                             " " (propertize "added" 'face 'orgdiff-latexdiff-blue)
                             " " (propertize "(default)" 'face 'shadow)))
         (CTRADITIONAL .
                       ,(concat (propertize "[.."'face 'orgdiff-latexdiff-red)
                                (propertize "1"'face '((:height 0.6) orgdiff-latexdiff-red))
                                (propertize "]"'face 'orgdiff-latexdiff-red)
                                " " (propertize "added" 'face '((:height 1.15) variable-pitch orgdiff-latexdiff-blue))))
         (TRADITIONAL .
                      ,(concat "[.." (propertize "1" 'face '(:height 0.6)) "]"
                               " " (propertize "added" 'face '((:height 1.15) variable-pitch))))
         (CFONT .
                ,(concat (propertize "discarded" 'face '((:height 0.7) orgdiff-latexdiff-red))
                         " " (propertize "added" 'face '((:height 1.15) variable-pitch orgdiff-latexdiff-blue))))
         (FONTSTRIKE .
                     ,(concat (propertize "discarded" 'face '((:strike-through t) small))
                              " " (propertize "added" 'face '((:height 1.15) variable-pitch))))
         (CHANGEBAR . ,(propertize "no markup, change marks in margins" 'face 'font-lock-doc-face))
         (CCHANGEBAR .
                     ,(concat (propertize "CHANGEBAR" 'face 'font-lock-function-name-face)
                              (propertize " + " 'face 'font-lock-comment-face)
                              (propertize "discarded" 'face 'orgdiff-latexdiff-red)
                              " " (propertize "added" 'face 'orgdiff-latexdiff-blue)))
         (CFONTCHBAR .
                     ,(concat (propertize "CHANGEBAR" 'face 'font-lock-function-name-face)
                              (propertize " + " 'face 'font-lock-comment-face)
                              (propertize "discarded" 'face '(small orgdiff-latexdiff-red))
                              " " (propertize "added" 'face '((:height 1.15) variable-pitch orgdiff-latexdiff-blue))))
         (CULINECHBAR .
                      ,(concat (propertize "CHANGEBAR" 'face 'font-lock-function-name-face)
                               (propertize " + " 'face 'font-lock-comment-face)
                               (propertize "discarded" 'face '((:strike-through t) orgdiff-latexdiff-red))
                               " " (propertize "added" 'face 'orgdiff-latexdiff-blue)))
         (INVISIBLE . "added")
         (BOLD .
               ,(propertize "added" 'face 'bold))
         (PDFCOMMENT .
                     ,(concat (propertize "discarded text in PDF comment, " 'face 'font-lock-doc-face) (propertize "added" 'face 'underline))))
        ("--subtype"
         (SAFE . ,(propertize "No additional markup (default, reccomended)" 'face 'font-lock-doc-face))
         (MARGIN . ,(propertize "Mark start and end of changed block with symbol in margin" 'face 'font-lock-doc-face))
         (COLOR .
                ,(concat (propertize "deleted passages" 'face 'orgdiff-latexdiff-red)
                         " " (propertize "added passages" 'face 'orgdiff-latexdiff-blue)))
         (ZLABEL . ,(propertize "Highlight changed pages, requires post-processing" 'face 'font-lock-doc-face))
         (ONLYCHANGEDPAGE . ,(propertize "(Also) Highlights changed pages, no post-processing but dodgy floats" 'face 'font-lock-doc-face)))
        ("--floattype"
         (FLOATSAFE . "")
         (TRADITIONALSAFE . "")
         (IDENTICAL . ""))
        ("--math-markup"
         (off . "Supress markup in math environments. Only show new version")
         (whole . "Any change causes the whole equation to be marked as changed")
         (coarse . ,(concat " Coarse granularity. Use when content and order being changed" (propertize "(default)" 'face 'shadow)))
         (fine . "Detect and mark up small changes. Suitable if minor changes (e.g. typo fixes) are expected"))
        ("--graphics-markup"
         (off . "No highlighting for figures")
         (new-only . ,(concat " Surround new/changed figures with a blue frame" (propertize "(default)" 'face 'shadow)))
         (both . "Surround new/changed figures with a blue frame, and shrink and cross out deleted figures"))))

(defun orgdiff-latexdiff-prompt-flag (flag)
  (let* ((max-key-width (thread-last (cdr (assoc flag orgdiff-latexdiff-flags))
                          (mapcar #'car)
                          (mapcar #'symbol-name)
                          (mapcar #'length)
                          (seq-max)))
         (options
          (mapcar (lambda (opt)
                    (concat (propertize (symbol-name (car opt)) 'face 'font-lock-variable-name-face)
                            (make-string (- max-key-width (length (symbol-name (car opt))) -2) ? )
                            (cdr opt)))
                  (cdr (assoc flag orgdiff-latexdiff-flags))))
         (selection
          (completing-read (concat flag ": ")
                           options
                           nil t)))
    (with-temp-buffer
      (insert selection)
      (goto-char (point-min))
      (intern (buffer-substring-no-properties
               (point-min)
               (cdr (bounds-of-thing-at-point 'word)))))))

(eval-when-compile
  (orgdiff--define-infix
   "-a" "latexdiff-async" "Export Org to TeX asyncronously"
   'boolean nil
   (not orgdiff-latexdiff-async))

  (orgdiff--define-infix
   "-t" "latexdiff-type" "Type"
   'symbol 'UNDERLINE
   (orgdiff-latexdiff-prompt-flag "--type"))
  (orgdiff--define-infix
   "-s" "latexdiff-subtype" "Sub-type"
   'symbol 'SAFE
   (orgdiff-latexdiff-prompt-flag "--subtype"))
  (orgdiff--define-infix
   "-F" "latexdiff-floattype" "Float type"
   'symbol 'FLOATSAFE
   (orgdiff-latexdiff-prompt-flag "--floattype"))

  (orgdiff--define-infix
   "-F" "latexdiff-flatten" "Flatten includes"
   'boolean nil
   (not orgdiff-latexdiff-flatten))

  (orgdiff--define-infix
   "-s" "latexdiff-allow-spaces" "Allow spaces"
   'boolean nil
   (not orgdiff-latexdiff-allow-spaces))
  (orgdiff--define-infix
   "-m" "latexdiff-math-markup" "Math markup"
   'symbol 'coarse
   (orgdiff-latexdiff-prompt-flag "--math-markup"))
  (orgdiff--define-infix
   "-g" "latexdiff-graphics-markup" "Graphics markup"
   'symbol 'new-only
   (orgdiff-latexdiff-prompt-flag "--graphics-markup")))

(transient-define-prefix orgdiff-latexdiff-transient ()
  ["Processing"
   (orgdiff--set-latexdiff-async)
   (orgdiff--set-latexdiff-flatten)
   (orgdiff--set-latexdiff-allow-spaces)
   ;; Filter script
   ;; Preamble file
   ;; Cleanup output # custom
   ]
  ["Style "
   (orgdiff--set-latexdiff-type)
   (orgdiff--set-latexdiff-subtype)
   (orgdiff--set-latexdiff-floattype)
   (orgdiff--set-latexdiff-math-markup)
   (orgdiff--set-latexdiff-graphics-markup)
   ;; Citation markup?
   ]
  ["Action"
   ("l" "run latexdiff, compile, and open PDF" orgdiff-latexdiff--action-pdf-open)
   ("L" "run latexdiff only" orgdiff-latexdiff--action-latex)
   ;; ("P" "PDF" orgdiff-latexdiff--action-pdf)
   ;; ("p" "run latexdiff, and compile" orgdiff-latexdiff--action-latex)
   ])

(defvar orgdiff-latexdiff--completion-action nil)

(defun orgdiff-latexdiff--action-latex ()
  (interactive)
  (setq orgdiff-latexdiff--completion-action '(:format latex :action nil))
  (orgdiff-latexdiff-execute-pt1))

(defun orgdiff-latexdiff--action-latex-open ()
  (interactive)
  (setq orgdiff-latexdiff--completion-action '(:format latex :action open))
  (orgdiff-latexdiff-execute-pt1))

(defun orgdiff-latexdiff--action-pdf ()
  (interactive)
  (setq orgdiff-latexdiff--completion-action '(:format pdf :action nil))
  (orgdiff-latexdiff-execute-pt1))

(defun orgdiff-latexdiff--action-pdf-open ()
  (interactive)
  (setq orgdiff-latexdiff--completion-action '(:format pdf :action open))
  (orgdiff-latexdiff-execute-pt1))

;;; Actually building the diff

(defcustom orgdiff-latexdiff-executable "latexdiff"
  "Path to the latexdiff executable to use on the generated tex files.")

(defun orgdiff-latexdiff-execute-pt1 ()
  (orgdiff-extract-revisions)
  (orgdiff-latex-create-from-org)
  (unless (executable-find orgdiff-latexdiff-executable)
    (user-error "Could not locate the latexdiff executable!"))
  (orgdiff-latexdiff-wait-for-export-then #'orgdiff-latexdiff-execute-pt2))

(defun orgdiff-latexdiff-execute-pt2 ()
  (unless
      (and (file-exists-p (concat (file-name-sans-extension orgdiff--rev1file) ".tex"))
           (file-exists-p (concat (file-name-sans-extension orgdiff--rev2file) ".tex")))
    (user-error "Error! Org files were not sucessfully exported to LaTeX"))
  (orgdiff-latexdiff-expand)
  (orgdiff-latexdiff-do-diff)
  (pcase (plist-get orgdiff-latexdiff--completion-action :format)
    ('latex
     (let ((dest (concat (file-name-directory orgdiff-file-1)
                         (file-name-nondirectory orgdiff--difffile))))
       (rename-file orgdiff--difffile dest t)
       (when (eq 'open (plist-get orgdiff-latexdiff--completion-action :action))
         (find-file-other-window dest))))
    ('pdf
     (orgdiff-latexdiff-compile)
     (orgdiff-latexdiff-wait-for-export-then #'orgdiff-latexdiff-handle-pdf))))

(defvar orgdiff--rev1dir nil)
(defvar orgdiff--rev2dir nil)

(defvar orgdiff--rev1file nil)
(defvar orgdiff--rev2file nil)

(defun orgdiff-extract-revisions ()
  (setq orgdiff--rev1dir nil
        orgdiff--rev2dir nil)
  (when orgdiff-git-revisions
    (message "%s%s" (propertize "Orgdiff" 'face 'bold) ": Checking out revisions...")
    (let ((revisions (split-string orgdiff-git-revisions "\\.\\."))
          (default-directory (string-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
      (setq orgdiff--rev1dir (make-temp-file "orgdiff-" t))
      ;; I could use some sort of in-emacs piping, but I'd rather not
      (call-process "sh" nil nil nil "-c" (format "git archive --format=tar %s | tar -xf - -C %s" (car revisions) (shell-quote-argument orgdiff--rev1dir)))
      (when (cdr revisions)
        (setq orgdiff--rev2dir (make-temp-file "orgdiff-" t))
        (call-process "sh" nil nil nil "-c" (format "git archive --format=tar %s | tar -xf - -C %s" (cadr revisions) (shell-quote-argument orgdiff--rev2dir))))))
  (setq orgdiff--rev1file
        (if orgdiff--rev1dir
            (expand-file-name (file-relative-name orgdiff-file-1 orgdiff--base-dir)
                              orgdiff--rev1dir)
          orgdiff-file-1)
        orgdiff--rev2file
        (if orgdiff--rev2dir
            (expand-file-name (file-relative-name (or orgdiff-file-2 orgdiff-file-1) orgdiff--base-dir)
                              orgdiff--rev2dir)
          (or orgdiff-file-2 orgdiff-file-1))))

(defvar orgdiff--export-processes nil)

(defun orgdiff-latex-create-from-org ()
  (dolist (file (list orgdiff--rev1file orgdiff--rev2file))
    (with-temp-buffer
      (setq buffer-file-name file
            default-directory (file-name-directory file))
      (insert-file-contents file)
      (unless orgdiff-latexdiff-async
        (message "%s%s%s%s" (propertize "Orgdiff" 'face 'bold) ": Exporting " file " to TeX"))
      (let ((inhibit-message t))
        (org-mode)
        (org-latex-export-to-latex orgdiff-latexdiff-async)
        (when orgdiff-latexdiff-async
          (push (caddar org-export-stack-contents) orgdiff--export-processes))
        (set-buffer-modified-p nil))))
  (message "%s%s" (propertize "Orgdiff" 'face 'bold) ": Exporting Org files to TeX..."))

(defun orgdiff-latexdiff-wait-for-export-then (then)
  (if orgdiff--export-processes
      (progn
        (setq orgdiff--export-processes
              (delq nil (mapcar
                         (lambda (proc)
                           (if (memq (process-status proc) '(exit signal failed)) nil proc))
                         orgdiff--export-processes)))
        (run-at-time 0.5 nil #'orgdiff-latexdiff-wait-for-export-then then))
    (funcall then)))

(defun orgdiff-latexdiff-abort ()
  (interactive)
  (when orgdiff--export-processes
    (ignore-errors (mapcar #'kill-process orgdiff--export-processes))
    (message "%s processes aborted" (length orgdiff--export-processes))
    (setq orgdiff--export-processes nil)
    (org-export-stack-clear)))

(defun orgdiff-latexdiff-expand ()
  (when orgdiff-latexdiff-flatten
    (message "%s%s" (propertize "Orgdiff" 'face 'bold) ": Flattening tex files...")
    (call-process "latexpand" nil nil nil orgdiff--rev1file)
    (call-process "latexpand" nil nil nil orgdiff--rev2file)))

(defvar orgdiff--difffile nil)

(defcustom orgdiff-latexdiff-postprocess-hooks nil
  "Post-processing functions which are run on the latexdiff result.")

(defun orgdiff-latexdiff-do-diff ()
  (message "%s%s" (propertize "Orgdiff" 'face 'bold) ": latexdiff-ing tex files...")
  (with-temp-buffer
    (apply #'call-process orgdiff-latexdiff-executable nil '(t nil) nil
           (delq nil (list
                      "-t" (symbol-name orgdiff-latexdiff-type)
                      "-s" (symbol-name orgdiff-latexdiff-subtype)
                      "-f" (symbol-name orgdiff-latexdiff-floattype)
                      (when orgdiff-latexdiff-allow-spaces "--allow-spaces")
                      (format "--math-markup=%s" orgdiff-latexdiff-math-markup)
                      (format "--graphics-markup=%s" orgdiff-latexdiff-graphics-markup)
                      (expand-file-name (concat (file-name-sans-extension orgdiff--rev1file) ".tex"))
                      (expand-file-name (concat (file-name-sans-extension orgdiff--rev2file) ".tex")))))
    (setq orgdiff--difffile
          (concat
           (file-name-directory orgdiff--rev2file)
           (if (string= (file-name-nondirectory orgdiff--rev1file) (file-name-nondirectory orgdiff--rev2file))
               (file-name-base orgdiff--rev1file)
             (concat (file-name-base orgdiff--rev1file)
                     "-diff-"
                     (file-name-base orgdiff--rev2file)))
           (when orgdiff-git-revisions
             (concat "-"
                     (car (split-string orgdiff-git-revisions "\\.\\."))
                     ":"
                     (or (cadr (split-string orgdiff-git-revisions "\\.\\."))
                         "current")))
           ".tex"))
    (run-hooks 'orgdiff-latexdiff-postprocess-hooks)
    (let (before-save-hook after-save-hook write-file-functions)
      (setq buffer-file-name orgdiff--difffile)
      (save-buffer 0))))

(defcustom orgdiff-latex-compiler-priorities
  '("pdflatex" "xelatex" "lualatex")
  "A list of compiler priorities, ascending.
If two documents require different compilers, the higher priority
compiler will be used.")

(defcustom orgdiff-latex-compile-command
  (delq nil (list "latexmk"
                  "-f"
                  "-pdf"
                  "-%compiler"
                  (when (string-match-p "-shell-escape" (car org-latex-pdf-process))
                    "-shell-escape")
                  "-interaction=nonstopmode"
                  "%texfile"))
  "Compile command as a list in the form (\"CMD\" \"ARGS\"...).
\"%compiler\" is replaced with the requested compiler, and
\"%texfile\" replaced with the path to the .tex file.")

(defun orgdiff-latexdiff-compile ()
  (let* ((compilers (mapcar
                     (lambda (file)
                       (with-temp-buffer
                         (insert-file-contents file)
                         (when (search-forward
                                (string-trim-right (format org-latex-compiler-file-string "") "\n")
                                nil t)
                           (buffer-substring-no-properties (point) (progn (end-of-line) (point))))))
                     (list (concat (file-name-sans-extension orgdiff--rev1file) ".tex")
                           (concat (file-name-sans-extension orgdiff--rev2file) ".tex"))))
         (compiler (nth (max (cl-position (car compilers) orgdiff-latex-compiler-priorities :test #'equal)
                             (cl-position (cadr compilers) orgdiff-latex-compiler-priorities :test #'equal))
                        orgdiff-latex-compiler-priorities))
         (compile-command
          (mapcar
           (lambda (arg)
             (thread-last arg
               (replace-regexp-in-string "%compiler" compiler)
               (replace-regexp-in-string "%texfile"
                                         (expand-file-name orgdiff--difffile))))
           orgdiff-latex-compile-command))
         (default-directory (file-name-directory orgdiff--difffile)))
    (message "%s%s" (propertize "Orgdiff" 'face 'bold) ": compiling diff file...")
    (push
     (apply #'start-process "orgdiff-latexdiff-compile" "*orgdiff-latexdiff-compile*" compile-command)
     orgdiff--export-processes)))

(defun orgdiff-latexdiff-handle-pdf ()
  (message "%s%s" (propertize "Orgdiff" 'face 'bold) ": diff created.")
  ;; Cleanup
  (when org-latex-remove-logfiles
    (mapc #'delete-file
          (directory-files
           (file-name-directory orgdiff--difffile)
           t
           (concat (regexp-quote (file-name-base orgdiff--difffile))
                   "\\(?:\\.[0-9]+\\)?\\."
                   (regexp-opt org-latex-logfiles-extensions))
           t)))
  ;; Move the PDF to the right place
  (let ((pdf-file (concat (file-name-sans-extension orgdiff--difffile) ".pdf"))
        (dest (concat (file-name-directory orgdiff-file-1)
                      (file-name-base orgdiff--difffile)
                      ".pdf")))
    (unless (file-exists-p pdf-file)
      (user-error "Error! Diff PDF was not produced"))
    (rename-file pdf-file dest t)
    (when (eq 'open (plist-get orgdiff-latexdiff--completion-action :action))
      (find-file-other-window dest))))

(provide 'orgdiff)
;;; orgdiff.el ends here
