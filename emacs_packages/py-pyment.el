;;; py-pyment.el --- Use pyment to generate the proper docstring.

;; Copyright (C) 2017, Manuel Kaufmann <humitos@gmail.com>

;; Author: Manuel Kaufmann <humitos@gmail.com>
;; URL: https://github.com/humitos/py-cmd-buffer/py-pyment.el
;; Version: 0.1
;; Package-Requires: ((buftra "0.6"))

;;; Commentary:

;; Provides the `py-pyment.el' command, which uses the external
;; "pyment" tool to generate the proper docstrings.

;; To automatically apply when saving a python file, use the
;; following code:

;;   (add-hook 'python-mode-hook 'py-pyment-enable-on-save)

;; To customize the behaviour of "pyment" you can set the
;; py-pyment-options e.g.

;;   (setq py-pyment-options '("--output reST" "--convert"))

;; To disable this command at certain situation you can set
;; py-pyment-enabled to nil e.g.

;;  (setq py-pyment-enabled nil)

;;; Code:

;; (require 'buftra)

(defgroup py-pyment nil
  "Use pyment to beautify a Python buffer."
  :group 'convenience
  :prefix "py-pyment-")


(defcustom py-pyment-options nil
  "Options used for pyment.
Note that `--write' is used by default."
  :group 'py-pyment
  :type '(repeat (string :tag "option")))

(defcustom py-pyment-enabled t
  "Wheter or not run \"pyment\" command even if the hook is ran."
  :group 'py-pyment
  :type 'boolean)

(defun py-pyment--call-executable (errbuf file)
  (zerop (apply 'call-process "pyment" nil errbuf nil
                (append py-pyment-options `("--write", file)))))


;;;###autoload
(defun py-pyment-buffer ()
  "Uses the \"pyment\" tool to reformat the current buffer."
  (interactive)
  (if py-pyment-enabled
      (buftra--apply-executable-to-buffer "pyment"
                                          'py-pyment--call-executable
                                          nil
                                          "py"
                                          nil)))


;;;###autoload
(defun py-pyment-region ()
  "Uses the \"pyment\" tool to reformat the selected region."
  (interactive)
  (buftra--apply-executable-to-buffer "pyment"
                                      'py-pyment--call-executable
                                      t
                                      "py"
                                      nil))


;;;###autoload
(defun py-pyment-enable-on-save ()
  "Pre-save hook to be used before running pyment."
  (interactive)
  (add-hook 'before-save-hook 'py-pyment-buffer nil t))

;; BEGIN manually copied buftra functions -----------------
;; from: https://github.com/paetzke/buftra.el
;; from: /data/Dropbox/dotfiles/emacs_packages/buftra.el
;; buftra.el
;; Copyright (C) 2015-2016, Friedrich Paetzke <f.paetzke@gmail.com>
;; Author: Friedrich Paetzke <f.paetzke@gmail.com>
;; URL: https://github.com/paetzke/buftra.el
;; Version: 0.6

;; This code is initially copied from go-mode.el (copyright the go-mode authors).
;; See LICENSE or https://raw.githubusercontent.com/dominikh/go-mode.el/master/LICENSE


(defun buftra--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in buftra--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (setq line-offset (+ line-offset len))
                (kill-whole-line len)
                (pop kill-ring)))
             (t
              (error "invalid rcs patch or internal error in buftra--apply-rcs-patch")))))))))


(defun buftra--replace-region (filename)
  (delete-region (region-beginning) (region-end))
  (insert-file-contents filename))


(defun buftra--apply-executable-to-buffer (executable-name
                                           executable-call
                                           only-on-region
                                           file-extension
                                           ignore-return-code)
  "Formats the current buffer according to the executable"
  (when (not (executable-find executable-name))
    (error (format "%s command not found." executable-name)))
  (let ((tmpfile (make-temp-file executable-name nil (concat "." file-extension)))
        (patchbuf (get-buffer-create (format "*%s patch*" executable-name)))
        (errbuf (get-buffer-create (format "*%s Errors*" executable-name)))
        (coding-system-for-read buffer-file-coding-system)
        (coding-system-for-write buffer-file-coding-system))
    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (if (and only-on-region (use-region-p))
        (write-region (region-beginning) (region-end) tmpfile)
      (write-region nil nil tmpfile))

    (if (or (funcall executable-call errbuf tmpfile)
            (ignore-return-code))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil
                                        patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (pop kill-ring)
              (message (format "Buffer is already %sed" executable-name)))

          (if only-on-region
              (buftra--replace-region tmpfile)
            (buftra--apply-rcs-patch patchbuf))

          (kill-buffer errbuf)
          (pop kill-ring)
          (message (format "Applied %s" executable-name)))
      (error (format "Could not apply %s. Check *%s Errors* for details"
                     executable-name executable-name)))
    (kill-buffer patchbuf)
    (pop kill-ring)
    (delete-file tmpfile)))
;; end buftra load

(provide 'py-pyment)

;;; py-pyment.el ends here
