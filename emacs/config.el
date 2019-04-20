(setq home-dir "/Users/____/.emacs.d")
(setq scheme-exe "/opt/local/bin/scheme")

(setenv "HOME" home-dir) 
(setenv "PATH" home-dir)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives  '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("popkit" . "http://elpa.popkit.org/packages/"))

(add-to-list 'package-archives '("melpa-milkbox" .  "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;;; Also use Melpa for most packages
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

;(set-face-attribute 'default nil :font "monaco-10:weight=normal")
;(set-face-font "-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;(setq inferior-lisp-program (concat home-dir "/ccl/wx86cl64.exe"))
;(setq inferior-lisp-program (concat home-dir "/clisp/clisp-2.49/clisp.exe"))
(setq inferior-lisp-program "/opt/local/bin/sbcl")
(add-to-list 'load-path (concat home-dir "/sitelisp/slime"))
(require 'slime)
(slime-setup)
(setq swank-loader::*fasl-directory* (concat home-dir "/temp/"))
(require 'slime-autoloads)

(add-to-list 'load-path (concat home-dir "/sitelisp/"))


;;http://mumble.net/~campbell/emacs/paredit.el

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(add-to-list 'load-path (concat home-dir "/sitelisp/parenface"))
(require 'parenface)
;;(set-face-foreground 'paren-face "DimGray")
(eval-after-load 'parenface
   (progn
     (set-face-foreground 'parenface-paren-face "SteelBlue4")
     (set-face-foreground 'parenface-bracket-face "SteelBlue4")
     (set-face-foreground 'parenface-curly-face "SteelBlue4")))
		 
;; next lines from yinwang.org
;;;;;;;;;;;
;; transpose-frame
(require 'transpose-frame)
;;;;;;;;;;;

;;;;;;;;;;;;
;; Scheme 
;;;;;;;;;;;;

(require 'cmuscheme)
(setq scheme-program-name scheme-exe)         ;; 如果用 Petite 就改成 "petite"


;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))


(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))


(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))


(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))
