(trace-function
 #'call-process nil
 (lambda ()
   (format "\nload-file-name: %s" load-file-name)))


(eval-and-compile
  (require 'package)
  (setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  ;; i always fetch the archive contents on startup and during compilation, which is slow
  (package-refresh-contents)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  ;; i don't really know why this isn't the default...
  (setf use-package-always-ensure t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cf9414f229f6df728eb2a5a9420d760673cca404fee9910551caf9c91cff3bfa" default))
 '(package-selected-packages '(quelpa tron-legacy-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; fonts
;;(add-to-list 'bdf-directory-list "~/.emacs.d/bdf")

;; paste image by tarhuntas
(defun paste-clipboard-image-md5 (&optional dir)
  (interactive)
  (unless dir
    (setq dir "pasted/"))
  (unless (file-directory-p (file-name-directory dir))
    (make-directory (file-name-directory dir))
    (message (concat "Directory " (file-name-directory dir) " created.")))
  (let (file-md5)
    (setq file-md5
	  (shell-command-to-string
	    (concat
	      "scrsht_file=$(mktemp);
	      xclip -selection clipboard -t image/png -o > $scrsht_file;
	      scrsht_md5=`md5sum $scrsht_file -b | head -c 32`;
	      mv $scrsht_file "
	      dir
	      "$scrsht_md5.png;
	      echo -n $scrsht_md5")))
	      (insert (concat "[[file:pasted/" file-md5 ".png]]"))))

(global-set-key (kbd "C-g") 'paste-clipboard-image-md5)
;;===================================
(load-theme 'tron-legacy)
;; cleaning emacs interface
(menu-bar-mode -1) ; disable menu
(scroll-bar-mode -1) ; disable scrollbar
(tool-bar-mode -1) ; disable toolbar
(tooltip-mode -1) ; disable tooltips
(set-fringe-mode 10) ; give some breathing room
(setq visible-bell t) ; set up the visible bell

;;------

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))



(quelpa '(telega :fetcher github
               :repo "zevlg/telega.el"
               :branch "master"
               :files (:defaults "contrib" "etc" "server" "Makefile"))



;; =====


(create-fontset-from-fontset-spec
  "-*-fixed-medium-r-normal-*-16-*-*-*-c-*-fontset-bdf,
  japanese-jisx0208:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1983-*,
  katakana-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
  latin-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
  japanese-jisx0208-1978:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1978-*,
  thai-tis620:-misc-fixed-medium-r-normal--16-160-72-72-m-80-tis620.2529-1,
  lao:-misc-fixed-medium-r-normal--16-160-72-72-m-80-MuleLao-1,
  tibetan-1-column:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-80-MuleTibetan-1,
  ethiopic:-Admas-Ethiomx16f-Medium-R-Normal--16-150-100-100-M-160-Ethiopic-Unicode,
  tibetan:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-160-MuleTibetan-0")

(setq font-encoding-alist
      
      (append '(("Portuguese-0"  (portuguese . 0))
		("MuleTibetan-0" (tibetan . 0))
		("GB2312"        (chinese-gb2312 . 0))
		("JISX0208"      (japanese-jisx0208 . 0))
		("JISX0212"      (japanese-jisx0212 . 0))
		("VISCII"        (vietnamese-viscii-lower . 0))
		("KSC5601"       (korean-ksc5601 . 0))
		("MuleArabic-0"  (arabic-digit . 0))
		("MuleArabic-1"  (arabic-1-column . 0))
		("MuleArabic-2"  (arabic-2-column . 0)))
	      font-encoding-alist))

(set-frame-font "fontset-bdf")
;;(set-face-attribute 'default t :font "Misc Fixed Regular" :height 100 )
;; emacs dashboard
(require 'use-package)
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
;; costumization
(setq dashboard-banner-logo-title "Hacking is our weapon")
(setq dashboard-startup-banner "~/.emacs.d/themes/banner.png")
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
(setq dashboard-set-navigator t)
(setq dashboard-set-init-info t)
