;;; iceberg256-theme.el --- Iceberg xterm256 color theme

(deftheme iceberg
  "Iceberg256 - The iceberg256 theme.")

(defvar iceberg-colors-alist
  (let* ((256color  (eq (display-color-cells (selected-frame)) 256))
     (colors `(("iceberg-accent"   . "#91acd1")
           ("iceberg-fg"       . (if ,256color "color-251" "#c6c8d1"))
           ("iceberg-bg"       . (if ,256color "color-234" "#282C34"))
           ("iceberg-bg-1"     . (if ,256color "color-233" "#121417"))
           ("iceberg-bg-hl"    . (if ,256color "color-236" "#2C323C"))
           ("iceberg-gutter"   . (if ,256color "color-239" "#4B5363"))
           ("iceberg-mono-1"   . (if ,256color "color-249" "#ABB2BF"))
           ("iceberg-mono-2"   . (if ,256color "color-245" "#828997"))
           ("iceberg-mono-3"   . (if ,256color "color-241" "#5C6370"))
           ("iceberg-cyan"     . "#89b8c2")
           ("iceberg-blue"     . "#84A0C6")
           ("iceberg-purple"   . "#A093C7")
           ("iceberg-green"    . "#87afaf")
           ("iceberg-red-1"    . "#e27878")
           ("iceberg-red-2"    . "#e98989")
           ("iceberg-orange-1" . "#e9b189")
           ("iceberg-orange-2" . "#E2A478")
           ("iceberg-gray"     . (if ,256color "color-238" "#3E4451"))
           ("iceberg-silver"   . (if ,256color "color-248" "#9DA5B4"))
           ("iceberg-black"    . (if ,256color "color-235" "#21252B"))
           ("iceberg-border"   . (if ,256color "color-233" "#181A1F")))))
    colors)
  "List of Iceberg256 colors.")

(defmacro iceberg-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
     ,@ (mapcar (lambda (cons)
              (list (intern (car cons)) (cdr cons)))
            iceberg-colors-alist))
     ,@body))

(iceberg-with-color-variables
  (custom-theme-set-faces
   'iceberg

   `(default ((t (:foreground ,iceberg-fg :background ,iceberg-bg))))
   `(success ((t (:foreground ,iceberg-green))))
   `(warning ((t (:foreground ,iceberg-orange-2))))
   `(error ((t (:foreground ,iceberg-red-1 :weight bold))))
   `(link ((t (:foreground ,iceberg-blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,iceberg-blue :underline t :weight normal))))
   `(cursor ((t (:background ,iceberg-accent))))
   `(fringe ((t (:background ,iceberg-bg))))
   `(region ((t (:background ,iceberg-gray :distant-foreground ,iceberg-mono-2))))
   `(highlight ((t (:background ,iceberg-gray :distant-foreground ,iceberg-mono-2))))
   `(hl-line ((t (:background ,iceberg-bg-hl :distant-foreground nil))))
   `(header-line ((t (:background ,iceberg-black))))
   `(vertical-border ((t (:background ,iceberg-border :foreground ,iceberg-border))))
   `(secondary-selection ((t (:background ,iceberg-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,iceberg-silver))))
   `(tooltip ((t (:foreground ,iceberg-fg :background ,iceberg-bg-1 :inherit variable-pitch))))

   `(font-lock-builtin-face ((t (:foreground ,iceberg-cyan))))
   `(font-lock-comment-face ((t (:foreground ,iceberg-mono-3 :slant italic))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,iceberg-blue))))
   `(font-lock-keyword-face ((t (:foreground ,iceberg-blue :weight normal))))
   `(font-lock-preprocessor-face ((t (:foreground ,iceberg-mono-2))))
   `(font-lock-string-face ((t (:foreground ,iceberg-green))))
   `(font-lock-type-face ((t (:foreground ,iceberg-orange-2))))
   `(font-lock-constant-face ((t (:foreground ,iceberg-purple))))
   `(font-lock-variable-name-face ((t (:foreground ,iceberg-fg))))
   `(font-lock-warning-face ((t (:foreground ,iceberg-mono-3 :bold t))))
   `(font-lock-negation-char-face ((t (:foreground ,iceberg-cyan :bold t))))


   ;; mode-line
   `(mode-line ((t (:background ,iceberg-black :foreground ,iceberg-silver :box (:color ,iceberg-border :line-width 1)))))
   `(mode-line-buffer-id ((t (:foreground ,iceberg-blue :weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,iceberg-border :foreground ,iceberg-gray :box (:color ,iceberg-border :line-width 1)))))

   ;; window-divider
   `(window-divider ((t (:foreground ,iceberg-border))))
   `(window-divider-first-pixel ((t (:foreground ,iceberg-border))))
   `(window-divider-last-pixel ((t (:foreground ,iceberg-border))))

   ;; custom
   `(custom-state ((t (:foreground ,iceberg-green))))

   ;; ido
   `(ido-first-match ((t (:foreground ,iceberg-purple :weight bold))))
   `(ido-only-match ((t (:foreground ,iceberg-red-1 :weight bold))))
   `(ido-subdir ((t (:foreground ,iceberg-blue))))
   `(ido-virtual ((t (:foreground ,iceberg-mono-3))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,iceberg-mono-3 :background ,iceberg-bg-1 :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,iceberg-red-1 :background ,iceberg-bg-1 :inverse-video nil))))

   ;; ace-window
   `(aw-background-face ((t (:inherit font-lock-comment-face))))
   `(aw-leading-char-face ((t (:foreground ,iceberg-red-1 :weight bold))))

   ;; centaur-tabs
   `(centaur-tabs-default ((t (:background ,iceberg-black :foreground ,iceberg-black))))
   `(centaur-tabs-selected ((t (:background ,iceberg-bg :foreground ,iceberg-fg :weight bold))))
   `(centaur-tabs-unselected ((t (:background ,iceberg-black :foreground ,iceberg-fg :weight light))))
   `(centaur-tabs-selected-modified ((t (:background ,iceberg-bg
                             :foreground ,iceberg-blue :weight bold))))
   `(centaur-tabs-unselected-modified ((t (:background ,iceberg-black :weight light
                               :foreground ,iceberg-blue))))
   `(centaur-tabs-active-bar-face ((t (:background ,iceberg-accent))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected :foreground,iceberg-accent))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected :foreground,iceberg-accent))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,iceberg-fg :background ,iceberg-bg-1))))
   `(company-tooltip-annotation ((t (:foreground ,iceberg-mono-2 :background ,iceberg-bg-1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,iceberg-mono-2 :background ,iceberg-gray))))
   `(company-tooltip-selection ((t (:foreground ,iceberg-fg :background ,iceberg-gray))))
   `(company-tooltip-mouse ((t (:background ,iceberg-gray))))
   `(company-tooltip-common ((t (:foreground ,iceberg-orange-2 :background ,iceberg-bg-1))))
   `(company-tooltip-common-selection ((t (:foreground ,iceberg-orange-2 :background ,iceberg-gray))))
   `(company-preview ((t (:background ,iceberg-bg))))
   `(company-preview-common ((t (:foreground ,iceberg-orange-2 :background ,iceberg-bg))))
   `(company-scrollbar-fg ((t (:background ,iceberg-mono-1))))
   `(company-scrollbar-bg ((t (:background ,iceberg-bg-1))))
   `(company-template-field ((t (:inherit highlight))))

   ;; doom-modeline
   `(doom-modeline-bar ((t (:background ,iceberg-accent))))

   ;; flyspell
   `(flyspell-duplicate ((t (:underline (:color ,iceberg-orange-1 :style wave)))))
   `(flyspell-incorrect ((t (:underline (:color ,iceberg-red-1 :style wave)))))

   ;; flymake
   `(flymake-error ((t (:underline (:color ,iceberg-red-1 :style wave)))))
   `(flymake-note ((t (:underline (:color ,iceberg-blue :style wave)))))
   `(flymake-warning ((t (:underline (:color ,iceberg-orange-1 :style wave)))))

   ;; flycheck
   `(flycheck-error ((t (:underline (:color ,iceberg-red-1 :style wave)))))
   `(flycheck-info ((t (:underline (:color ,iceberg-blue :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,iceberg-orange-1 :style wave)))))

   ;; compilation
   `(compilation-face ((t (:foreground ,iceberg-fg))))
   `(compilation-line-number ((t (:foreground ,iceberg-mono-2))))
   `(compilation-column-number ((t (:foreground ,iceberg-mono-2))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))

   ;; isearch
   `(isearch ((t (:foreground ,iceberg-bg :background ,iceberg-purple))))
   `(isearch-fail ((t (:foreground ,iceberg-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,iceberg-purple :background ,iceberg-bg-1 :underline ,iceberg-purple))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#89b8c2" :background "color-234"))))
   '(diff-hl-delete ((t (:foreground "#e27878" :background "color-234"))))
   '(diff-hl-insert ((t (:foreground "#84A0C6" :background "color-234"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#A093C7"))))

   ;; dired-async
   `(dired-async-failures ((t (:inherit error))))
   `(dired-async-message ((t (:inherit success))))
   `(dired-async-mode-message ((t (:foreground ,iceberg-purple))))

   ;; helm
   `(helm-header ((t (:foreground ,iceberg-mono-2
                  :background ,iceberg-bg
                  :underline nil
                  :box (:line-width 6 :color ,iceberg-bg)))))
   `(helm-source-header ((t (:foreground ,iceberg-blue
                     :background ,iceberg-bg
                     :underline nil
                     :weight bold
                     :box (:line-width 6 :color ,iceberg-bg)))))
   `(helm-selection ((t (:background ,iceberg-gray))))
   `(helm-selection-line ((t (:background ,iceberg-gray))))
   `(helm-visible-mark ((t (:background ,iceberg-bg :foreground ,iceberg-blue))))
   `(helm-candidate-number ((t (:foreground ,iceberg-green :background ,iceberg-bg-1))))
   `(helm-separator ((t (:background ,iceberg-bg :foreground ,iceberg-red-1))))
   `(helm-M-x-key ((t (:foreground ,iceberg-purple))))
   `(helm-bookmark-addressbook ((t (:foreground ,iceberg-purple))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,iceberg-purple))))
   `(helm-bookmark-info ((t (:foreground ,iceberg-green))))
   `(helm-bookmark-man ((t (:foreground ,iceberg-blue))))
   `(helm-bookmark-w3m ((t (:foreground ,iceberg-purple))))
   `(helm-match ((t (:foreground ,iceberg-cyan))))
   `(helm-ff-directory ((t (:foreground ,iceberg-cyan :background ,iceberg-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,iceberg-fg :background ,iceberg-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,iceberg-green :background ,iceberg-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,iceberg-red-1 :background ,iceberg-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,iceberg-orange-2 :background ,iceberg-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,iceberg-bg :background ,iceberg-orange-2 :weight normal))))
   `(helm-buffer-not-saved ((t (:foreground ,iceberg-red-1))))
   `(helm-buffer-process ((t (:foreground ,iceberg-blue))))
   `(helm-buffer-saved-out ((t (:foreground ,iceberg-fg))))
   `(helm-buffer-size ((t (:foreground ,iceberg-mono-2))))
   `(helm-buffer-directory ((t (:foreground ,iceberg-purple))))

   `(helm-grep-cmd-line ((t (:foreground ,iceberg-cyan))))
   `(helm-grep-file ((t (:foreground ,iceberg-fg))))
   `(helm-grep-finish ((t (:foreground ,iceberg-green))))
   `(helm-grep-lineno ((t (:foreground ,iceberg-mono-2))))
   `(helm-grep-finish ((t (:foreground ,iceberg-red-1))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-swoop-target-line-block-face ((t (:background ,iceberg-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-line-face ((t (:background ,iceberg-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-word-face ((t (:background ,iceberg-purple :foreground "#ffffff"))))
   `(helm-locate-finish ((t (:foreground ,iceberg-green))))
   `(info-menu-star ((t (:foreground ,iceberg-red-1))))

   ;; ivy
   `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,iceberg-green))))
   `(ivy-current-match ((t (:background ,iceberg-gray :weight normal))))
   `(ivy-highlight-face ((t (:inherit font-lock-builtin-face))))
   `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,iceberg-red-1))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,iceberg-bg-hl))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background ,iceberg-black :foreground ,iceberg-purple :weight semi-bold))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2 :background ,iceberg-black :foreground ,iceberg-green :weight semi-bold))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2 :background ,iceberg-black :foreground ,iceberg-blue :weight semi-bold))))
   `(ivy-minibuffer-match-highlight ((t (:inherit ivy-current-match))))
   `(ivy-modified-buffer ((t (:inherit default :foreground ,iceberg-orange-1))))
   `(ivy-virtual ((t (:inherit font-lock-builtin-face :slant italic))))

   ;; counsel
   `(counsel-key-binding ((t (:foreground ,iceberg-blue :weight bold))))

   ;; swiper
   `(swiper-match-face-1 ((t (:inherit ivy-minibuffer-match-face-1))))
   `(swiper-match-face-2 ((t (:inherit ivy-minibuffer-match-face-2))))
   `(swiper-match-face-3 ((t (:inherit ivy-minibuffer-match-face-3))))
   `(swiper-match-face-4 ((t (:inherit ivy-minibuffer-match-face-4))))

   ;; eshell
   `(eshell-ls-archive ((t (:foreground ,iceberg-purple :weight bold))))
   `(eshell-ls-backup ((t (:foreground ,iceberg-orange-2))))
   `(eshell-ls-clutter ((t (:foreground ,iceberg-red-2 :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,iceberg-blue :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,iceberg-green :weight bold))))
   `(eshell-ls-missing ((t (:foreground ,iceberg-red-1 :weight bold))))
   `(eshell-ls-product ((t (:foreground ,iceberg-orange-2))))
   `(eshell-ls-special ((t (:foreground "#FD5FF1" :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,iceberg-cyan :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,iceberg-mono-1))))
   `(eshell-prompt ((t (:inherit minibuffer-prompt))))

   ;; man
   `(Man-overstrike ((t (:inherit font-lock-type-face :weight bold))))
   `(Man-underline ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; woman
   `(woman-bold ((t (:inherit font-lock-type-face :weight bold))))
   `(woman-italic ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; dictionary
   `(dictionary-button-face ((t (:inherit widget-button))))
   `(dictionary-reference-face ((t (:inherit font-lock-type-face :weight bold))))
   `(dictionary-word-entry-face ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; erc
   `(erc-error-face ((t (:inherit error))))
   `(erc-input-face ((t (:inherit shadow))))
   `(erc-my-nick-face ((t (:foreground ,iceberg-accent))))
   `(erc-notice-face ((t (:inherit font-lock-comment-face))))
   `(erc-timestamp-face ((t (:foreground ,iceberg-green :weight bold))))

   ;; jabber
   `(jabber-roster-user-online ((t (:foreground ,iceberg-green))))
   `(jabber-roster-user-away ((t (:foreground ,iceberg-red-1))))
   `(jabber-roster-user-xa ((t (:foreground ,iceberg-red-2))))
   `(jabber-roster-user-dnd ((t (:foreground ,iceberg-purple))))
   `(jabber-roster-user-chatty ((t (:foreground ,iceberg-orange-2))))
   `(jabber-roster-user-error ((t (:foreground ,iceberg-red-1 :bold t))))
   `(jabber-roster-user-offline ((t (:foreground ,iceberg-mono-3))))
   `(jabber-chat-prompt-local ((t (:foreground ,iceberg-blue))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,iceberg-orange-2))))
   `(jabber-chat-prompt-system ((t (:foreground ,iceberg-mono-3))))
   `(jabber-chat-error ((t (:inherit jabber-roster-user-error))))
   `(jabber-rare-time-face ((t (:foreground ,iceberg-cyan))))
   `(jabber-activity-face ((t (:inherit jabber-chat-prompt-foreign))))
   `(jabber-activity-personal-face ((t (:inherit jabber-chat-prompt-local))))

   ;; eww
   `(eww-form-checkbox ((t (:inherit eww-form-submit))))
   `(eww-form-file ((t (:inherit eww-form-submit))))
   `(eww-form-select ((t (:inherit eww-form-submit))))
   `(eww-form-submit ((t (:background ,iceberg-gray :foreground ,iceberg-fg :box (:line-width 2 :color ,iceberg-gray :style released-button)))))
   `(eww-form-text ((t (:background "#505050" :foreground "white" :box (:line-width 1)))))
   `(eww-form-textarea ((t (:inherit eww-form-text))))
   `(eww-invalid-certificate ((t (:foreground ,iceberg-red-1))))
   `(eww-valid-certificate ((t (:foreground ,iceberg-green))))


   ;; message
   `(message-cited-text ((t (:foreground ,iceberg-cyan))))
   `(message-header-cc ((t (:foreground ,iceberg-green :weight bold))))
   `(message-header-name ((t (:foreground ,iceberg-purple))))
   `(message-header-newsgroups ((t (:foreground ,iceberg-orange-2 :weight bold :slant italic))))
   `(message-header-other ((t (:foreground ,iceberg-purple))))
   `(message-header-subject ((t (:foreground ,iceberg-blue))))
   `(message-header-to ((t (:foreground ,iceberg-orange-2 :weight bold))))
   `(message-header-xheader ((t (:foreground ,iceberg-silver))))
   `(message-mml ((t (:foreground ,iceberg-purple))))
   `(message-separator ((t (:foreground ,iceberg-mono-3 :slant italic))))

   ;; epa
   `(epa-field-body ((t (:foreground ,iceberg-blue :slant italic))))
   `(epa-field-name ((t (:foreground ,iceberg-cyan :weight bold))))

   ;; js2-mode
   `(js2-error ((t (:underline (:color ,iceberg-red-1 :style wave)))))
   `(js2-external-variable ((t (:foreground ,iceberg-cyan))))
   `(js2-warning ((t (:underline (:color ,iceberg-orange-1 :style wave)))))
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,iceberg-mono-1))))
   `(js2-jsdoc-tag ((t (:foreground ,iceberg-blue))))
   `(js2-jsdoc-type ((t (:foreground ,iceberg-orange-2))))
   `(js2-jsdoc-value((t (:foreground ,iceberg-purple))))
   `(js2-object-property ((t (:foreground ,iceberg-fg))))

   ;; elfeed
   `(elfeed-log-debug-level-face ((t (:background ,iceberg-black :foreground ,iceberg-green))))
   `(elfeed-log-error-level-face ((t (:background ,iceberg-black :foreground ,iceberg-red-1))))
   `(elfeed-log-info-level-face ((t (:background ,iceberg-black :foreground ,iceberg-blue))))
   `(elfeed-log-warn-level-face ((t (:background ,iceberg-black :foreground ,iceberg-orange-1))))
   `(elfeed-search-date-face ((t (:foreground ,iceberg-purple))))
   `(elfeed-search-feed-face ((t (:foreground ,iceberg-blue))))
   `(elfeed-search-tag-face ((t (:foreground ,iceberg-green))))
   `(elfeed-search-title-face ((t (:foreground ,iceberg-mono-1))))
   `(elfeed-search-unread-count-face ((t (:foreground ,iceberg-silver))))

   ;; perspective
   `(persp-selected-face ((t (:foreground ,iceberg-blue))))

   ;; powerline
   `(powerline-active1 ((,class (:background ,iceberg-bg-hl :foreground ,iceberg-blue))))
   `(powerline-active2 ((,class (:background ,iceberg-bg-hl :foreground ,iceberg-purple))))
   `(powerline-inactive1 ((,class (:background ,iceberg-bg :foreground ,iceberg-fg))))
   `(powerline-inactive2 ((,class (:background ,iceberg-bg :foreground ,iceberg-fg))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,iceberg-blue))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,iceberg-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,iceberg-orange-1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,iceberg-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,iceberg-purple))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,iceberg-orange-2))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,iceberg-blue))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,iceberg-green))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,iceberg-orange-1))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,iceberg-cyan))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,iceberg-purple))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,iceberg-orange-2))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,iceberg-red-1 :weight bold))))

   ;; rbenv
   `(rbenv-active-ruby-face ((t (:foreground ,iceberg-green))))

   ;; elixir
   `(elixir-iceberg-face ((t (:foreground ,iceberg-cyan))))
   `(elixir-attribute-face ((t (:foreground ,iceberg-purple))))

   ;; show-paren
   `(show-paren-match ((,class (:foreground ,iceberg-purple :inherit bold :underline t))))
   `(show-paren-mismatch ((,class (:foreground ,iceberg-red-1 :inherit bold :underline t))))

   ;; sh-mode
   `(sh-heredoc ((t (:inherit font-lock-string-face :slant italic))))

   ;; cider
   `(cider-fringe-good-face ((t (:foreground ,iceberg-green))))

   ;; sly
   `(sly-error-face ((t (:underline (:color ,iceberg-red-1 :style wave)))))
   `(sly-mrepl-note-face ((t (:inherit font-lock-comment-face))))
   `(sly-mrepl-output-face ((t (:inherit font-lock-string-face))))
   `(sly-mrepl-prompt-face ((t (:inherit comint-highlight-prompt))))
   `(sly-note-face ((t (:underline (:color ,iceberg-blue :style wave)))))
   `(sly-style-warning-face ((t (:underline (:color ,iceberg-orange-2 :style wave)))))
   `(sly-warning-face ((t (:underline (:color ,iceberg-orange-1 :style wave)))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,iceberg-red-1 :background ,iceberg-gray :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,iceberg-gray :weight bold))))

   ;; lispy
   `(lispy-face-hint ((t (:background ,iceberg-border :foreground ,iceberg-orange-2))))

   ;; lispyville
   `(lispyville-special-face ((t (:foreground ,iceberg-cyan))))

   ;; spaceline
   `(spaceline-flycheck-error  ((,class (:foreground ,iceberg-red-1))))
   `(spaceline-flycheck-info   ((,class (:foreground ,iceberg-green))))
   `(spaceline-flycheck-warning((,class (:foreground ,iceberg-orange-1))))
   `(spaceline-python-venv ((,class (:foreground ,iceberg-purple))))

   ;; solaire mode
   `(solaire-default-face ((,class (:inherit default :background ,iceberg-black))))
   `(solaire-minibuffer-face ((,class (:inherit default :background ,iceberg-black))))

   ;; web-mode
   `(web-mode-doctype-face ((t (:inherit font-lock-comment-face))))
   `(web-mode-error-face ((t (:background ,iceberg-black :foreground ,iceberg-red-1))))
   `(web-mode-html-attr-equal-face ((t (:inherit default))))
   `(web-mode-html-attr-name-face ((t (:foreground ,iceberg-purple))))
   `(web-mode-html-tag-bracket-face ((t (:inherit default))))
   `(web-mode-html-tag-face ((t (:foreground ,iceberg-blue))))
   `(web-mode-symbol-face ((t (:foreground ,iceberg-orange-1))))

   ;; nxml
   `(nxml-attribute-local-name ((t (:foreground ,iceberg-purple))))
   `(nxml-element-local-name ((t (:foreground ,iceberg-blue))))
   `(nxml-markup-declaration-delimiter ((t (:inherit (font-lock-comment-face nxml-delimiter)))))
   `(nxml-processing-instruction-delimiter ((t (:inherit nxml-markup-declaration-delimiter))))

   ;; flx-ido
   `(flx-highlight-face ((t (:inherit (link) :weight bold))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:foreground ,iceberg-blue))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground "#FFFFFF" :background ,iceberg-purple))))
   `(rpm-spec-macro-face ((t (:foreground ,iceberg-cyan))))
   `(rpm-spec-var-face ((t (:foreground ,iceberg-purple))))
   `(rpm-spec-doc-face ((t (:foreground ,iceberg-orange-1))))
   `(rpm-spec-dir-face ((t (:foreground ,iceberg-cyan))))
   `(rpm-spec-package-face ((t (:foreground ,iceberg-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,iceberg-red-2))))
   `(rpm-spec-section-face ((t (:foreground ,iceberg-orange-2))))

   ;; guix
   `(guix-true ((t (:foreground ,iceberg-green :weight bold))))
   `(guix-build-log-phase-end ((t (:inherit success))))
   `(guix-build-log-phase-start ((t (:inherit success :weight bold))))

   ;; gomoku
   `(gomoku-O ((t (:foreground ,iceberg-red-1 :weight bold))))
   `(gomoku-X ((t (:foreground ,iceberg-green :weight bold))))

   ;; term
   `(term-color-black ((t :foreground ,iceberg-mono-1)))
   `(term-color-blue ((t (:foreground ,iceberg-blue))))
   `(term-color-cyan ((t :foreground ,iceberg-cyan)))
   `(term-color-green ((t (:foreground ,iceberg-green))))
   `(term-color-magenta ((t :foreground ,iceberg-purple)))
   `(term-color-red ((t :foreground ,iceberg-red-1)))
   `(term-color-white ((t :foreground ,iceberg-fg)))
   `(term-color-yellow ((t (:foreground ,iceberg-orange-1))))

   ;; tabbar
   `(tabbar-default ((,class (:foreground ,iceberg-fg :background ,iceberg-black))))
   `(tabbar-highlight ((,class (:underline t))))
   `(tabbar-button ((,class (:foreground ,iceberg-fg :background ,iceberg-bg))))
   `(tabbar-button-highlight ((,class (:inherit 'tabbar-button :inverse-video t))))
   `(tabbar-modified ((,class (:inherit tabbar-button :foreground ,iceberg-purple :weight light :slant italic))))
   `(tabbar-unselected ((,class (:inherit tabbar-default :foreground ,iceberg-fg :background ,iceberg-black :slant italic :underline nil :box (:line-width 1 :color ,iceberg-bg)))))
   `(tabbar-unselected-modified ((,class (:inherit tabbar-modified :background ,iceberg-black :underline nil :box (:line-width 1 :color ,iceberg-bg)))))
   `(tabbar-selected ((,class (:inherit tabbar-default :foreground ,iceberg-blue :background ,iceberg-bg :weight bold :underline nil :box (:line-width 1 :color ,iceberg-bg)))))
   `(tabbar-selected-modified ((,class (:inherit tabbar-selected :foreground ,iceberg-purple :underline nil :box (:line-width 1 :color ,iceberg-bg)))))

   ;; linum
   `(linum ((t (:foreground ,iceberg-gutter :background ,iceberg-bg))))
   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,iceberg-fg :background ,iceberg-bg))))
   ;; native line numbers (emacs version >=26)
   `(line-number ((t (:foreground ,iceberg-gutter :background ,iceberg-bg))))
   `(line-number-current-line ((t (:foreground ,iceberg-fg :background ,iceberg-bg))))

   ;; regexp-builder
   `(reb-match-0 ((t (:background ,iceberg-gray :foreground ,iceberg-orange-1 :weight semi-bold))))
   `(reb-match-1 ((t (:background ,iceberg-gray :foreground ,iceberg-red-1 :weight semi-bold))))
   `(reb-match-2 ((t (:background ,iceberg-gray :foreground ,iceberg-purple :weight semi-bold))))
   `(reb-match-3 ((t (:background ,iceberg-gray :foreground ,iceberg-orange-2 :weight semi-bold))))

   ;; desktop-entry
   `(desktop-entry-deprecated-keyword-face ((t (:inherit font-lock-warning-face))))
   `(desktop-entry-group-header-face ((t (:inherit font-lock-type-face))))
   `(desktop-entry-locale-face ((t (:inherit font-lock-string-face))))
   `(desktop-entry-unknown-keyword-face ((t (:underline (:color ,iceberg-red-1 :style wave) :inherit font-lock-keyword-face))))
   `(desktop-entry-value-face ((t (:inherit default))))

   ;; latex-mode
   `(font-latex-sectioning-0-face ((t (:foreground ,iceberg-blue :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:foreground ,iceberg-blue :height 1.0))))
   `(font-latex-sectioning-2-face ((t (:foreground ,iceberg-blue :height 1.0))))
   `(font-latex-sectioning-3-face ((t (:foreground ,iceberg-blue :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:foreground ,iceberg-blue :height 1.0))))
   `(font-latex-sectioning-5-face ((t (:foreground ,iceberg-blue :height 1.0))))
   `(font-latex-bold-face ((t (:foreground ,iceberg-green :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,iceberg-green))))
   `(font-latex-warning-face ((t (:foreground ,iceberg-red-1))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,iceberg-cyan))))
   `(font-latex-script-char-face ((t (:foreground ,iceberg-mono-2))))

   ;; org-mode
   `(org-date ((t (:foreground ,iceberg-cyan))))
   `(org-document-info ((t (:foreground ,iceberg-mono-2))))
   `(org-document-info-keyword ((t (:inherit org-meta-line :underline t))))
   `(org-document-title ((t (:weight bold))))
   `(org-footnote ((t (:foreground ,iceberg-cyan))))
   `(org-sexp-date ((t (:foreground ,iceberg-cyan))))

   ;; calendar
   `(diary ((t (:inherit warning))))
   `(holiday ((t (:foreground ,iceberg-green))))

   ;; gud
   `(breakpoint-disabled ((t (:foreground ,iceberg-orange-1))))
   `(breakpoint-enabled ((t (:foreground ,iceberg-red-1 :weight bold))))

   ;; realgud
   `(realgud-overlay-arrow1        ((t (:foreground ,iceberg-green))))
   `(realgud-overlay-arrow3        ((t (:foreground ,iceberg-orange-1))   `(realgud-overlay-arrow2        ((t (:foreground ,iceberg-orange-2))))
                    ))
   '(realgud-bp-enabled-face       ((t (:inherit (error)))))
   `(realgud-bp-disabled-face      ((t (:inherit (secondary-selection)))))
   `(realgud-bp-line-enabled-face  ((t (:box (:color ,iceberg-red-1)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color ,iceberg-gray)))))
   `(realgud-line-number           ((t (:foreground ,iceberg-mono-2))))
   `(realgud-backtrace-number      ((t (:inherit (secondary-selection)))))

   ;; rmsbolt
   `(rmsbolt-current-line-face ((t (:inherit hl-line :weight bold))))

   ;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit ruler-mode-default))))
   `(ruler-mode-comment-column ((t (:foreground ,iceberg-red-1))))
   `(ruler-mode-current-column ((t (:foreground ,iceberg-accent :inherit ruler-mode-default))))
   `(ruler-mode-default ((t (:inherit mode-line))))
   `(ruler-mode-fill-column ((t (:foreground ,iceberg-orange-1 :inherit ruler-mode-default))))
   `(ruler-mode-fringes ((t (:foreground ,iceberg-green :inherit ruler-mode-default))))
   `(ruler-mode-goal-column ((t (:foreground ,iceberg-cyan :inherit ruler-mode-default))))
   `(ruler-mode-margins ((t (:inherit ruler-mode-default))))
   `(ruler-mode-tab-stop ((t (:foreground ,iceberg-mono-3 :inherit ruler-mode-default))))

   ;; undo-tree
   `(undo-tree-visualizer-current-face ((t (:foreground ,iceberg-red-1))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,iceberg-orange-1))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,iceberg-cyan))))

   ))

(iceberg-with-color-variables
  (custom-theme-set-variables
   'iceberg
   ;; fill-column-indicator
   `(fci-rule-color ,iceberg-gray)

   ;; tetris
   ;; | Tetromino | Color                    |
   ;; | O         | `iceberg-orange-2' |
   ;; | J         | `iceberg-blue'     |
   ;; | L         | `iceberg-orange-1' |
   ;; | Z         | `iceberg-red-1'    |
   ;; | S         | `iceberg-green'    |
   ;; | T         | `iceberg-purple'   |
   ;; | I         | `iceberg-cyan'     |
   '(tetris-x-colors
     [[229 192 123] [97 175 239] [209 154 102] [224 108 117] [152 195 121] [198 120 221] [86 182 194]])

   ;; ansi-color
   `(ansi-color-names-vector
     [,iceberg-black ,iceberg-red-1 ,iceberg-green ,iceberg-orange-2
             ,iceberg-blue ,iceberg-purple ,iceberg-cyan ,iceberg-fg])
   ))

(defvar iceberg-theme-force-faces-for-mode t)

(defun iceberg-theme-change-faces-for-mode ()
  (interactive)
  (when (or iceberg-theme-force-faces-for-mode (called-interactively-p))
    (iceberg-with-color-variables
      (cond
       ((member major-mode '(js2-mode))
    (face-remap-add-relative 'font-lock-constant-face :foreground iceberg-orange-1)
    (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))
    (face-remap-add-relative 'font-lock-variable-name-face :foreground iceberg-mono-1))
       ((member major-mode '(html-mode))
    (face-remap-add-relative 'font-lock-function-name-face :foreground iceberg-red-1)
    (face-remap-add-relative 'font-lock-variable-name-face :foreground iceberg-orange-1))))))

(add-hook 'after-change-major-mode-hook 'iceberg-theme-change-faces-for-mode)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
          (file-name-as-directory
           (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'iceberg)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; iceberg-theme.el ends here
