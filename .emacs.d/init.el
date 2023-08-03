;;; ----------------------------------------------------------------------------
;;; パッケージの設定
;;; ----------------------------------------------------------------------------
(require 'package)

;; リポジトリ
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; パッケージの初期化
(package-initialize)

;; パッケージ情報を更新する
(unless package-archive-contents (package-refresh-contents))

;; use-packageのインストール
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

;;; ----------------------------------------------------------------------------
;;; 基本的な設定
;;; ----------------------------------------------------------------------------

(setq tramp-default-method "scp")

;; カレントディレクトリをホームディレクトリに設定
(cd "~/")

;; 範囲を選択してCtrl+dまたはDeleteを押下すると選択した範囲をキルリングに入れずに削除する
(delete-selection-mode t)

(setq resize-mini-windows 'grow-only)

;; 起動時のメッセージを非表示にする
(setq inhibit-startup-message t)

;;; 行番号を表示する
;; 行番号の書式
(setq linum-format "%03d")

;; 行番号表示の有無
;(global-linum-mode t)

;; タブサイズ
(setq-default tab-width 4 indent-tabs-mode nil)

;; 列番号を表示する
(column-number-mode t)

;; Ctrl+h でバックスペース
(global-set-key "\C-h" 'delete-backward-char)

;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;; .# とかのロックファイルを作らない
(setq create-lockfiles nil)

;;; スクロールを一行ずつにする
;;(setq scroll-step 1)

;;; カーソル位置をなるべく中央にするように調整
(setq scroll-conservatively 0)
(setq scroll-preserve-screen-position t)
(setq scroll-margin 10)
(setq next-screen-context-lines 10)

;; メニューバーをなくす
(menu-bar-mode -1)

;; 行末の空白文字を可視化する
;;(setq-default show-trailing-whitespace t)

;; 保存時に末尾の空白を取り除く
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 行を折り返さない
(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)

;; タイトルにフルパス表示
(setq frame-title-format "%f")

;; ヘッダーラインにファイルのフルパスを表示
(setq header-line-format "%f")

;; 閉じ括弧を自動挿入する
;;(electric-pair-mode 1)

; 自動インデント(-1で無効)
;;(electric-indent-mode -1)

;;複数ウィンドウを開かないようにする
(setq ns-pop-up-frames nil)

;; シンボリックリンクの読み込みを許可
(setq vc-follow-symlinks t)

;; シンボリックリンク先のVCS内で更新が入った場合にバッファを自動更新
(setq auto-revert-check-vc-info t)

;; バッファの内容が更新されている場合は自動的に更新する
(global-auto-revert-mode t)

;; dired-find-alternate-file の有効化
;; この機能を有効化するとdiredでディレクトリ移動や
;; ファイルを開く際に新たなバッファは作られず、
;; 自バッファで開くようになる。
;; この機能をRETURNキーに適用する設定
(put 'dired-find-alternate-file 'disabled nil)
(eval-after-load "dired" '(progn
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ))

;;; 環境を日本語、UTF-8にする
;; UTF-8の文字列を貼り付けると文字化けする事象が発生した。
;; それでこの設定を入れたら直った。
(setenv "LANG" "ja_JP.UTF-8")

;; ローカル環境の設定
;; set-locale-environment関数は通常、システムメッセージをデコードするために、
;; 言語環境により確立された、優先されるコーディングシステムを使用します。
;; しかしlocaleが変数locale-preferred-coding-systemsのエントリーに
;; マッチした場合、Emacsはかわりに対応するコーディングシステムを使用します。
;; たとえばlocaleの‘ja_JP.PCK’が、locale-preferred-coding-systemsのjapanese-shift-jisにマッチした場合

;; Emacsは通常ならjapanese-iso-8bitが使われるような場合でも、エンコーディングにそのコーディングシステムを使用します。
(set-locale-environment nil)

;; 言語環境の設定
(set-language-environment "Japanese")
(setq menu-tree-coding-system 'utf-8)

;; 端末出力のためのコーディングシステムを指定
(set-terminal-coding-system 'utf-8)

;; 端末出力のためのコーディングシステムを指定します。
;; 端末出力の文字コードを指定した場合、端末へのすべての文字出力は、
;; 指定したコーディングシステムに変換されます。
(set-keyboard-coding-system 'utf-8)

;; カレントバッファーのファイルのコーディングシステムをセット
(set-buffer-file-coding-system 'utf-8)

;; デフォルトの文字コードを設定
(prefer-coding-system 'utf-8)

;;; Mac専用設定
(when (eq system-type 'darwin)
  (progn
    ;; クリップボードの共有設定
    (defun copy-from-osx ()
      (shell-command-to-string "pbpaste"))
    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)
    ))

;;; 日付挿入
(defun insert-current-time()
  (interactive)
  (insert (format-time-string "%Y-%m-%d(%a) %H:%M:%S" (current-time))))
(define-key global-map (kbd "C-c d") 'insert-current-time)

(use-package visual-regexp-steroids
  :ensure t
  )

;; .logファイルでは自動で auto-revert-tail-mode にする
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; ファイル更新やバッファオープン時、自動でカーソルを末尾に移動する
(defun do-end-of-buffer()
  (when auto-revert-tail-mode
    (end-of-buffer)))
(add-hook 'after-revert-hook 'do-end-of-buffer)
(add-hook 'find-file-hook 'do-end-of-buffer)

;;; white space
(use-package whitespace

  ;; インストールされていなければ自動インストールする
  :ensure t

  ;; ライブラリの設定
  :config
  (set-face-foreground 'whitespace-space nil) ;空白(前景色)
  (set-face-background 'whitespace-space "gray33") ;空白(背景色)
  (set-face-background 'whitespace-empty "gray33") ;ファイル先頭と末尾の空行
  (set-face-foreground 'whitespace-tab nil) ;タブ(前景色)
  (set-face-background 'whitespace-tab "gray33") ;タブ(背景色)
  (set-face-background 'whitespace-trailing "gray33")
  (set-face-background 'whitespace-hspace "gray33")
  (setq whitespace-style '(face trailing tabs emptyspaces tab-mark))
  (setq whitespace-display-mappings '((tab-mark ?\t [?\xBB ?\t]))) ; タブの表示を変更

  :init
  (global-whitespace-mode 1)
  )

;;; パッケージ：Golden-ratio ウィンドウサイズ自動調整
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

;;; パッケージ：ダッシュボード
(use-package dashboard

  ;; インストールされていなければ自動インストールする
  :ensure t

  ;; ライブラリの設定
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 30))) ; 最近開いたアイテムを表示する数
  )

;;; パッケージ：ファイルの自動保存
(use-package auto-save-buffers-enhanced

  :ensure t

  ;; ライブラリの設定
  :config
  (setq auto-save-buffers-enhanced-interval 0.5) ; 1秒たったら自動保存する
  (setq auto-save-buffers-enhanced-quiet-save-p t) ; Wroteのメッセージを抑制する
  (auto-save-buffers-enhanced t) ; 自動保存 nil=無効 t=有効
  )

;;; パッケージ：eww (WEBブラウザ)
(use-package eww

  ;; インストールされていなければ自動インストールする
  :ensure t

  ;; :config キーワードについて
  ;; 当該のパッケージが読み込まれた後に評価されます。
  ;; initに比べ、configは後述の遅延ロードを設定した際にパッケージがロードされるまで評価されなくなります。
  :config
  (setq eww-search-prefix "https://www.google.co.jp/search?q=") ; 検索エンジンをgoogleに変更する
  )

;;; パッケージ：helm
;;; HelmはEmacsにおける様々なものを検索するための統一的な
;;; インターフェイスを提供してくれる非常に大きなパッケージです
(use-package helm

  ;; インストールされていなければ自動インストールする
  :ensure t

  ;; キーの割り当てを指定
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         :map helm-map
         ("TAB" . helm-execute-persistent-action) ; TABキーで補完が効くようにする
         ("C-i" . helm-execute-persistent-action) ;; C-iはタブ。
         ("C-z" . helm-select-action)
         ("C-h" . delete-backward-char)) ; Ctrl+hでバックスペースが効くようにする

  ;; ライブラリの設定
  :config
  (helm-mode 1))

;;; パッケージ：org
(use-package org

  ;; インストールされていなければ自動インストールする
  :ensure t

  ;; ライブラリの設定
  :config

  :custom
  (org-startup-indented t) ; 見出しをインデントする
  (org-indent-mode-turns-on-hiding-stars nil) ; 見出しをインデントした時にアスタリスクが減るのを防ぐ
  (org-indent-indentation-per-level 4); インデントの幅を設定
  (org-startup-folded 'showall)); 見出しの初期状態

;;; パッケージ：geben
(use-package geben

  ;; インストールされていなければ自動インストールする
  :ensure t
  )

;;; パッケージ：git操作パッケージ
(use-package magit

  ;; インストールされていなければ自動インストールする
  :ensure t

  :bind
  ("C-x g" . magit-status))

;;; パッケージ： LSP
(use-package lsp-mode

  ;; インストールされていなければ自動インストールする
  :ensure t

  :custom
  (lsp-enable-snippet t) ;スニペット
  (lsp-enable-indentation nil)
  (lsp-prefer-flymake nil)
  (lsp-document-sync-method 2)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (create-lockfiles nil)
  (lsp-prefer-capf t)

  :commands
  (lsp lsp-deferred)

  :hook
  (php-mode . lsp-deferred))

;;; パッケージ： LSP-UI
(use-package lsp-ui

  ;; インストールされていなければ自動インストールする
  :ensure t

  :after lsp-mode

  :custom

  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width  60)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)

  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable t)

  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t) ; ホバーメッセージをサイドラインに表示する
  (lsp-ui-sideline-show-diagnostics t) ; 診断メッセージをサイドラインに表示する
  (lsp-ui-sideline-show-code-actions t) ; サイドラインでコードアクションを表示する

  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t) ; LSP imenu の有効化
  (lsp-ui-imenu-window-width 100) ; ウィンドウ幅を設定する

  ;; lsp-ui-peek
  (lsp-ui-peek-enable t) ; ui-peek を有効化する
  (lsp-ui-peek-always-show t) ; 候補が一つでも、常にpeek表示する。
  (lsp-ui-peek-peek-height 30)
  (lsp-ui-peek-list-width 100)
  (lsp-ui-peek-fontify 'always)

  :bind
  (:map lsp-ui-mode-map
        ;; デフォルトの xref-find-definitions だと、ジャンプはできるが、ui-peek が使えない。
        ("M-." . lsp-ui-peek-find-definitions)

        ;; デフォルトの xref-find-references だと、ジャンプはできるが、ui-peek が使えない。
        ("M-?" . lsp-ui-peek-find-references)
        )
  :hook
  (lsp-mode . lsp-ui-mode)
)

;;; パッケージ：company
(use-package company

  ;; インストールされていなければ自動インストールする
  :ensure t

  :custom
  (company-transformers '(company-sort-by-backend-importance))
  (company-idle-delay 0) ; 遅延なしにする。
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る。
  (completion-ignore-case t)
  (company-show-numbers t) ; 番号を表示する。

  :init
  (global-company-mode t)

  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-s" . company-filter-candidates)
              ("C-h" . backward-delete-char)
              ("<tab>" . company-complete-selection))
  :bind (:map company-search-map
              ("C-n" . company-select-next)
              ("C-h" . backward-delete-char)
              ("C-p" . company-select-previous)))

;;; php
(use-package php-mode

  ;; インストールされていなければ自動インストールする
  :ensure t

  ;; no-requireについて
  ;; 通常、use-packageは設定をコンパイルする前に、コンパイル時に各パッケージをロードし、
  ;; バイトコンパイラを満足させるために必要なシンボルがスコープ内にあることを確認します。これが問題になることがあります。
  ;; パッケージが特別なロード要件を持っていて、use-packageを使いたいのはeval-after-loadフックに設定を追加したいだけだからです。
  ;; このような場合には :no-require キーワードを使用してください。
  :no-require t

  ;; deferキーワードについて
  ;; commands, :bind, :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter, :hook
  ;; (これらはすべて :defer を意味します。
  ;; それぞれの簡単な説明は use-package の docstring を参照してください)を使用していない場合は、次のことができます。
  ;; 読み込みを延期するには、:deferキーワードが必要です。
  :defer t

  ;; ;; :init について
  ;; ;; パッケージが読み込まれる前に評価されます。遅延キーワードの影響を受けません。
  ;; :init
  ;; (add-hook 'php-mode-hook
  ;;       '(lambda()
  ;;          ;; オートコンプリートモードを有効にする
  ;;          (auto-complete-mode t)
  ;;          (setq tab-width 4)
  ;;          ;; indent_tabs-mode
  ;;          ;; ・全てのインデントにタブを使う場合は t を入れる
  ;;          ;; ・全てのインデントにスペースのみを使う場合は nil を入れる
  ;;          (setq indent-tabs-mode nil)))

  ;; :custom キーワード
  ;; パッケージの設定を書くキーワードです、(setq ~)と書いているのと同じことです。
  :custom
  ;; 参照するPHPマニュアルの言語を日本語にする
  (php-manual-url 'ja)

  ;; php-mode-coding-styleについて
  ;; PHPのコーディングスタイルを設定する
  ;;
  ;; psr2について
  ;; PSR-2は、PHPにおける基本的なコーディング標準であるPSR-1を拡張し、
  ;; PHPコードをどのようにフォーマットするかについての共通の規則を定めています。 
  (php-mode-coding-style 'psr2))

;; ;; helm-gtags ...「GNU global」のhelmインタフェース。
;; ;; 「GNU global」... ソースコードに索引付けを行うことで、大規模システムのハックやレビューを効率化するソフトウエア。
;; (use-package helm-gtags
;;   ;; ensure について
;;   ;; package.el が既にパッケージのリストを取得し終わっている場合、
;;   ;; ensure キーワードは「既に取得されているパッケージのリストの情報をもとに」
;;   ;; パッケージのダウンロードを行おうとします。
;;   :ensure t
;;   :init
;;   ;; add-hook ... フックを追加する。
;;   ;; hook：フックとは、引っ掛けておくフックのことで、決められたイベント (例えば、ファイルを保存するときとか、モードを変更したときなど) に
;;   ;; 予め関数をセットしておくことで、イベント発生のタイミングで関数を実行させることができる仕組みです。
;;   (add-hook 'helm-gtags-mode-hook
;;     '(lambda()
;;        ;;入力されたタグの定義元へジャンプ
;;        (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
;;        ;;入力タグを参照する場所へジャンプ
;;        (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
;;        ;;入力したシンボルを参照する場所へジャンプ
;;        (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
;;       ;;タグ一覧からタグを選択し, その定義元にジャンプする
;;        (local-set-key (kbd "M-l") 'helm-gtags-select)
;;        ;;ジャンプ前の場所に戻る
;;        (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))
;;   (add-hook 'php-mode-hook 'helm-gtags-mode)
;;   )

;;; パッケージ：flycheck
;;; flycheckは、on-the-flyで(その場で)書式チェックを行うもの
(use-package flycheck

  ;; インストールされていなければ自動インストールする
  :ensure t

  :init
  ;; デフォルトでflycheckを有効にする
  (global-flycheck-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(package-selected-packages
   '(visual-regexp-steroids lsp-mode atom-one-dark-theme use-package tron-legacy-theme tao-theme magit helm-gtags geben flycheck dracula-theme dashboard auto-save-buffers-enhanced ac-php)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "color-34"))))
 '(lsp-headerline-breadcrumb-path-face ((t (:foreground "black"))))
 '(lsp-headerline-breadcrumb-project-prefix-face ((t (:foreground "black" :weight bold))))
 '(lsp-headerline-breadcrumb-separator-face ((t (:foreground "black" :height 0.8))))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t (:foreground "black" :underline (:color "Red1" :style wave)))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:foreground "black" :weight bold))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t (:foreground "black" :underline (:color "Green" :style wave))))))
