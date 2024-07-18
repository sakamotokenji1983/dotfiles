;;; Copyright (c) 2024 SAKAMOTO Kenji
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

(require 'package)
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

;; iceberg
(add-to-list 'custom-theme-load-path "~/.emacs.d/theme/iceberg/")
(load-theme 'iceberg t)

;; dracula
(use-package darcula-theme
  :ensure t)

;;; ----------------------------------------------------------------------------
;;; 基本的な設定
;;; ----------------------------------------------------------------------------

;; 起動時に最大化する
;; Start Emacs in fullscreen mode
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;(setq tramp-default-method "scp")

;; フォントの設定
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 150)  ;; 14ptのフォントサイズを設定

;; ショートカットの設定
(when (equal window-system 'mac)
  (setq mac-function-modifier 'meta)  ; macosのfunctionキーをmetaキーに置き換える
  (setq mac-option-modifier 'meta)    ; macosのoptionキーをmetaキーに置き換える
  (setq mac-command-modifier 'super)  ; macosのcommandキーをsuperキーに置き換える
  (global-set-key (kbd "s-x") 'kill-region)        ; Command+X で選択したテキストを削除してキルリングに保存する
  (global-set-key (kbd "s-c") 'kill-ring-save)     ; Command+C で選択したテキストを削除せずにキルリングに保存する
  (global-set-key (kbd "s-v") 'yank)               ; Command+V でキルリングからテキストを貼り付ける
  (global-set-key (kbd "s-a") 'mark-whole-buffer)  ; Command+A でバッファ内の全てのテキストを保存する
  (global-set-key (kbd "s-s") 'save-buffer)        ; Command+S でバッファを保存する
  (global-set-key (kbd "s-z") 'undo)               ; Command+X で一つ前の状態に戻す
  (global-set-key (kbd "s-+") 'text-scale-adjust)  ; Command++ でテキストの表示倍率を変更する
  (global-set-key (kbd "s--") 'text-scale-adjust)) ; Command+- でテキストの表示倍率を変更する

;; カレントディレクトリをホームディレクトリに設定
(cd "~/")

;; 範囲を選択してCtrl+dまたはDeleteを押下すると選択した範囲をキルリングに入れずに削除する
(delete-selection-mode t)

(setq resize-mini-windows 'grow-only)

;; 起動時のメッセージを非表示にする
(setq inhibit-startup-message t)

;; 行番号を表示する
(global-display-line-numbers-mode)

;; タブサイズ
(setq-default tab-width 4 indent-tabs-mode nil)

;; 列番号を表示する
(column-number-mode t)

;; Ctrl+h でバックスペース
(global-set-key "\C-h" 'delete-backward-char)

;; バックアップファイルを作成しない
(setq make-backup-files nil) ; *.~ とかのバックアップファイルを作らない
(setq backup-inhibited nil)
(setq create-lockfiles nil) ; .# とかのロックファイルを作らない

;; ビープ音を無効化
(setq ring-bell-function 'ignore)

(setq isearch-wrap-pause nil) ; インクリメンタルサーチを循環させる

;; 縦のスクロールバーを非表示にする
(scroll-bar-mode -1)

;; スクロール時にのみスクロールバーを表示する
(add-hook 'scroll-bar-mode-hook
          (lambda ()
            (when scroll-bar-mode
              (set-scroll-bar-mode 'right))))

;;; スクロールを一行ずつにする
;;(setq scroll-step 1)

;;; カーソル位置をなるべく中央にするように調整
;(setq scroll-conservatively 0)
;(setq scroll-preserve-screen-position t)
;(setq scroll-margin 10)
;(setq next-screen-context-lines 10)

;; メニューバーをなくす
(menu-bar-mode -1)

;; ツールバーをなくす
(tool-bar-mode -1)

;; 行末の空白文字を可視化する
(setq-default show-trailing-whitespace t)

;; 保存時に末尾の空白を取り除く
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 行を折り返さない
(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)

;; タイトルにフルパス表示
(setq frame-title-format "%f")

;; ヘッダーラインにファイルのフルパスを表示
;;(setq header-line-format "%f")

;; 閉じ括弧を自動挿入する
;;(electric-pair-mode 1)

; 自動インデント(-1で無効)
;;(electric-indent-mode -1)

;;複数ウィンドウを開かないようにする
(setq ns-pop-up-frames nil)

;; シンボリックリンクの読み込みを許可
(setq vc-follow-symlinks t)

;;; 自動リロードの設定
(global-auto-revert-mode 1) ;; 全てのモードで自動リロードを有効にする
(setq auto-revert-interval 10) ;; 10秒ごとにリロード
(setq auto-revert-check-vc-info t) ;; シンボリックリンク先のVCS内で更新が入った場合にバッファを自動更新
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode)) ;; .logファイルでは自動で auto-revert-tail-mode にする

;;; dired-find-alternate-file の有効化
;;; この機能を有効化するとdiredでディレクトリ移動や
;;; ファイルを開く際に新たなバッファは作られず、
;;; 自バッファで開くようになる。
;;; この機能をRETURNキーに適用する設定
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
(set-language-environment "Japanese") ; 日本語環境を設定
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

;;; 正規表現をインタラクティブに置換するためのツール。
;;; 標準のEmacsの正規表現置換コマンド(query-replace-regexp)を強化し、
;;; より視覚的で使いやすいインターフェースを提供する。
(use-package visual-regexp-steroids
  :ensure t)

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

;;; パッケージ：ダッシュボード
(use-package dashboard
  :ensure t ;; インストールされていなければ自動インストールする
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 30)))) ; 最近開いたアイテムを表示する数

;;; パッケージ：ファイルの自動保存
(use-package auto-save-buffers-enhanced
  :ensure t
  :config
  (setq auto-save-buffers-enhanced-interval 0.5) ; 1秒たったら自動保存する
  (setq auto-save-buffers-enhanced-quiet-save-p t) ; Wroteのメッセージを抑制する
  (auto-save-buffers-enhanced t)) ; 自動保存 nil=無効 t=有効

;;; パッケージ：org
(use-package org
  :ensure t ;; インストールされていなければ自動インストールする
  ;; ライブラリの設定
  :config
  :custom
  (org-startup-indented t)                    ; 見出しをインデントする
  (org-indent-mode-turns-on-hiding-stars nil) ; 見出しをインデントした時にアスタリスクが減るのを防ぐ
  (org-indent-indentation-per-level 4)        ; インデントの幅を設定
  (org-startup-folded 'showall))              ; 見出しの初期状態

;;; php
(use-package php-mode
  :ensure t ;; インストールされていなければ自動インストールする

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

;;; パッケージ：flycheck
;;; flycheckは、on-the-flyで(その場で)書式チェックを行うもの
(use-package flycheck
  :ensure t ;; インストールされていなければ自動インストールする
  :init
  ;; デフォルトでflycheckを有効にする
  (global-flycheck-mode))

(use-package nerd-icons
  :ensure t)
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(nerd-icons visual-regexp-steroids vertico use-package treesit-auto php-mode orderless magit lsp-ui helm golden-ratio geben flycheck dashboard darcula-theme corfu company auto-save-buffers-enhanced)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
