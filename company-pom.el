;;; company-pom.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 ThomasDon
;;
;; Author: ThomasDon <awerdx520@gmail.com>
;; Maintainer: ThomasDon <awerdx520@gmail.com>
;; Created: 四月 03, 2023
;; Modified: 四月 03, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/company-pom
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary: 对 Maven 项目中的 pom.xml 进行补全
;;
;;; Code:

(require 'company)
(require 'f)
(require 's)
(require 'dash)

(when (not (>= emacs-version  "28.0"))
  (error "当前 Emacs (%s) 版本过低，最低支持版本 28.0" emacs-version))

(defgroup company-pom nil
  "Completion backend for Pom.xml."
  :group 'company)

(defcustom company-pom-maven-user-local-repository "~/.m2/repository/"
  "Maven 本地仓库路径."
  :type 'string
  :group 'company-pom)

(defcustom company-pom-local-groupId-cache-switch nil
  "GroupId 缓存定时更新功能开启开关."
  :type 'bool
  :group 'company-pom)

(defcustom company-pom-local-groupId-cache-interval 300
  "GroupId 缓存定时更新间隔 (unit/s)."
  :type 'integer
  :group 'company-pom)

(defvar company-pom-groupId-cache nil
  "本地 Maven 仓库 groupId 缓存 .
由于本地检索的时间比较长，所以在 `company-pom` 启动时
将 group Id 相关信息缓存上，加快补全速度。这个缓存将
会在固定时间自动更新或者手动更新。")

(defun company-pom--search-groupId ()
  "搜索本地仓库中的 groupId.
本地仓库位置可以通过 `comapny-pom-maven-user-local-repository`值设置。"
  (delete-dups
   (mapcar (lambda (group-id)
             (string-replace "/" "."
                             (s-left
                              (car (car (s-matched-positions-all "\\(\\/[^/]*\\)\\{3\\}\\.pom$" group-id))) group-id)))
           (mapcar (lambda (group-dir)
                     (s-chop-left (length (f-full company-pom-maven-user-local-repository))
                                  group-dir))
                   (f-files company-pom-maven-user-local-repository  (lambda (pom-file) (and (s-matches? "\\.pom$" pom-file)
                                                                                             (not (s-contains? ".cache" pom-file)))) t)))))
(defun company-pom--update-groupId-cache-timer ()
  "GroupId 缓存更新定时任务."
  (when company-pom-local-groupId-cache-switch
    (run-with-idle-timer 0 company-pom-local-groupId-cache-interval
                         (lambda ()
                           (setq company-pom-groupId-cache (company-pom--search-groupId)) ))))

(defun company-pom-update-groupId-cache ()
  "更新 Maven GroupId 缓存."
  (interactive)
  (setq company-pom-groupId-cache (company-pom--search-groupId)))

(defun company-pom--directories (parent)
  "根据 `PARENT' 目录搜索子目录."
  (when (f-exists-p parent)
    (f-uniquify (f-directories parent))))

(defun company-pom--get-artifactId-candidates (groupId)
  "搜索本地仓库中对应的 GROUPID 的 artifactId.
本地仓库位置可以通过 `comapny-pom-maven-user-local-repository' 值设置。"
  (company-pom--directories
   (concat company-pom-maven-user-local-repository (string-replace "." "/" groupId))))

(defun company-pom--get-version-candidates (groupId artifactId)
  "搜索本地仓库中 `GROUPID' + `ARTIFACTID' 对应库版本.
本地仓库位置可以通过 `comapny-pom-maven-user-local-repository' 值设置。"
  (company-pom--directories
   (concat company-pom-maven-user-local-repository (string-replace "." "/" groupId) "/" artifactId)))

(defcustom company-pom-candidates-match-model 'prefix
  "`company-pom' 候选结果搜索模式.
'split 根据 . 号拆分，每个 segment 匹配 GroupId.
'prefix 精确匹配前缀， 默认匹配模式。"
  :type 'symbol
  :group 'company-pom)

(defun company-pom--get-groupId-candidates (prompt)
  "根据 `PROMPT' 获取 GroupId 候选结果.
具体匹配模式可以参见`company-pom-candidates-match-model' 变量"
  (unless company-pom-groupId-cache
    (setq company-pom-groupId-cache (company-pom--search-groupId)))
  (-filter (lambda (groupId)
             (cond ((equal company-pom-candidates-match-model 'split)
                    (--some (string-match-p it groupId)
                            (if (s-contains-p "." prompt)
                                (s-split "\\." prompt 'omit-nulls)
                              (list prompt))))
                   (t (s-prefix? prompt groupId))))
           company-pom-groupId-cache))


(provide 'company-pom)
;;; company-pom.el ends here
