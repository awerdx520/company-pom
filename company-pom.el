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

(require 'f)
(require 's)
(require 'dash)
(require 'company)

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
             (s-replace "/" "."
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

(defun company-pom--dependency-entries (&optional groupId artifactId)
  "根据 `GROUPID' + `ARTIFACTID' 获取候选结果."
  (if (not (s-blank? groupId))
      (let ((parent (concat company-pom-maven-user-local-repository (s-replace "."  "/" groupId) "/" artifactId)))
        (message "%s" parent)
        (when (f-exists-p parent)
          (f-uniquify (f-directories parent))))
    (if (or company-pom-groupId-cache (seq-empty-p company-pom-groupId-cache))
        (setq company-pom-groupId-cache (company-pom--search-groupId))
      company-pom-groupId-cache)))

(defvar company-pom-tag-regex
  "<[[:alpha:]]+>[[:space:]]*\\([a-zA-Z0-9-_.]*\\)[[:space:]]*</[[:alpha:]]+>[\n \t]*"
  "标签匹配正则表达式.")

(defvar company-pom-prefix-regex  "<\\([[:alpha:]]+\\)>\\([a-zA-Z0-9-_. ]*\\)"
  "当前 `point' 前缀+标签正则表达式.")

(defun company-pom--grab-dependency-prefix ()
  "获取当前位置的前缀 ."
  (when (looking-back company-pom-prefix-regex)
    (match-string-no-properties 2)))


(defun company-pom--dependency-cnadidates (prefix)
  "根据 `PREFIX' 用于 `company-mode' 获取依赖候选项."
  (let* ((tag (when (looking-back company-pom-prefix-regex) (match-string-no-properties 1))))
    (--filter (s-prefix? prefix it)
              (pcase tag
                ("groupId"
                 (company-pom--dependency-entries))
                ("artifactId"
                 (when (looking-back (concat company-pom-tag-regex company-pom-prefix-regex))
                   (company-pom--dependency-entries
                    (match-string-no-properties 1))))
                ("version"
                 (when (looking-back
                        (concat company-pom-tag-regex
                                company-pom-tag-regex
                                company-pom-prefix-regex))
                   (company-pom--dependency-entries
                    (match-string-no-properties 1)
                    (match-string-no-properties 2))))
                (_ nil)))))

;;;###autoload
(defun company-pom (command &optional arg &rest ignored)
  "pom.xm 构建文件补全 backend."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-pom))
    (prefix (company-pom--grab-dependency-prefix))
    (candidates
     (company-pom--dependency-cnadidates arg))))

;;;###autoload
(defun company-pom-update-groupId-cache ()
  "更新 Maven GroupId 缓存."
  (interactive)
  (setq company-pom-groupId-cache (company-pom--search-groupId)))

(provide 'company-pom)
;;; company-pom.el ends here
