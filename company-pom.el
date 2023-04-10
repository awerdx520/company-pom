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

(defcustom company-pom-ignored-directories '(".cache" )
  "忽略不检索的文件夹." )

(defcustom company-pom-groupId-completion-type 'split
  "GroupId 补全方式.
'split 根据 '.' 将前缀拆分，然后检查目录下时否存在文件。
'cache 缓存本地所有的库，统一过滤（如果本地库太多，会很卡）。" )

(defvar company-pom-groupId-cache nil
  "本地 Maven 仓库 groupId 缓存 .
由于本地检索的时间比较长，所以在 `company-pom` 启动时
将 group Id 相关信息缓存上，加快补全速度。这个缓存将
会在固定时间自动更新或者手动更新。")

(defvar company-pom-tag-regex
  "<[[:alpha:]]+>[[:space:]]*\\([a-zA-Z0-9-_.]*\\)[[:space:]]*</[[:alpha:]]+>[\n \t]*"
  "标签匹配正则表达式.")

(defvar company-pom-prefix-regex  "<\\([[:alpha:]]+\\)>\\([a-zA-Z0-9-_. ]*\\)"
  "当前 `point' 前缀+标签正则表达式.")


(defvar company-pom-dependency-items-regexp
  "<dependency>[ \n\t]*<groupId>\\([a-zA-Z0-9-_. ]*\\)</groupId>[ \n\t]*<artifactId>\\([a-zA-Z0-9-_. ]*\\)</artifactId>[ \n\t]*<version>\\([a-zA-Z0-9-_. ]*\\)</version>[ \n\t]*</dependency>"
  "Maven Dependency 标签每个子标签值正则表达式.")

(defun company-pom--resolve-existing-dependency ()
  "解析当前 pom.xml 文件已经配置的 dependencies."
  (let ((matches '())
        (pos (point)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward company-pom-dependency-items-regexp nil t)
        (when (or (> pos (match-end 0)) (< pos (match-beginning 0)))
          (push (s-join "|" (list (match-string-no-properties 1)
                                  (match-string-no-properties 2)
                                  (match-string-no-properties 3))) matches))))
    matches))

(defconst company-pom--artifactId-plus-version-plus-pom-file-regex "\\(\\/[^/]*\\)\\{3\\}\\.pom$"
  "确定一个库的位置的正则表达式.")

(defun company-pom--groupId-with-split(prefix)
  "将 `PREFIX' 根据 '.' 分成不同的目录匹配项来匹配 groupId."
  (let ((root (f-full company-pom-maven-user-local-repository))
        (path (apply #'f-join (cons "/" (cons (f-full company-pom-maven-user-local-repository)
                                              (s-split "\\." prefix 'omit-nulls))))))
    (if (not (s-blank? prefix))
        (--map (s-replace "/" "." (s-chop-prefix root it))
               (f-directories
                (if (not (s-suffix? "." prefix))
                    (f-parent path)
                  path)
                (lambda (dir) (not (-contains? company-pom-ignored-directories (f-filename dir))))))
      (f-uniquify (f-directories root)))))

(defun company-pom--search-groupId ()
  "搜索本地仓库中的 groupId.
本地仓库位置可以通过 `comapny-pom-maven-user-local-repository`值设置。"
  (delete-dups
   (--map (s-replace "/" "." (s-left (car (car (s-matched-positions-all company-pom--artifactId-plus-version-plus-pom-file-regex it))) it))
          (--map (s-chop-left (length (f-full company-pom-maven-user-local-repository)) it)
                 (f-files company-pom-maven-user-local-repository  (lambda (pom-file) (and (s-matches? "\\.pom$" pom-file)
                                                                                           (not (s-contains? ".cache" pom-file)))) t)))))

(defun company-pom--update-groupId-cache-timer ()
  "GroupId 缓存更新定时任务."
  (when company-pom-local-groupId-cache-switch
    (run-with-idle-timer 0 company-pom-local-groupId-cache-interval
                         (lambda ()
                           (setq company-pom-groupId-cache (company-pom--search-groupId))))))


(defun company-pom--dependency-entries (&optional groupId artifactId)
  "根据 `GROUPID' + `ARTIFACTID' 获取候选结果."
  (if (not (s-blank? groupId))
      (let ((parent (concat company-pom-maven-user-local-repository (s-replace "."  "/" groupId) "/" artifactId)))
        (message "%s" parent)
        (when (f-exists-p parent)
          (f-uniquify (f-directories parent))))
    (if (or company-pom-groupId-cache (null company-pom-groupId-cache))
        (setq company-pom-groupId-cache (company-pom--search-groupId))
      company-pom-groupId-cache)))

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
                 (if (eq company-pom-groupId-completion-type 'split)
                     (company-pom--groupId-with-split prefix)
                   (company-pom--dependency-entries)))

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
