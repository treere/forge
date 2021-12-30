;;; forge-gitea.el --- Gitea support              -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Forge is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Forge is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Forge.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'gtea)
(require 'forge)

;;; Class

(defclass forge-gitea-repository (forge-unusedapi-repository)
  ((issues-url-format         :initform "https://%h/%o/%n/issues")
   (issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   ;; The anchor for the issue itself is .../%i#issue-%i
   (issue-post-url-format     :initform "https://%h/%o/%n/issues/%i#issuecomment-%I")
   (pullreqs-url-format       :initform "https://%h/%o/%n/pulls")
   (pullreq-url-format        :initform "https://%h/%o/%n/pulls/%i")
   (pullreq-post-url-format   :initform "https://%h/%o/%n/pulls/%i#issuecomment-%I")
   (commit-url-format         :initform "https://%h/%o/%n/commit/%r")
   (branch-url-format         :initform "https://%h/%o/%n/commits/branch/%r")
   (remote-url-format         :initform "https://%h/%o/%n")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/pulls") ; sic
   (pullreq-refspec :initform "+refs/pull/*/head:refs/pullreqs/*")))

(cl-defmethod forge--pull ((repo forge-gitea-repository) until)
  (let ((cb (let ((buf (and (derived-mode-p 'magit-mode)
                            (current-buffer)))
                  (dir default-directory)
                  (val nil))
              (lambda (cb &optional v)
                (when v (if val (push v val) (setq val v)))
                (let-alist val
                  (cond
                   ((not val)
                    (forge--fetch-repository repo cb))
                   ((not (assq 'assignees val))
                    (forge--fetch-assignees repo cb))
                   ((not (assq 'forks val))
                    (forge--fetch-forks repo cb))
                   ((not (assq 'labels val))
                    (forge--fetch-labels repo cb))
                   ((and .has_issues
                         (not (assq 'issues val)))
                    (forge--fetch-issues repo cb until))
                   ((and .has_pull_requests
                         (not (assq 'pullreqs val)))
                    (forge--fetch-pullreqs repo cb until))
                   (t
                    (forge--msg repo t t   "Pulling REPO")
                    (forge--msg repo t nil "Storing REPO")
                    (emacsql-with-transaction (forge-db)
                      (forge--update-repository repo val)
                      (forge--update-assignees  repo .assignees)
                      (forge--update-labels     repo .labels)
                      (dolist (v .issues)   (forge--update-issue repo v))
                      (dolist (v .pullreqs) (forge--update-pullreq repo v))
                      (oset repo sparse-p nil)
                      )
                    (forge--msg repo t t "Storing REPO")
                    (unless (oref repo selective-p)
                      (forge--git-fetch buf dir repo)))))))))
    (funcall cb cb)))

(cl-defmethod forge--fetch-repository ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "/repos/:project" nil
    :callback (lambda (value _headers _status _req)
                (cond ((oref repo selective-p)
                       (setq value (append '((assignees) (forks) (labels)
                                             (issues) (pullreqs))
                                           value)))
                      ((magit-get-boolean "forge.omitExpensive")
                       (setq value (append '((assignees) (forks) (labels))
                                           value))))
                (funcall callback callback value))))

(cl-defmethod forge--update-repository ((repo forge-gitea-repository) data)
  (let-alist data
    (oset repo created        .created_at)
    (oset repo updated        .updated_at)
    (oset repo pushed         nil)
    (oset repo parent         .parent)
    (oset repo description    .description)
    (oset repo homepage       nil)
    (oset repo default-branch .default_branch)
    (oset repo archived-p     .archived)
    (oset repo fork-p         .fork)
    (oset repo locked-p       nil)
    (oset repo mirror-p       .mirror)
    (oset repo private-p      .private)
    (oset repo issues-p       .has_issues)
    (oset repo wiki-p         .has_enabled)
    (oset repo stars          .star_count)
    (oset repo watchers       .watchers_count)))


(cl-defmethod forge--fetch-assignees ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "/repos/:project/assignees"
    '((per_page . 100))
    :unpaginate t
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'assignees value)))))

(cl-defmethod forge--update-assignees ((repo forge-gitea-repository) data)
  (oset repo assignees
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      ;; For other forges we don't need to store `id'
                      ;; but here we do because that's what has to be
                      ;; used when assigning issues.
                      (list (forge--object-id id .id)
                            .username
                            .full_name
                            .id)))
                  data))))

(cl-defmethod forge--fetch-forks ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "/repos/:project/forks" 
    '((per_page . 100)
      (simple . "true"))
    :unpaginate t
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'forks value)))))

(cl-defmethod forge--update-forks ((repo forge-gitea-repository) data)
  (oset repo forks
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (nconc (forge--repository-ids
                              (eieio-object-class repo)
                              (oref repo githost)
                              .namespace.path
                              .path)
                             (list .namespace.path
                                   .path))))
                  data))))

(cl-defmethod forge--fetch-labels ((repo forge-gitea-repository) callback)
  (forge--gtea-get repo "/repos/:project/labels"
    '((per_page . 100))
    :unpaginate t
    :callback (lambda (value _headers _status _req)
                (funcall callback callback (cons 'labels value)))))

(cl-defmethod forge--update-labels ((repo forge-gitea-repository) data)
  (oset repo labels
        (with-slots (id) repo
          (mapcar (lambda (row)
                    (let-alist row
                      (list (forge--object-id id .name)
                            .name
                            (format "#%s" (downcase .color))
                            .description)))
                  ;; For now simply remove one of the duplicates.
                  (cl-delete-duplicates data
                                        :key (apply-partially #'alist-get 'name)
                                        :test #'equal)))))

(cl-defmethod forge--fetch-issue-posts ((repo forge-gitea-repository) cur cb)
  (let-alist (car cur)
    (forge--gtea-get repo
      (format "/repos/:project/issues/%s/comments" .number)
      '((per_page . 100))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (setf (alist-get 'notes (car cur)) value)
                  (funcall cb cb)))))


(cl-defmethod forge--fetch-issues ((repo forge-gitea-repository) callback until)
  (let ((cb (let (val cur cnt pos)
              (lambda (cb &optional v)
                (cond
                 ((not pos)
                  (if (setq cur (setq val v))
                      (progn
                        (setq pos 1)
                        (setq cnt (length val))
                        (forge--msg nil nil nil "Pulling issue %s/%s" pos cnt)
                        (forge--fetch-issue-posts repo cur cb))
                    (forge--msg repo t t "Pulling REPO issues")
                    (funcall callback callback (cons 'issues val))))
                 (t
                  (if (setq cur (cdr cur))
                      (progn
                        (cl-incf pos)
                        (forge--msg nil nil nil "Pulling issue %s/%s" pos cnt)
                        (forge--fetch-issue-posts repo cur cb))
                    (forge--msg repo t t "Pulling REPO issues")
                    (funcall callback callback (cons 'issues val)))))))))
    (forge--msg repo t nil "Pulling REPO issues")
    (forge--gtea-get repo "/repos/:project/issues"
      `((per_page . 100)
        (order_by . "updated_at")
        (type . "issues")
        (updated_after . ,(forge--topics-until repo until 'issue)))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (funcall cb cb value)))))

(cl-defmethod forge--update-issue ((repo forge-gitea-repository) data)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let* ((issue-id (forge--object-id 'forge-issue repo .number))
             (issue
              (forge-issue
               :id           issue-id
               :repository   (oref repo id)
               :number       .number
               :state        (pcase-exhaustive .state
                               ("close" 'closed)
                               ("open" 'open))
               :author       .user.username
               :title        .title
               :created      .created_at
               :updated      .updated_at
               ;; `.closed_at' may be nil even though the issues is
               ;; closed.  In such cases use 1, so that this slots
               ;; at least can serve as a boolean.
               :closed       (or .closed_at (and (equal .state "close") 1))
               :locked-p     .is_locked
               :milestone    .milestone.iid
               :body         (forge--sanitize-string .body))))
        (closql-insert (forge-db) issue t)
        (unless (magit-get-boolean "forge.omitExpensive")
          (forge--set-id-slot repo issue 'assignees .assignees)
          (forge--set-id-slot
           repo
           issue
           'labels
           (mapcar (lambda (x) (alist-get 'name x)) .labels)))
        (dolist (c .notes)
          (let-alist c
            (let ((post
                   (forge-issue-post
                    :id      (forge--object-id issue-id .id)
                    :issue   issue-id
                    :number  .number
                    :author  .user.username
                    :created .created_at
                    :updated .updated_at
                    :body    (forge--sanitize-string .body))))
              (closql-insert (forge-db) post t))))))))


(cl-defmethod forge--fetch-pullreqs ((repo forge-gitea-repository) callback until)
  (let ((cb (let (val cur cnt pos)
              (lambda (cb &optional v)
                (cond
                 ((not pos)
                  (if (setq cur (setq val v))
                      (progn
                        (setq pos 1)
                        (setq cnt (length val))
                        (forge--msg nil nil nil "Pulling pullreq %s/%s" pos cnt)
                        (forge--fetch-issue-posts repo cur cb)
                        )
                    (forge--msg repo t t "Pulling REPO pullreqs")
                    (funcall callback callback (cons 'pullreqs val))))
                 (t
                  (if (setq cur (cdr cur))
                      (progn
                        (cl-incf pos)
                        (forge--msg nil nil nil "Pulling pullreq %s/%s" pos cnt)
                        (forge--fetch-issue-posts repo cur cb))
                    (forge--msg repo t t "Pulling REPO pullreqs")
                    (funcall callback callback (cons 'pullreqs val)))))))))
    (forge--msg repo t nil "Pulling REPO pullreqs")
    (forge--gtea-get repo  "/repos/:project/pulls"
      `((per_page . 100)
        (order_by . "updated_at")
        (updated_after . ,(forge--topics-until repo until 'pullreq)))
      :unpaginate t
      :callback (lambda (value _headers _status _req)
                  (funcall cb cb value)))))


(cl-defmethod forge--update-pullreq ((repo forge-gitea-repository) data)
  (emacsql-with-transaction (forge-db)
    (let-alist data
      (let* ((pullreq-id (forge--object-id 'forge-pullreq repo .number))
             (pullreq
              (forge-pullreq
               :id           pullreq-id
               :repository   (oref repo id)
               :number       .number
               :state        (pcase-exhaustive .state
                               ("merge" 'merged)
                               ("close" 'closed)
                               ("open" 'open))
               :author       .user.username
               :title        .title
               :created      .created_at
               :updated      .updated_at
               ;; `.merged_at' and `.closed_at' may both be nil even
               ;; though the pullreq is merged or otherwise closed.
               ;; In such cases use 1, so that these slots at least
               ;; can serve as booleans.
               :closed       (or .closed_at
                                 (and (member .state '("closed" "merged")) 1))
               :merged       (or .merged_at
                                 (and (equal .state "merged") 1))
               :locked-p     .discussion_locked
               :editable-p   .allow_maintainer_to_push
               :cross-repo-p (not (equal .source_project_id
                                         .target_project_id))
               :base-ref     .base.ref
               :base-repo    .base.repo.full_name
               :head-ref     .head.ref
               :head-user    .head.repo.owner.username
               :head-repo    .head.repo.fullname
               :milestone    .milestone.iid
               :body         (forge--sanitize-string .description))))
        (closql-insert (forge-db) pullreq t)
        (unless (magit-get-boolean "forge.omitExpensive")
          (forge--set-id-slot repo pullreq 'assignees (list .assignee))
          (forge--set-id-slot repo pullreq 'labels (mapcar (lambda (x) (alist-get 'name x)) .labels)))
        (dolist (c .notes)
          (let-alist c
            (let ((post
                   (forge-pullreq-post
                    :id      (forge--object-id pullreq-id .number)
                    :pullreq pullreq-id
                    :number  .number
                    :author  .user.username
                    :created .created_at
                    :updated .updated_at
                    :body    (forge--sanitize-string .body))))
              (closql-insert (forge-db) post t))))))))


;;; Mutations

(cl-defmethod forge--submit-create-issue ((_ forge-gitea-repository) repo)
  (let-alist (forge--topic-parse-buffer)
    (forge--gtea-post repo "/repos/:project/issues"
      `((title       . , .title)
        (body . , .body))
      :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback))))

(cl-defmethod forge--submit-create-post ((_ forge-gitea-repository) topic)
  (forge--gtea-post topic
    "/repos/:project/issues/:number/comments"
    `((body . ,(string-trim (buffer-string))))
    :callback  (forge--post-submit-callback)
    :errorback (forge--post-submit-errorback)))

(cl-defmethod forge--topic-templates ((repo forge-gitea-repository)
                                      (_ (subclass forge-issue)))
  (--filter (string-match-p "\\`\\.gitea/issue_template.md\\'" it)
            (magit-revision-files (oref repo default-branch))))

(cl-defmethod forge--topic-templates ((repo forge-gitea-repository)
                                      (_ (subclass forge-pullreq)))
  (--filter (string-match-p "\\`\\.gitea/pull_request_template.md\\'" it)
            (magit-revision-files (oref repo default-branch))))

;;; Utilities

(cl-defun forge--gtea-get (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host callback errorback)
  (declare (indent defun))
  (gtea-get (if obj (forge--gitea-format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback
            :errorback (or errorback (and callback t))))

(cl-defun forge--gtea-put (obj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host callback errorback)
  (declare (indent defun))
  (gtea-put (if obj (forge--gitea-format-resource obj resource) resource)
            params
            :host (or host (oref (forge-get-repository obj) apihost))
            :auth 'forge
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback
            :errorback (or errorback (and callback t))))

(cl-defun forge--gtea-post (obj resource
                                &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                host callback errorback)
  (declare (indent defun))
  (gtea-post (forge--gitea-format-resource obj resource)
             params
             :host (or host (oref (forge-get-repository obj) apihost))
             :auth 'forge
             :query query :payload payload :headers headers
             :silent silent :unpaginate unpaginate
             :noerror noerror :reader reader
             :callback callback
             :errorback (or errorback (and callback t))))

(cl-defun forge--gtea-delete (obj resource
                                  &optional params
                                  &key query payload headers
                                  silent unpaginate noerror reader
                                  host callback errorback)
  (declare (indent defun))
  (gtea-delete (forge--gitea-format-resource obj resource)
               params
               :host (or host (oref (forge-get-repository obj) apihost))
               :auth 'forge
               :query query :payload payload :headers headers
               :silent silent :unpaginate unpaginate
               :noerror noerror :reader reader
               :callback callback
               :errorback (or errorback (and callback t))))


(cl-defmethod forge--gitea-format-resource ((object forge-object) resource)
  (save-match-data
    (setq resource
          (replace-regexp-in-string
           ":\\([^/]+\\)"
           (lambda (str)
             (let ((slot (intern (substring str 1))))
               (or (when-let
                       ((v (ignore-errors
                             (cl-case slot
                               (repo    (oref object name))
                               (project (concat (replace-regexp-in-string
                                                 "/" "%2F" (oref object owner))
                                                "/"
                                                (oref object name)))
                               (topic   (and (forge--childp object 'forge-topic)
                                             (oref object number)))
                               (t       (eieio-oref object slot))))))
                     (format "%s" v))
                   str)))
           resource t t))
    (if (string-match ":[^/]*" resource)
        (if-let ((parent (ignore-errors (forge-get-parent object))))
            (forge--gitea-format-resource parent resource)
          (error "Cannot resolve %s for a %s"
                 (match-string 0 resource)
                 (eieio-object-class object)))
      resource)))

;;; _
(provide 'forge-gitea)
;;; forge-gitea.el ends here
