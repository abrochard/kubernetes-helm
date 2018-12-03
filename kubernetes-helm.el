;;; kubernetes-helm.el --- extension for controlling helm, the package manager for kubernetes

;; Copyright (C) 2018, Adrien Brochard

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 1.0
;; Author: Adrien Brochard
;; Keywords: kubernetes helm k8s
;; URL: https://github.com/abrochard/kubernetes-helm
;; License: GNU General Public License >= 3
;; Package-Requires: ((yaml-mode "") (emacs "26.1"))

;;; Commentary:

;; Emacs utility functions and bindings for [helm](https://helm.sh/), the kubernetes package manager.

;;; Usage:

;; Invoke the interactive functions
;;
;; M-x kubernetes-helm-dep-up
;; M-x kubernetes-helm-install
;; M-x kubernetes-helm-upgrade
;; M-x kubernetes-helm-values
;; M-x kubernetes-helm-status
;;
;; To respectively
;; - update the dependencies
;; - install a chart
;; - upgrade a chart
;; - get the values of a deployed chart
;; - get the status of a deployment
;;
;; Note that in most cases, you will be prompted for the k8s namespace.

;;; Code:

(require 'yaml-mode)

(defun kubernetes-helm--run-command (command &optional buffer-name)
  "Utility function to run a command in a temp buffer.

COMMAND is the command string.
BUFFER-NAME is the name of the temp buffer.  Default to *kubel-command*"
  (if (not buffer-name)
      (setq buffer-name "*kubernetes-helm-command*"))
  (with-output-to-temp-buffer buffer-name
    (shell-command command
                   buffer-name)
    (pop-to-buffer buffer-name)))

;;;###autoload
(defun kubernetes-helm-dep-up (directory)
  "Run helm dep up on a directory.

DIRECTORY is the chart location."
  (interactive "DChart: ")
  (kubernetes-helm--run-command (concat "helm dep up " directory " &")))

(defun kubernetes-helm-install (namespace directory values-file)
  "Run helm install.

NAMESPACE is the namespace.
DIRECTORY is the chart location.
VALUES-FILE is the override values."
  (interactive "MNamespace: \nDChart: \nfValues file: ")
  (kubernetes-helm--run-command
   (concat "helm install " directory (if (y-or-n-p "Dry run? ") " --dry-run --debug") " -f " values-file " --name " namespace " &")))

(defun kubernetes-helm-upgrade (namespace directory values-file)
  "Run helm upgrade.

NAMESPACE is the namespace.
DIRECTORY si the chart location.
VALUES-FILE is teh override values."
  (interactive "MNamespace: \nDChart: \nfValues file: ")
  (kubernetes-helm--run-command
   (concat "helm upgrade " namespace " " directory (if (y-or-n-p "Dry run? ") " --dry-run --debug") " -f " values-file " &")))

(defun kubernetes-helm-values (namespace)
  "Get helm values for a namespace.

NAMESPACE is the namespace."
  (interactive "MNamespace: ")
  (let ((buffer-name (concat "*kubernetes - helm - " namespace "*")))
    (shell-command (concat "helm get values " namespace) buffer-name)
    (pop-to-buffer buffer-name)
    (yaml-mode)))

(defun kubernetes-helm-status (namespace)
  "Get helm status for a namespace.

NAMESPACE is the namespace."
  (interactive "MNamespace: ")
  (kubernetes-helm--run-command (concat "helm status " namespace)))

(provide 'kubernetes-helm)
;;; kubernetes-helm.el ends here
