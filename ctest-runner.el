;; -*- lexical-binding: t -*-

(require 'dash)
(require 'treemacs)
(require 'treemacs-treelib)


(defun fresh-buffer (bufname)
  (--when-let (get-buffer bufname) (kill-buffer it))
  (get-buffer-create bufname))

(defun get-ctests-json-output(dir)
  (let* ((ctest-json-output-buffer-name (format "*ctest-list-tests: %s*" dir))
         (default-directory dir))
    (--when-let (get-buffer ctest-json-output-buffer-name) (kill-buffer it))
    (let ((ctest-json-output-buffer (get-buffer-create ctest-json-output-buffer-name)))
      (with-current-buffer ctest-json-output-buffer
        (call-process "ctest" nil (current-buffer) nil "--show-only=json-v1")
        (beginning-of-buffer)
        (json-parse-buffer :object-type 'plist :array-type 'list)))))


(defun get-test-filepath(atest)
  (car (plist-get atest :command)))

(defun get-test-filename(atest)
  (file-name-base (car (plist-get atest :command))))

(defun get-ctests-tests-files()
  (let* ((json-structure (get-ctests-json-output (projectile-compilation-dir)))
         (tests (plist-get json-structure ':tests)))
    (->> tests
         (-map 'get-test-filepath)
         (-distinct)
         (-map (lambda (filepath) (file-name-base filepath)))
         )))

(defun treemacs-showcase--tests-by-file (filename)
  (let* ((json-structure (get-ctests-json-output (projectile-compilation-dir)))
         (tests (plist-get json-structure ':tests)))
    (->> tests
         (-filter
          (lambda (atest)
            (let* ((afilename (get-test-filename atest)))
              (string-equal filename  afilename))))
         )))

(defun treemacs-showcase-RET-buffer-action (&optional _)
  (let* ((test-command (-some-> (treemacs-current-button)
                         (treemacs-button-get :testcmd)))
         (test-name (-some-> (treemacs-current-button)
                      (treemacs-button-get :testname)))
         (binary (car test-command))
         (arguments (cdr test-command))
         (out-buffer-name (format "*%s:%s*" binary test-name))
         (out-buffer (fresh-buffer out-buffer-name)))
    ;; use apply to be able to give arguments (a list) where
    ;; a direct call to call-process expects individual elements
    (apply #'call-process binary nil out-buffer nil arguments)
    (with-current-buffer out-buffer
      (compilation-mode))
    (when (buffer-live-p out-buffer)
      (pop-to-buffer out-buffer))
    ))


(defun treemacs-showcase-visit-buffer-action (btn)
  (let ((buffer (treemacs-safe-button-get btn :buffer)))
    (message "visit on %s" buffer)))

(treemacs-define-expandable-node-type showcase-test-file
  :closed-icon "+ "
  :open-icon "- "
  :label (propertize item 'face 'font-lock-variable-name-face)
  :key item
  :children (treemacs-showcase--tests-by-file item)
  :child-type 'showcase-single-test
  :more-properties `(:major-mode ,item)
  :on-expand (message "Expanding node with key %s" (treemacs-button-get btn :key))
  :on-collapse (message "Collapsing node with key %s" (treemacs-button-get btn :key)))

(treemacs-define-leaf-node-type showcase-single-test
  ;; at this level, item is a plist
  ;; that looks like thos :command ("binary" "--gtest_filter=testname" "--gtest_also_run_disabled_tests") :name "testname" :properties (... ...)
  :icon "• "
  :label (propertize (plist-get item ':name) 'face 'font-lock-string-face)
  :key item
  :more-properties `(:testcmd ,(plist-get item ':command) :testname ,(plist-get item ':name))
  :visit-action #'treemacs-showcase-visit-buffer-action
  :ret-action #'treemacs-showcase-RET-buffer-action)

(treemacs-define-variadic-entry-node-type showcase-tests-files-variadic
  :key 'showcase-tests-files-variadic
  :children  (get-ctests-tests-files)
  :child-type 'showcase-test-file)

(defun showcase-tests ()
  (interactive)
  (let ((bufname (format "*Showcase tests %s*" (projectile-project-name))))
    (let ((buf (fresh-buffer bufname)))
      (pop-to-buffer buf)
      (treemacs-initialize showcase-tests-files-variadic))))
