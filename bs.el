(require 'cl-lib)

(defun bs-get-node-from-html (url)
  "从http `url'中获取经过`libxml-parse-html-region'解析的内容"
  (let ((url-buffer (url-retrieve-synchronously url))
        url-content)
    (with-current-buffer url-buffer
      (search-forward-regexp "^$")
      (setq url-content (libxml-parse-html-region (point) (point-max))))
    (kill-buffer url-buffer)
    url-content))

(defun bs-get-tag (node)
  "Return current tag name from NODE"
  (symbol-name (car node)))

(defun bs-get-attrs (node)
  "Return attrs as an alist from NODE"
  (second node))

(defun bs-get-attr (node attr)
  "Return attr from NODE"
  (assoc-string attr (second node)))

(defun bs-get-subnodes (node &optional subnode-tag-name)
  "Return subnodes as list from NODE"
  (let ((subnodes (cl-remove-if 'stringp (cddr node))))
    (if subnode-tag-name
        (remove-if-not (lambda (node)
                         (string= subnode-tag-name (bs-get-tag node)))
                       subnodes)
      subnodes)))

(defun bs-get-subnode (node &rest tags)
  "Return subnodes from NODE"
  (if tags
      (apply 'bs-get-subnode (assoc-string (car tags)
                                    (bs-get-subnodes node))
                      (cdr tags))
    node))

(cl-defun bs-get-texts (node)
  "Return text content from NODE

If the text in NODE splited, It will a list contains all part of text"
  (cl-remove-if-not #'stringp (cddr node)))

(cl-defun bs-get-text (node)
  "Return text content from NODE

If the text in NODE splited, It will be concated"
  (string-join (bs-get-texts node) ""))

(defun bs--node-match-p (node tag attr-rules text-rule)
  "If NODE matched,return node"
  (let ((node-tag (bs-get-tag node))
        (node-attrs (bs-get-attrs node))
        (node-text (bs-get-text node)))
    (and (or (null tag)
             (string= node-tag tag))
         (or (null text-rule)
             (string-match-p text-rule node-text))
         (or (null attr-rules)
             (cl-some (lambda (attr-rule)
                        (let* ((attr-key (car attr-rule))
                               (attr-value-reg (cdr attr-rule))
                               (the-node-attr (assoc-string attr-key node-attrs)))
                          (and the-node-attr
                               (string-match-p attr-value-reg (cdr the-node-attr)))))
                      attr-rules))
         node)))

(cl-defun bs-findAll (node tag &optional attr-rules (recursive-p t) text-rule)
  "find out all the matched subnodes like findAll method in the python package bs"
  (let ((node-subnodes (bs-get-subnodes node))
        (node-match-p (lambda (node)
                        (bs--node-match-p node tag attr-rules text-rule))))
    (if recursive-p
        (append (cl-remove-if-not node-match-p node-subnodes)
                (cl-mapcan (lambda (node)
                          (bs-findAll node tag attr-rules recursive-p text-rule))
                        node-subnodes))
      (cl-remove-if-not node-match-p node-subnodes))))

(cl-defun bs-find (node tag &optional attr-rules (recursive-p t) text-rule)
  "find out the first matched subnodes like find method in the python package bs"
  (let ((node-subnodes (bs-get-subnodes node))
        (node-match-p (lambda (node)
                        (bs--node-match-p node tag attr-rules text-rule))))
    (if recursive-p
        (or (cl-some node-match-p node-subnodes)
            (cl-some (lambda (node)
                       (bs-find node tag attr-rules recursive-p text-rule))
                     node-subnodes))
      (cl-some node-match-p node-subnodes))))



(provide 'bs)
