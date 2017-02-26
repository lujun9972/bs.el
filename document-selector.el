(require 'cl-lib)

(defun ds-get-node-from-html (url)
  "从http `url'中获取经过`libxml-parse-html-region'解析的内容"
  (let ((url-buffer (url-retrieve-synchronously url 'silent))
        url-content)
    (with-current-buffer url-buffer
      (search-forward-regexp "^$")
      (setq url-content (libxml-parse-html-region (point) (point-max))))
    (kill-buffer url-buffer)
    url-content))

(defun ds-get-tag (node)
  "Return current tag name from NODE"
  (symbol-name (car node)))

(defun ds-get-attrs (node)
  "Return attrs as an alist from NODE"
  (second node))

(defun ds-get-attr (node attr)
  "Return attr from NODE"
  (assoc-string attr (second node)))

(defun ds-get-subnodes (node &optional subnode-tag-name)
  "Return subnodes as list from NODE"
  (let ((subnodes (cl-remove-if 'stringp (cddr node))))
    (if subnode-tag-name
        (remove-if-not (lambda (node)
                         (string= subnode-tag-name (ds-get-tag node)))
                       subnodes)
      subnodes)))

(defun ds-get-subnode (node &rest tags)
  "Return subnodes from NODE"
  (if tags
      (apply 'ds-get-subnode (assoc-string (car tags)
                                    (ds-get-subnodes node))
                      (cdr tags))
    node))



(cl-defun ds-get-text (node)
  "Return text content from NODE

If the text in NODE splited, It will be concated"
  (let (text)
    (dolist (ele (cddr node) (string-join (reverse text) ""))
      (if (stringp ele)
          (push ele texts)
        (push (ds-get-text ele) texts)))))

(defun ds--node-match-p (node tag attr-rules text-rule)
  "If NODE matched,return node"
  (let ((node-tag (ds-get-tag node))
        (node-attrs (ds-get-attrs node))
        (node-text (ds-get-text node)))
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

(cl-defun ds-findAll (node tag &optional attr-rules (recursive-p t) text-rule)
  "find out all the matched subnodes like findAll method in the python package bs"
  (let ((node-subnodes (ds-get-subnodes node))
        (node-match-p (lambda (node)
                        (ds--node-match-p node tag attr-rules text-rule))))
    (if recursive-p
        (append (cl-remove-if-not node-match-p node-subnodes)
                (cl-mapcan (lambda (node)
                          (ds-findAll node tag attr-rules recursive-p text-rule))
                        node-subnodes))
      (cl-remove-if-not node-match-p node-subnodes))))

(cl-defun ds-find (node tag &optional attr-rules (recursive-p t) text-rule)
  "find out the first matched subnodes like find method in the python package bs"
  (let ((node-subnodes (ds-get-subnodes node))
        (node-match-p (lambda (node)
                        (ds--node-match-p node tag attr-rules text-rule))))
    (if recursive-p
        (or (cl-some node-match-p node-subnodes)
            (cl-some (lambda (node)
                       (ds-find node tag attr-rules recursive-p text-rule))
                     node-subnodes))
      (cl-some node-match-p node-subnodes))))



(provide 'document-selector)
