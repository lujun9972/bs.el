(require 'document-selector)
(require 'ert)

(defvar ds-test-text "<html><head></head><body width=101><div class=thing>Foo<div>Yes")
(defvar ds-test-node (with-temp-buffer
                       (insert ds-test-text)
                       (libxml-parse-html-region (point-min) (point-max))))

(ert-deftest ds-test-findAll ()
  ""
  (should (equal (ds-findAll ds-test-node
                             "div")
                 '((div ((class . "thing")) "Foo" (div nil "Yes")) (div nil "Yes"))))
  (should (null (ds-findAll ds-test-node
                            "table")))
  (should (equal (ds-findAll ds-test-node
                             "div" '((class . "^th")))
                 '((div ((class . "thing")) "Foo" (div nil "Yes")))))
  (should (null (ds-findAll ds-test-node
                            "div" '((class . "^th$")))))
  (should (equal (ds-findAll ds-test-node
                             "body" '((width . "101")) nil)
                 '((body ((width . "101")) (div ((class . "thing")) "Foo" (div nil "Yes"))))))
  (should (null (ds-findAll ds-test-node
                            "div" '((class . "^th")) nil)))
  (should (equal (ds-findAll ds-test-node
                             "div" nil t "Yes")
                 '((div nil "Yes"))))
  (should (null (ds-findAll ds-test-node
                            "div" nil t "None"))))
(ert-deftest ds-test-find ()
  ""
  (should (equal (ds-find ds-test-node
                             "div")
                 '(div ((class . "thing")) "Foo" (div nil "Yes"))))
  (should (null (ds-find ds-test-node
                            "table")))
  (should (equal (ds-find ds-test-node
                             "div" '((class . "^th")))
                 '(div ((class . "thing")) "Foo" (div nil "Yes"))))
  (should (null (ds-find ds-test-node
                            "div" '((class . "^th$")))))
  (should (equal (ds-find ds-test-node
                             "body" '((width . "101")) nil)
                 '(body ((width . "101")) (div ((class . "thing")) "Foo" (div nil "Yes")))))
  (should (null (ds-find ds-test-node
                            "div" '((class . "^th")) nil)))
  (should (equal (ds-find ds-test-node
                             "div" nil t "Yes")
                 '(div nil "Yes")))
  (should (null (ds-find ds-test-node
                         "div" nil t "None"))))

(ert-deftest ds-test-get-subnode ()
  ""
  (should (equal (ds-get-subnode ds-test-node
                             "head")
                 '(head nil)))
  (should (equal (ds-get-subnode ds-test-node
                                 "body")
                 '(body ((width . "101")) (div ((class . "thing")) "Foo" (div nil "Yes")))))
  (should (null (ds-get-subnode ds-test-node
                            "table")))
  (should (equal (ds-get-subnode ds-test-node
                                 "body" "div")
                 '(div ((class . "thing")) "Foo" (div nil "Yes"))))
  (should (null (ds-get-subnode ds-test-node
                            "head" "div")))
  (should (equal (ds-get-subnode ds-test-node
                             "body" "div" "div")
                 '(div nil "Yes"))))
