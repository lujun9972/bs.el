(require 'bs)
(require 'ert)

(defvar bs-test-text "<html><head></head><body width=101><div class=thing>Foo<div>Yes")
(defvar bs-test-node (with-temp-buffer
                       (insert bs-test-text)
                       (libxml-parse-html-region (point-min) (point-max))))

(ert-deftest bs-test-findAll ()
  ""
  (should (equal (bs-findAll bs-test-node
                             "div")
                 '((div ((class . "thing")) "Foo" (div nil "Yes")) (div nil "Yes"))))
  (should (null (bs-findAll bs-test-node
                            "table")))
  (should (equal (bs-findAll bs-test-node
                             "div" '((class . "^th")))
                 '((div ((class . "thing")) "Foo" (div nil "Yes")))))
  (should (null (bs-findAll bs-test-node
                            "div" '((class . "^th$")))))
  (should (equal (bs-findAll bs-test-node
                             "body" '((width . "101")) nil)
                 '((body ((width . "101")) (div ((class . "thing")) "Foo" (div nil "Yes"))))))
  (should (null (bs-findAll bs-test-node
                            "div" '((class . "^th")) nil)))
  (should (equal (bs-findAll bs-test-node
                             "div" nil t "Yes")
                 '((div nil "Yes"))))
  (should (null (bs-findAll bs-test-node
                            "div" nil t "None"))))
(ert-deftest bs-test-find ()
  ""
  (should (equal (bs-find bs-test-node
                             "div")
                 '(div ((class . "thing")) "Foo" (div nil "Yes"))))
  (should (null (bs-find bs-test-node
                            "table")))
  (should (equal (bs-find bs-test-node
                             "div" '((class . "^th")))
                 '(div ((class . "thing")) "Foo" (div nil "Yes"))))
  (should (null (bs-find bs-test-node
                            "div" '((class . "^th$")))))
  (should (equal (bs-find bs-test-node
                             "body" '((width . "101")) nil)
                 '(body ((width . "101")) (div ((class . "thing")) "Foo" (div nil "Yes")))))
  (should (null (bs-find bs-test-node
                            "div" '((class . "^th")) nil)))
  (should (equal (bs-find bs-test-node
                             "div" nil t "Yes")
                 '(div nil "Yes")))
  (should (null (bs-find bs-test-node
                         "div" nil t "None"))))

(ert-deftest bs-test-get-subnode ()
  ""
  (should (equal (bs-get-subnode bs-test-node
                             "head")
                 '(head nil)))
  (should (equal (bs-get-subnode bs-test-node
                                 "body")
                 '(body ((width . "101")) (div ((class . "thing")) "Foo" (div nil "Yes")))))
  (should (null (bs-get-subnode bs-test-node
                            "table")))
  (should (equal (bs-get-subnode bs-test-node
                                 "body" "div")
                 '(div ((class . "thing")) "Foo" (div nil "Yes"))))
  (should (null (bs-get-subnode bs-test-node
                            "head" "div")))
  (should (equal (bs-get-subnode bs-test-node
                             "body" "div" "div")
                 '(div nil "Yes"))))
