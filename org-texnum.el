;;; org-texnum.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org-ml)

(defun org-texnum//tag-from-num-list (num-lst)
  "Convert NUM-LST to tag expected by latex.
For example, '(1 2 a) would become 1.2a."
  (let ((tag-str ""))
    (dolist (elt num-lst)
      (if (numberp elt)
          (progn
            (if (not (string-equal "" tag-str))
                (setq tag-str (concat tag-str ".")))
            (setq tag-str (concat tag-str (number-to-string elt))))
        (setq tag-str (concat tag-str elt))))
    tag-str))

(defun org-texnum//get-headlines (data)
  "Retrieve all headlines in DATA using org-ml.
DATA can either be the result of `org-ml-parse-this-buffer' or a
subheadline."
  (--filter (org-ml-is-type 'headline it) data))

(defun org-texnum//get-section (data)
  ""
  (car (--filter (org-ml-is-type 'section it) data)))

(defun org-texnum//get-latex-blocks-in-section (section)
  ""
  (let* ((src-blocks (--filter (org-ml-is-type 'src-block it) section))
         (latex-blocks (--filter (string-equal "latex" (org-ml-get-property :language it)) src-blocks)))
    latex-blocks))

(defun org-texnum//latex-block-alignp (contents)
  ""
  (if (string-match
       (rx (seq (regexp "[ \t]*")
                "\\begin{"
                (seq "align"
	             (opt "*"))
                "}")) contents)
      t
    nil))

(defun org-texnum//latex-block-equationp (contents)
  ""
  (if (string-match
       (rx (seq (regexp "[ \t]*")
                "\\begin{"
                (seq "equation"
	             (opt "*"))
                "}")) contents)
      t
    nil))

(defun org-texnum//update-tags-in-align (num-lst beg end)
  "Update tags in a latex block assuming it consists of an align environment."
  (save-excursion
    (goto-char beg)
    (setq num-lst (-snoc num-lst "a"))
    (while (re-search-forward
            (rx "\\tag{"
                (group-n 1
                         (seq (+ (seq (+ digit)
                                      (opt ".")))
                              (* (regexp "[a-z]")))) "}") end t)
      (replace-match (org-texnum//tag-from-num-list num-lst) nil nil nil 1)
      (setq num-lst (-snoc (butlast num-lst) (org-texnum//inc-char (car (last num-lst))))))
    ;; update source block result
    (goto-char beg)
    (org-ctrl-c-ctrl-c)
    ;; If we didn't encounter a tag, the last number of num-lst should
    ;; remain unchanged for the next latex block. Otherwise, increment
    ;; the last number for the next block.
    (let ((last-char (car (last num-lst)))
          (last-num (car (last (butlast num-lst)))))
      (if (string-equal "a" last-char)
          last-num
        (+ 1 last-num)))))

(defun org-texnum//update-tags-in-equation (num-lst beg end)
  ""
  (save-excursion
    (goto-char beg)
    (while (re-search-forward
            (rx "\\tag{"
                (group-n 1
                         (seq (+ (seq (+ digit)
                                      (opt ".")))
                              (* (regexp "[a-z]")))) "}") end t)
      (replace-match (org-texnum//tag-from-num-list num-lst) nil nil nil 1)
      (setq num-lst (-snoc (butlast num-lst) (+ 1 (car (last num-lst))))))
    ;; update source block result
    (goto-char beg)
    (org-ctrl-c-ctrl-c)
    ;; If we didn't encounter a tag, the last number of num-lst should
    ;; remain unchanged for the next latex block. Otherwise, increment
    ;; the last number for the next block.
    (let ((last-num (car (last num-lst))))
      last-num)))

(defun org-texnum//update-eqn-numbers-in-section (section num-lst)
  ""
  (let ((latex-blocks (org-texnum//get-latex-blocks-in-section section))
        (i 1))
    (setq num-lst (-snoc num-lst i))
    (dolist (latex-block latex-blocks)
      (let ((contents (org-ml-get-property :value latex-block))
            (beg (org-ml-get-property :begin latex-block))
            (end (org-ml-get-property :end latex-block)))
        (if (org-texnum//latex-block-alignp contents)
            (progn
              (setq i (org-texnum//update-tags-in-align num-lst beg end))
              (setq num-lst (-snoc (butlast num-lst) i)))
          (if (org-texnum//latex-block-equationp contents)
              (progn
                (setq i (org-texnum//update-tags-in-equation num-lst beg end))
                (setq num-lst (-snoc (butlast num-lst) i)))))))))

(defun org-texnum//update-eqn-numbers-in-headline (headline num-lst)
  ""
  (let ((section (org-texnum//get-section headline))
        (headlines (org-texnum//get-headlines headline)))
    (org-texnum//update-eqn-numbers-in-section section num-lst)
    (let ((i 1))
      (dolist (headline headlines)
        (org-texnum//update-eqn-numbers-in-headline headline (-snoc num-lst i))
        (setq i (+ 1 i))))))

(defun org-texnum/update-eqn-numbers-in-buffer ()
  ""
  (interactive)
  (let* ((data (org-ml-get-children (org-ml-parse-this-buffer)))
         (section (org-texnum//get-section data))
         (headlines (org-texnum//get-headlines data))
         (num-lst '(0)))
    (org-texnum//update-eqn-numbers-in-section section num-lst)
    (let ((i 1))
      (dolist (headline headlines)
        (org-texnum//update-eqn-numbers-in-headline headline (list i))
        (setq i (+ 1 i))))))

(provide 'org-texnum)

;;; org-texnum.el ends here
