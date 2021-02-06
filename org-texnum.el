;;; org-texnum.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org-ml)

(defun org-texnum//inc-char (char)
  "Increment CHAR.
For instance this will perform 'a' -> 'b'"
  (string (1+ (string-to-char char))))

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
      ;; replace regex match if different from current tag
      (let ((replacement-text (org-texnum//tag-from-num-list num-lst)))
        (if (not (string-equal (match-string 1) replacement-text))
            (progn
              (replace-match replacement-text nil nil nil 1)
              ;; buffer parse is now invalid, perform the whole process again
              (funcall-interactively #'org-texnum/update-eqn-numbers-in-current-buffer))))
      (setq num-lst (-snoc (butlast num-lst) (org-texnum//inc-char (car (last num-lst))))))
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
      ;; replace regex match if different from current tag
      (let ((replacement-text (org-texnum//tag-from-num-list num-lst)))
        (if (not (string-equal (match-string 1) replacement-text))
            (progn
              (replace-match replacement-text nil nil nil 1)
              ;; buffer parse is now invalid, perform the whole process again
              (funcall-interactively #'org-texnum/update-eqn-numbers-in-current-buffer))))
      (setq num-lst (-snoc (butlast num-lst) (+ 1 (car (last num-lst))))))
    ;; return number that should be used for next equation tag
    (let ((last-num (car (last num-lst))))
      last-num)))

(defun org-texnum//execute-equations-in-section (section take)
  "Only execute TAKE block so that this function can be called recursively."
  (let ((latex-block (nth take (org-texnum//get-latex-blocks-in-section section))))
    (if latex-block
        (let ((beg (org-ml-get-property :begin latex-block)))
          (goto-char (+ 1 beg))
          (org-ctrl-c-ctrl-c)
          (org-texnum//execute-equations-in-section
           ;; need to refresh invalidated section
           (org-ml-parse-section-at (point))
           (+ 1 take))))))

(defun org-texnum//execute-equations-in-headline (headline)
  ""
  (let ((section (org-texnum//get-section headline))
        (headlines (org-texnum//get-headlines headline)))
    (org-texnum//execute-equations-in-section section 0)
    (dolist (headline headlines)
      (org-texnum//execute-equations-in-headline headline))))

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

(defun org-texnum//latex-blocks-in-headline (headline latex-blocks)
  "Retrieve all LATEX-BLOCKS (LaTeX src blocks) in HEADLINE."
  (let ((section (org-texnum//get-section headline))
        (headlines (org-texnum//get-headlines headline)))
    (setq latex-blocks
          (append latex-blocks (org-texnum//get-latex-blocks-in-section section)))
    (dolist (headline headlines)
      (setq latex-blocks
            (org-texnum//latex-blocks-in-headline headline latex-blocks)))
    latex-blocks))

(defun org-texnum//latex-blocks-in-current-buffer ()
  "Retrieve all LaTeX src blocks in the current buffer."
  (let* ((buffer-tree (org-ml-parse-this-buffer))
         (top-section (car (org-ml-match '(section) buffer-tree)))
         (latex-blocks (org-texnum//get-latex-blocks-in-section top-section))
         (headlines (org-texnum//get-headlines buffer-tree)))
    (dolist (headline headlines)
      (setq latex-blocks (org-texnum//latex-blocks-in-headline headline latex-blocks)))
    latex-blocks))

(defun org-texnum//latex-blocks-begin ()
  "Retrieve begin position of all LaTeX blocks in the current buffer."
  (let ((latex-blocks (org-texnum//latex-blocks-in-current-buffer))
        (blocks-begin '()))
    (dolist (latex-block latex-blocks)
      (setq blocks-begin (append blocks-begin
                                 (list (org-ml-get-property :begin latex-block)))))
    blocks-begin))

(defun org-texnum//execute-latex-block-at-pos (pos)
  "Execute LaTeX block at POS."
  (goto-char pos)
  (org-ctrl-c-ctrl-c))

(defun org-texnum//execute-all-latex-blocks-in-current-buffer-from-count (count)
  "Execute all LaTeX src blocks (ctrl-C ctrl-C) in the current buffer from COUNT."
  (let* ((latex-blocks-begin (org-texnum//latex-blocks-begin))
         (latex-block-counter-max (length latex-blocks-begin)))
    (if (not (eq count latex-block-counter-max))
        (let ((begin (nth count latex-blocks-begin)))
          (while  (and (not (buffer-modified-p))
                       (< count latex-block-counter-max))
            (org-texnum//execute-latex-block-at-pos begin)
            (setq count (+ 1 count))
            (setq begin (nth count latex-blocks-begin)))
          (save-buffer)
          (org-texnum//execute-all-latex-blocks-in-current-buffer-from-count count)))))

(defun org-texnum/execute-all-latex-blocks-in-current-buffer ()
  "Execute all LaTeX src blocks (ctrl-C ctrl-C) in the current buffer."
  (interactive)
  (org-texnum//execute-all-latex-blocks-in-current-buffer-from-count 0))

;; TODO separate update equation numbers and execute all
;; equations. Top-level function that calls both should be called
;; `org-texnum/normalize-equation-numbers-in-buffer'.

(defun org-texnum/update-eqn-numbers-in-current-buffer ()
  "Update equation numbers LaTeX src blocks and execute all blocks."
  (interactive)
  (let* ((data (org-ml-get-children (org-ml-parse-this-buffer)))
         (section (org-texnum//get-section data))
         (headlines (org-texnum//get-headlines data))
         (num-lst '(0)))
    (org-texnum//update-eqn-numbers-in-section section num-lst)
    (let ((i 1))
      (dolist (headline headlines)
        (org-texnum//update-eqn-numbers-in-headline headline (list i))
        (setq i (+ 1 i))))
    (org-texnum/execute-all-latex-blocks-in-current-buffer)))

(provide 'org-texnum)

;;; org-texnum.el ends here
