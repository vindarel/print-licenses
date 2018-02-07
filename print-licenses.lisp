;;;
;;; Taken from https://github.com/sjl/cl-losh/blob/master/losh.lisp
;;; Some Quickutil functions were included, as indicated. See http://quickutil.org/.
;;;

(defpackage print-licenses
  (:use :cl
        :alexandria
        :iterate)
  (:export :print-licenses))

(in-package :print-licenses)


(defmacro shut-up (&body body)
  "Run `body` with stdout and stderr redirected to the void."
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream)))
     ,@body))

;; from quickutil.
(defun map-tree (function tree)
    "Map `function` to each of the leave of `tree`."
    (check-type tree cons)
    (labels ((rec (tree)
               (cond
                 ((null tree) nil)
                 ((atom tree) (funcall function tree))
                 ((consp tree)
                  (cons (rec (car tree))
                        (rec (cdr tree)))))))
      (rec tree)))

(defun aesthetic-string (thing)
  ;; Quickutil
  "Return the string used to represent `thing` when printing aesthetically."
  (format nil "~A" thing))

(defun weave (&rest lists)
  ;; Quickutil
  "Return a list whose elements alternate between each of the lists
`lists`. Weaving stops when any of the lists has been exhausted."
  (apply #'mapcan #'list lists))

(defun print-table (rows)
  "Print `rows` as a nicely-formatted table.
  Each row should have the same number of colums.
  Columns will be justified properly to fit the longest item in each one.
  Example:
    (print-table '((1 :red something)
                   (2 :green more)))
    =>
    1 | RED   | SOMETHING
    2 | GREEN | MORE
  "
  (when rows
    (iterate
      (with column-sizes =
            (reduce (curry #'mapcar #'max)
                    (mapcar (curry #'mapcar (compose #'length #'aesthetic-string))
                            rows))) ; lol
      (for row :in rows)
      (format t "~{~vA~^ | ~}~%" (weave column-sizes row))))
  (values))

;;; Original code from @dk_jackdaniel:
;;; http://paste.lisp.org/display/327154
(defun license-tree (quicklisp-project-designator)
  (flet ((dependency-tree (project)
           (uiop:symbol-call :ql-dist :dependency-tree project))
         (quickload (project)
           (uiop:symbol-call :ql :quickload project))
         (system-file-name (s)
           (uiop:symbol-call :ql-dist :system-file-name s)))
    (let ((name-slot-symbol (find-symbol (string 'name) :ql-dist))
          (sys (dependency-tree quicklisp-project-designator)))
      (assert (not (null sys)) ()
              "Cannot find Quicklisp project for designator ~S"
              quicklisp-project-designator)
      (shut-up
       (quickload quicklisp-project-designator))
      (map-tree
       (lambda (s)
         (vector (slot-value s name-slot-symbol)
                 (or (asdf:system-license
                      (asdf:find-system
                       (system-file-name s)))
                     "Unspecified")))
       sys))))

(defun license-list (quicklisp-project-designator)
  (remove-duplicates
    (mapcar (alexandria:rcurry #'coerce 'list)
            (alexandria:flatten (license-tree quicklisp-project-designator)))
    :key #'car :test #'string=))

(defun print-licenses (quicklisp-project-designator)
  "Print the licenses used by the given project and its dependencies.
  Note that in order to do this the project must be `quickload`ed, so you might
  want to do this in a separate Lisp image if you don't want to clutter your
  current one.
  If the project does not specify its license in its ASDF system definition it
  will be listed as 'Unspecified'.  You should manually figure out what license
  it uses (and maybe send a pull request).
  Example:
    (print-licenses 'fast-io)
    =>
    alexandria           | Public Domain / 0-clause MIT
    babel                | MIT
    cffi                 | MIT
    cffi-grovel          | MIT
    cffi-toolchain       | MIT
    fast-io              | NewBSD
    static-vectors       | MIT
    trivial-features     | MIT
    trivial-gray-streams | MIT
    uiop                 | Unspecified
  "
  (print-table (sort (license-list quicklisp-project-designator)
                     #'string<
                     :key #'car)))
