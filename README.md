# Print licenses used by the given project and its dependencies

Install with Quicklisp:

    (ql:quickload "print-licenses")

Note that in order to do this the project must be `quickload`ed, so you might
want to do this in a separate Lisp image if you don't want to clutter your
current one.


If the project does not specify its license in its ASDF system definition it
will be listed as 'Unspecified'.  You should manually figure out what license
it uses (and maybe send a pull request).

Example:

~~~lisp
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
~~~

Or you might want to group systems by their license:

~~~lisp
  (print-licenses :fast-io
                  :group-by-license t)
  =>
  MIT
    babel, cffi, cffi-grovel, cffi-toolchain, fast-io, static-vectors, trivial-features, trivial-gray-streams

  Public Domain / 0-clause MIT
    alexandria

  Unspecified
    asdf, uiop
~~~

## See also

Related, to get a graph of dependencies:

* [asdf-dependency-graph](https://github.com/digikar99/asdf-dependency-graph/) - A minimal wrapper around `dot` to generate an image of the dependencies graph.

and this snippet:

```lisp
;; thanks https://gist.github.com/svetlyak40wt/a8ed639bf8fe07caed1531611bcf932d
;; added: max-level
(defun print-dependency-graph (system-name &key (level 0) (max-level most-positive-fixnum))
  (when (= level max-level)
    (return-from print-dependency-graph))
  (loop repeat level
        do (format t "  "))
  (format t "~A~%" system-name)
  (typecase system-name
    ((or string symbol)
     (let ((system (asdf/system:find-system system-name)))
       (loop for dep in (asdf/system:system-depends-on system)
              do (print-dependency-graph dep :level (1+ level) :max-level max-level))))))
```

```txt
(print-dependency-graph :str :max-level 3)
;; =>
STR
  cl-ppcre
  cl-ppcre-unicode
    cl-ppcre
    cl-unicode
  cl-change-case
    cl-ppcre
    cl-ppcre-unicode
```

## Credit

Original code entirely taken from
[**@sjl**'s utilities](https://github.com/sjl/cl-losh/blob/master/losh.lisp),
the original snippet to build the license tree coming from a
**@dk_jackdaniel**'s [snippet](http://paste.lisp.org/display/327154).

MIT.
