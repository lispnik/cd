;;;; tools/gen-bindings.lisp — draft the src/ffi/ layer from CD's headers.
;;;;
;;;; NOT part of any shipped system. Run by hand when upstream changes:
;;;;
;;;;   sbcl --non-interactive --load tools/gen-bindings.lisp \
;;;;        --eval '(cd.gen:generate "/path/to/tecgraf-cd")'
;;;;
;;;; What comes out is a first draft. Every file it writes is committed and
;;;; then hand-corrected, and the corrections stay -- regenerating overwrites,
;;;; so re-run it into a clean tree and diff.
;;;;
;;;; Adapted from the same tool in lispnik/im. The parsing machinery carries
;;;; over unchanged; the constants do not. IM declares named enums, so a
;;;; DEFCENUM falls out of the C. CD writes anonymous ones and names the group
;;;; only in a comment:
;;;;
;;;;     enum {                        /* bitmap type */
;;;;      CD_RGB,
;;;;      CD_MAP,
;;;;      CD_RGBA = 0x100
;;;;     };
;;;;
;;;; so the group name has to come from that comment -- which sits on the
;;;; `enum {' line most of the time and on the line above it twice.

(require :asdf)
(asdf:load-system :cl-ppcre)
(asdf:load-system :alexandria)

(defpackage #:cd.gen
  (:use #:common-lisp)
  (:export #:generate #:coverage-report))

(in-package #:cd.gen)

;;; ---------------------------------------------------------------------------
;;; Name conversion
;;; ---------------------------------------------------------------------------

(defparameter *name-fixups*
  '(("RGBA" . "Rgba") ("RGB" . "Rgb") ("YAxis" . "Yaxis")
    ("MM" . "Mm") ("DXF" . "Dxf") ("DGN" . "Dgn") ("CGM" . "Cgm")
    ("EMF" . "Emf") ("WMF" . "Wmf") ("PPTX" . "Pptx") ("PDF" . "Pdf")
    ("PS" . "Ps") ("SVG" . "Svg") ("GL" . "Gl") ("2D" . "2d"))
  "C spellings whose camel-case boundaries the general rule gets wrong.

The rule that turns HTTPServer into http-server needs the following word to be
capitalised. CD has runs where it is not -- cdCanvasPutImageRectRGBA is
...Rect + RGBA, and the rule sees RGB|A. Listing the handful of affected
spellings is cheaper and clearer than a heuristic that would need to know
which capital runs are words.")

(defun apply-name-fixups (name)
  (let ((result name))
    (dolist (fix *name-fixups* result)
      (let ((at (search (car fix) result)))
        (when at
          (setf result (concatenate 'string (subseq result 0 at) (cdr fix)
                                    (subseq result (+ at (length (car fix)))))))))))

(defun kebab (name)
  "Convert a C identifier to kebab case. Runs of capitals stay together."
  (let* ((name (apply-name-fixups name))
         (out (make-string-output-stream)))
    (loop for i below (length name)
          for c = (char name i)
          for prev = (when (plusp i) (char name (1- i)))
          for next = (when (< (1+ i) (length name)) (char name (1+ i)))
          do (when (and prev
                        (or (and (upper-case-p c)
                                 (or (lower-case-p prev) (digit-char-p prev)
                                     (and next (lower-case-p next))))
                            (char= c #\_)))
               (write-char #\- out))
             (unless (char= c #\_)
               (write-char (char-downcase c) out)))
    (get-output-stream-string out)))

(defparameter *function-prefixes* '("cdf" "wdf" "cd" "wd")
  "C name prefixes this binding covers, longest first.

Order matters: cdfCanvasLine starts with \"cd\", so testing \"cd\" first
would strip two characters and leave fCanvasLine.")

(defun split-prefix (name)
  "cdfCanvasLine -> (values \"cdf\" \"CanvasLine\"), or NIL if no prefix fits."
  (dolist (prefix *function-prefixes*)
    (when (and (> (length name) (length prefix))
               (string= prefix (subseq name 0 (length prefix)))
               ;; The character after the prefix must start a new word, or
               ;; "cdecl"-shaped names would match "cd".
               (upper-case-p (char name (length prefix))))
      (return-from split-prefix
        (values prefix (subseq name (length prefix))))))
  nil)

(defun lisp-fn-name (c-name)
  "cdCanvasLine -> \"%cd-canvas-line\"; wdCanvasLine -> \"%wd-canvas-line\".

The prefix is kept rather than dropped. In IM there was one namespace and
dropping \"im\" lost nothing; here cdCanvasLine and wdCanvasLine are different
functions -- pixels versus world coordinates -- and collapsing both to
%canvas-line would silently bind one over the other."
  (multiple-value-bind (prefix rest) (split-prefix c-name)
    (if prefix
        (format nil "%~A-~A" prefix (kebab rest))
        (format nil "%~A" (kebab c-name)))))

;;; ---------------------------------------------------------------------------
;;; Type mapping
;;; ---------------------------------------------------------------------------

(defparameter *scalar-types*
  '(("void" . :void) ("int" . :int) ("unsigned int" . :unsigned-int)
    ("char" . :char) ("signed char" . :char) ("unsigned char" . :unsigned-char)
    ("short" . :short) ("unsigned short" . :unsigned-short)
    ("long" . :long) ("unsigned long" . :unsigned-long)
    ("float" . :float) ("double" . :double) ("size_t" . :size)))

(defparameter *opaque-types*
  '(("cdCanvas"  . cd-canvas)
    ("cdContext" . cd-context)
    ("cdState"   . cd-state)
    ("cdImage"   . cd-image)
    ("cdBitmap"  . cd-bitmap)
    ("cdPattern" . cd-pattern)
    ("cdStipple" . cd-stipple)
    ;; The CD <-> IM bridge. Declared here so cdim.h's signatures read
    ;; im-image rather than :pointer; the CD package hands IM:HANDLE across.
    ("imImage"   . im-image))
  "CD's opaque handles, each a DEFCTYPE aliasing :POINTER in src/ffi/types.lisp.")

(defparameter *callback-types* '("cdCallback" "cdSizeCB")
  "Function-pointer typedefs. :POINTER is the right answer for all of them.")

(defparameter *enum-types* (make-hash-table :test #'equal))

(defparameter *ignored-type-tokens* '("CD_API" "CDAPI" "cdecl")
  "Macros that sit inside a declaration's type but are not part of it.")

(defun strip-ignored-tokens (s)
  (let ((result s))
    (dolist (token *ignored-type-tokens* result)
      (setf result (cl-ppcre:regex-replace-all
                    (format nil "\\b~A\\b" token) result " ")))))

(defun normalize-type (s)
  ;; The bag must be a list of characters. Common Lisp string literals have no
  ;; \t escape, so " \t" is the two-element bag {space, t} and STRING-TRIM
  ;; would strip a trailing letter t from every type it saw.
  (string-trim '(#\Space #\Tab)
               (cl-ppcre:regex-replace-all "\\s+" (strip-ignored-tokens s) " ")))

(defun map-type (raw &key returnp)
  "Map a C type string to a CFFI type, or a TODO marker for review."
  (let* ((s (normalize-type raw))
         (constp (search "const " s))
         (base (normalize-type (cl-ppcre:regex-replace-all "\\bconst\\b" s "")))
         (stars (count #\* base))
         (bare (normalize-type (remove #\* base))))
    (cond
      ;; const char* is an input string. A non-const char* parameter is an
      ;; output buffer -- cdCanvasGetFont fills one -- and must stay a pointer,
      ;; or CFFI would convert a Lisp string in and nothing would come back.
      ((and (string= bare "char") (= stars 1) (or constp returnp)) :string)
      ((and (string= bare "char") (= stars 1)) :pointer)
      ((and (string= bare "void") (zerop stars)) :void)
      ((member bare *callback-types* :test #'string=) :pointer)
      ((plusp stars)
       (let ((opaque (cdr (assoc bare *opaque-types* :test #'string=))))
         (if (and opaque (= stars 1)) opaque :pointer)))
      ((gethash bare *enum-types*) (gethash bare *enum-types*))
      ((cdr (assoc bare *scalar-types* :test #'string=))
       (cdr (assoc bare *scalar-types* :test #'string=)))
      (t (list :todo raw)))))

;;; ---------------------------------------------------------------------------
;;; Header parsing
;;; ---------------------------------------------------------------------------

(defun read-header (path)
  (with-open-file (in path :external-format :latin-1)
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))))

(defparameter +comment-scanner+
  ;; Comments and preprocessor directives, including backslash continuations.
  ;;
  ;; Blanked before the declaration scanner runs, not after. That scanner lets
  ;; a parameter list span newlines, as real C declarations do; over comment
  ;; text it will start at an English word, take a parenthesis from prose, and
  ;; scan on for the first `);' -- which is the closing paren of the NEXT real
  ;; declaration. The match swallows it, and since matches do not overlap the
  ;; function silently disappears from the output.
  (cl-ppcre:create-scanner
   "(?s)/\\*.*?\\*/|//[^\\n]*|(?m:^[ \\t]*#(?:[^\\n\\\\]|\\\\.)*)"))

(defun blank-comments (text)
  "TEXT with comments and preprocessor lines replaced by spaces, offsets kept."
  (let ((out (copy-seq text)))
    (cl-ppcre:do-matches (ms me +comment-scanner+ text)
      (loop for i from ms below me
            unless (char= (char out i) #\Newline)
              do (setf (char out i) #\Space)))
    out))

(defparameter +doc-scanner+
  (cl-ppcre:create-scanner "/\\*(.*?)\\*/" :single-line-mode t))

(defparameter +decl-scanner+
  (cl-ppcre:create-scanner
   "([A-Za-z_][A-Za-z0-9_]*(?:\\s+[A-Za-z_][A-Za-z0-9_]*)*\\s*\\**)\\s*\\b((?:cdf|wdf|cd|wd)[A-Z][A-Za-z0-9_]*)\\s*\\(([^;{}]*?)\\)\\s*;"
   :single-line-mode t))

(defstruct (cdecl (:conc-name decl-))
  name return params doc header)

(defun clean-doc (text)
  "TEXT as a docstring, or NIL if it does not look like documentation.

CD's headers are not doxygen. What they carry between declarations are
section headers -- /* primitives */, /* color */, /* attributes */ -- and
adopting the nearest one as a function's docstring produces confident
nonsense: cdCanvasGetSize came out documented as \"need an external library\"
and cdCanvasPlay as \"interpretation\", each inherited from a comment about
something else entirely.

A missing docstring is a gap someone can fill. A wrong one is a trap, so the
bar is prose: at least one sentence-ending period, and long enough not to be a
label. That keeps the handful of real comments -- the Quartz driver's, for
one -- and drops all twenty-odd section headers."
  (when text
    (let* ((s (cl-ppcre:regex-replace-all "(?m)^\\s*\\*+\\s?" text ""))
           (s (cl-ppcre:regex-replace-all "\\s+" s " ")))
      (setf s (string-trim " " s))
      (when (and (>= (length s) 40)
                 (find #\. s))
        s))))

(defun parse-params (text)
  (let ((text (normalize-type text)))
    (when (or (string= text "") (string= text "void"))
      (return-from parse-params nil))
    (loop for raw in (cl-ppcre:split "\\s*,\\s*" text)
          for i from 0
          collect (let* ((raw (normalize-type raw))
                         (m (nth-value 1 (cl-ppcre:scan-to-strings
                                          "^(.*?[\\s\\*])([A-Za-z_][A-Za-z0-9_]*)\\s*(\\[\\s*\\])?$"
                                          raw))))
                    (cond
                      ((string= raw "...") (cons "&rest" :varargs))
                      (m (let ((type (aref m 0)) (name (aref m 1)) (array (aref m 2)))
                           (cons (kebab name) (if array :pointer (map-type type)))))
                      (t (cons (format nil "arg~D" i) (map-type raw))))))))

;;; Anonymous enums -----------------------------------------------------------

(defparameter +enum-scanner+
  (cl-ppcre:create-scanner "enum\\s*(?:[A-Za-z_][A-Za-z0-9_]*)?\\s*\\{([^}]*)\\}\\s*;"
                           :single-line-mode t))

(defun enum-group-name (text start)
  "The name for the anonymous enum beginning at START.

CD puts it in a comment: usually trailing the `enum {' line, twice on the line
above. Both are checked; without a name the group would have to be numbered,
and :ENUM-7-RGB is no use to anyone."
  (let* ((line-end (or (position #\Newline text :start start) (length text)))
         (line (subseq text start line-end))
         (trailing (nth-value 1 (cl-ppcre:scan-to-strings "/\\*\\s*(.*?)\\s*\\*/" line))))
    (or (when trailing (aref trailing 0))
        ;; Look at the preceding line.
        (let* ((prev-end (position #\Newline text :end start :from-end t))
               (prev-start (when prev-end
                             (or (position #\Newline text :end prev-end :from-end t) 0))))
          (when prev-start
            (let ((prev (nth-value 1 (cl-ppcre:scan-to-strings
                                      "/\\*\\s*(.*?)\\s*\\*/"
                                      (subseq text prev-start prev-end)))))
              (when prev (aref prev 0))))))))

(defun sanitize-group-name (name)
  "\"polygon mode (begin...end)\" -> \"polygon-mode\"."
  (when name
    (let* ((s (string-downcase name))
           (s (cl-ppcre:regex-replace "\\s*\\(.*" s ""))
           (s (cl-ppcre:regex-replace-all "[^a-z0-9]+" s "-"))
           (s (string-trim "-" s)))
      (when (plusp (length s)) s))))

(defun common-member-prefix (members)
  "The longest underscore-delimited prefix every member shares, e.g. CD_CAP_."
  (when (null members) (return-from common-member-prefix ""))
  (flet ((parts (s) (let (out (start 0))
                      (loop for i = (position #\_ s :start start)
                            do (push (subseq s start i) out)
                               (if i (setf start (1+ i)) (return)))
                      (nreverse out))))
    (let* ((split (mapcar #'parts members))
           (shortest (reduce #'min split :key #'length))
           (n 0))
      (loop for i below (1- shortest)
            for candidate = (nth i (first split))
            while (every (lambda (p) (equal (nth i p) candidate)) split)
            do (incf n))
      (if (zerop n) "" (format nil "~{~A_~}" (subseq (first split) 0 n))))))

(defun parse-enums (text)
  "Every anonymous enum in TEXT, as (group-name members), members (NAME . VALUE)."
  (let (result)
    (cl-ppcre:do-matches (ms me +enum-scanner+ text)
      (declare (ignore me))
      (multiple-value-bind (whole groups)
          (cl-ppcre:scan-to-strings +enum-scanner+ text :start ms)
        (declare (ignore whole))
        (let* ((body (cl-ppcre:regex-replace-all "(?s)/\\*.*?\\*/" (aref groups 0) ""))
               (name (sanitize-group-name (enum-group-name text ms)))
               (members
                 (loop for chunk in (cl-ppcre:split "\\s*,\\s*" body)
                       for trimmed = (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                  chunk)
                       when (plusp (length trimmed))
                         collect (let ((eq (position #\= trimmed)))
                                   (if eq
                                       (cons (string-trim " " (subseq trimmed 0 eq))
                                             (string-trim " " (subseq trimmed (1+ eq))))
                                       (cons trimmed nil))))))
          (when (and name members)
            (push (list name members) result)))))
    (nreverse result)))

(defun parse-decls (text header)
  (let ((docs '())
        (code (blank-comments text)))
    (cl-ppcre:do-matches (ms me +doc-scanner+ text)
      (push (cons me (clean-doc (subseq text (+ ms 2) (- me 2)))) docs))
    (setf docs (nreverse docs))
    (let (result)
      (cl-ppcre:do-matches (ms me +decl-scanner+ code)
        (declare (ignore me))
        (multiple-value-bind (whole groups)
            (cl-ppcre:scan-to-strings +decl-scanner+ code :start ms)
          (declare (ignore whole))
          (push (make-cdecl
                 :name (aref groups 1)
                 :return (map-type (aref groups 0) :returnp t)
                 :params (parse-params (aref groups 2))
                 ;; Only adopt a comment separated from the declaration by
                 ;; whitespace alone; otherwise a function with no doc block
                 ;; silently inherits its neighbour's prose, which is a
                 ;; confident wrong answer rather than a missing one.
                 :doc (let ((candidate (car (last (remove-if (lambda (d) (> (car d) ms))
                                                             docs)))))
                        (when (and candidate
                                   (every (lambda (c)
                                            (member c '(#\Space #\Tab #\Newline #\Return)))
                                          (subseq code (car candidate) ms)))
                          (cdr candidate)))
                 :header header)
                result)))
      (nreverse result))))

;;; ---------------------------------------------------------------------------
;;; Exported-symbol discovery
;;; ---------------------------------------------------------------------------

(defun exported-symbols (lib-dir)
  "Every cd*/wd* code symbol libcd exports, as a hash of name -> T.

From nm(1) on the BUILT LIBRARY, not from the headers. Headers declare things
no library provides: the previous binding had ten such entries --
cdCanvasBezier, cdCanvasSpline and cdCanvasTextBounds among them -- each a
function that existed until you called it."
  (let ((table (make-hash-table :test #'equal))
        (dir (uiop:ensure-directory-pathname lib-dir)))
    (block found
      (dolist (name '("libcd.dylib" "libcd.so" "libcd.5.dylib"
                      "libcd.so.5" "cd.dll"))
        (let ((path (merge-pathnames name dir)))
          (when (probe-file path)
            (dolist (line (uiop:split-string
                           (uiop:run-program (list "nm" "-gU" (namestring path))
                                             :output :string
                                             :ignore-error-status t)
                           :separator '(#\Newline)))
              (let* ((fields (remove "" (uiop:split-string (string-trim " " line)
                                                           :separator '(#\Space))
                                     :test #'string=))
                     (kind (second fields))
                     (sym (third fields)))
                ;; Code symbols only. Exported data has no signature to bind,
                ;; so counting it as unbound would leave a permanent false
                ;; entry in the coverage report.
                (when (and sym kind (string= kind "T") (plusp (length sym)))
                  (let ((clean (if (char= (char sym 0) #\_) (subseq sym 1) sym)))
                    (when (split-prefix clean)
                      (setf (gethash clean table) t))))))
            (return-from found)))))
    table))

;;; ---------------------------------------------------------------------------
;;; Emission
;;; ---------------------------------------------------------------------------

(defun render (object)
  "Print OBJECT the way a Lisp programmer would have typed it.

The default printer upcases symbols, so :string comes out :STRING; worse, ~A
on a keyword drops the colon, and (x STRING) is not a CFFI type but a free
variable that happens to compile. Non-keyword symbols print unqualified: they
name types that will be interned in CD.FFI when the output is read, but exist
here as CD.GEN symbols, and PRIN1 would write cd.gen::cd-canvas."
  (etypecase object
    (keyword (string-downcase (prin1-to-string object)))
    (string object)
    (symbol (string-downcase (symbol-name object)))))

(defun render-type (type)
  (if (and (consp type) (eq :todo (car type)))
      (format nil ":pointer #| ~A |#" (string-trim " " (second type)))
      (render type)))

(defun c-integer (text)
  "A C integer literal as a Lisp one. 0x100 is a symbol in Lisp; #x100 is 256."
  (let ((s (string-trim " " text)))
    (cond ((and (> (length s) 2) (string-equal "0x" (subseq s 0 2)))
           (format nil "#x~A" (subseq s 2)))
          (t s))))

(defun escape-string-body (text)
  (with-output-to-string (s)
    (loop for c across text
          do (when (or (char= c #\") (char= c #\\)) (write-char #\\ s))
             (write-char c s))))

(defun wrap-docstring (doc indent)
  (when doc
    (let ((words (remove "" (uiop:split-string (escape-string-body doc)
                                               :separator '(#\Space))
                         :test #'string=))
          (lines '()) (current ""))
      (dolist (w words)
        (cond ((string= current "") (setf current w))
              ((> (+ (length current) 1 (length w)) 74)
               (push current lines) (setf current w))
              (t (setf current (concatenate 'string current " " w)))))
      (when (plusp (length current)) (push current lines))
      (setf lines (nreverse lines))
      (when lines
        (format nil "~A\"~{~A~^~%~}\""
                (make-string indent :initial-element #\Space) lines)))))

(defun power-of-two-p (n) (and (integerp n) (plusp n) (zerop (logand n (1- n)))))

(defun bitfield-p (members)
  "True when MEMBERS look like flags: every value explicit and a distinct power
of two. Accepts CD's capability bits; rejects sequences like the paper sizes."
  (and members (every #'cdr members)
       (let ((values (mapcar (lambda (m)
                               (let ((text (c-integer (cdr m))))
                                 (ignore-errors
                                  (if (and (> (length text) 2)
                                           (string= "#x" (subseq text 0 2)))
                                      (parse-integer text :start 2 :radix 16)
                                      (parse-integer text)))))
                             members)))
         (and (every #'power-of-two-p values)
              (= (length values) (length (remove-duplicates values)))))))

(defun enum-keyword (group member prefix)
  (let ((tail (if (and (plusp (length prefix))
                       (alexandria:starts-with-subseq prefix member))
                  (subseq member (length prefix))
                  member)))
    (string-downcase (format nil ":~A-~A" group (substitute #\- #\_ tail)))))

(defun emit-enum (stream group members)
  (let ((prefix (common-member-prefix (mapcar #'car members)))
        (bitfield (bitfield-p members)))
    (format stream "~%(cffi:~A ~A" (if bitfield "defbitfield" "defcenum") group)
    (dolist (m members)
      (let ((kw (enum-keyword group (car m) prefix)))
        (if (cdr m)
            (format stream "~%  (~A ~A)" kw (c-integer (cdr m)))
            (format stream "~%  ~A" kw))))
    (format stream ")~%")))

(defun emit-defcfun (stream decl)
  (let ((varargs (find :varargs (decl-params decl) :key #'cdr))
        (todo (or (and (consp (decl-return decl)) (eq :todo (car (decl-return decl))))
                  (some (lambda (p) (and (consp (cdr p)) (eq :todo (car (cdr p)))))
                        (decl-params decl)))))
    (when todo
      (format stream "~%;; REVIEW: unmapped C type(s) below; check against ~A~%"
              (decl-header decl)))
    (format stream "~%(cffi:defcfun (\"~A\" ~A) ~A"
            (decl-name decl) (lisp-fn-name (decl-name decl))
            (render-type (decl-return decl)))
    (let ((doc (wrap-docstring (decl-doc decl) 2)))
      (when doc (format stream "~%~A" doc)))
    (dolist (p (decl-params decl))
      (if (eq (cdr p) :varargs)
          (format stream "~%  &rest")
          (format stream "~%  (~A ~A)" (car p) (render-type (cdr p)))))
    (format stream ")~%")
    (values varargs)))

(defparameter *header-groups*
  '(("cd"        "cd.h")
    ("cd-wd"     "wd.h")
    ("cd-image"  "cdimage.h")
    ("cd-im"     "cdim.h")
    ("cd-svg"    "cdsvg.h")
    ("cd-ps"     "cdps.h")
    ("cd-pdf"    "cdpdf.h")
    ("cd-cgm"    "cdcgm.h")
    ("cd-dxf"    "cddxf.h")
    ("cd-dgn"    "cddgn.h")
    ("cd-mf"     "cdmf.h")
    ("cd-picture" "cdpicture.h")
    ("cd-debug"  "cddebug.h")
    ("cd-gl"     "cdgl.h")
    ("cd-native" "cdnative.h")
    ("cd-clipboard" "cdclipbd.h")
    ("cd-dbuffer" "cddbuf.h")
    ("cd-irgb"   "cdirgb.h")
    ("cd-print"  "cdprint.h")
    ("cd-quartz" "cdquartz.h")
    ("cd-cairo"  "cdcairo.h"))
  "Output file <- upstream headers, one coherent area each.")

(defparameter *private-headers*
  '("cd_private.h" "cdmf_private.h" "cdlua3_private.h" "cdlua5_private.h"
    "cd_old.h" "wd_old.h" "cd_plus.h" "cd_canvas.hpp"
    "cdlua.h" "cdluagl.h" "cdluaim.h" "cdluaiup.h" "cdluapdf.h")
  "Headers whose declarations are not part of the public API.

Two different reasons, both meaning \"do not bind\":

  cd_private.h and the *_private.h family declare what a DRIVER author needs
  -- the simulation layer (cdSimArc, cdSimulationText), the internal string
  and directory helpers (cdStrDup, cdMakeDirectory), the TrueType glue
  (cdTT_load), the geometry used to implement primitives (cdMatrixMultiply,
  cdGetArcPath). All of them are exported by libcd, because it is one shared
  object with no visibility control, but none is a supported entry point.

  cd_old.h and wd_old.h are the pre-5.0 global \"active canvas\" API: cdLine
  draws on whichever canvas cdActivate last selected. Deliberately excluded,
  not merely unbound -- it cannot be made safe with more than one canvas or
  more than one thread, and every one of its functions has a cdCanvas*
  counterpart that takes the canvas explicitly.

  The Lua and C++ headers are bindings of their own.")

(defun public-declarations (include-dir)
  "Every cd*/wd* name the PUBLIC headers declare.

This is what makes the coverage report mean something. libcd exports 459 such
symbols and only 227 belong to the public API; without separating the two, a
report of \"232 unbound\" is noise that trains the reader to ignore it."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (path (directory (merge-pathnames "*.h" include-dir)) table)
      (unless (member (file-namestring path) *private-headers* :test #'string=)
        (let ((text (blank-comments (read-header path))))
          (cl-ppcre:do-matches-as-strings
              (m "\\b(?:cdf|wdf|cd|wd)[A-Z][A-Za-z0-9_]*\\s*\\(" text)
            (let ((name (string-trim '(#\Space #\Tab #\() m)))
              (setf (gethash name table) t))))))))

(defparameter *build-directories* '("build-local/" "build/" "build-fix/" "build-im/")
  "Where to look for a built libcd, in order of preference.

Which symbols exist depends on how CD was configured: the drivers are compiled
into libcd per CMake option, so a build with CD_ENABLE_IM=OFF exports no
cdCanvasPutImImage and the generator would correctly emit no IM bridge.
Generate against the most fully-featured build available, or pass :LIB-DIR.")

(defun %find-build-directory (root)
  "The first of *BUILD-DIRECTORIES* under ROOT that holds a libcd."
  (or (dolist (candidate *build-directories*)
        (let ((dir (merge-pathnames candidate root)))
          (when (some (lambda (name) (probe-file (merge-pathnames name dir)))
                      '("libcd.dylib" "libcd.so" "cd.dll"))
            (return dir))))
      (merge-pathnames "build/" root)))

(defun generate (source-root &key (output "src/ffi/") (lib-dir nil))
  "Draft src/ffi/*.lisp from the headers under SOURCE-ROOT/include/."
  (let* ((root (uiop:ensure-directory-pathname source-root))
         (include (merge-pathnames "include/" root))
         (libs (uiop:ensure-directory-pathname
                (or lib-dir (%find-build-directory root))))
         (exports (exported-symbols libs))
         (out (uiop:ensure-directory-pathname output))
         (bound (make-hash-table :test #'equal)))
    (ensure-directories-exist out)
    (format t "~&Exported cd*/wd* symbols found: ~D~%" (hash-table-count exports))
    (when (zerop (hash-table-count exports))
      (cl:error "No symbols found in ~A -- build CD first, or pass :lib-dir." libs))

    (with-open-file (s (merge-pathnames "types.lisp" out)
                       :direction :output :if-exists :supersede)
      (format s ";;;; src/ffi/types.lisp — DRAFTED by tools/gen-bindings.lisp.~%")
      (format s ";;;;~%;;;; CD's opaque handles. Each is a :POINTER underneath, but naming~%")
      (format s ";;;; them makes a signature say which kind it wants.~%~%")
      (format s "(in-package #:cd.ffi)~%~%")
      (dolist (entry *opaque-types*)
        (format s "(cffi:defctype ~A :pointer)   ; ~A*~%"
                (render (cdr entry)) (car entry))))

    ;; Enums first: MAP-TYPE consults *ENUM-TYPES*.
    (dolist (group *header-groups*)
      (dolist (h (rest group))
        (let ((path (merge-pathnames h include)))
          (when (probe-file path)
            (dolist (e (parse-enums (read-header path)))
              (setf (gethash (first e) *enum-types*) (first e)))))))

    (dolist (group *header-groups*)
      (destructuring-bind (file &rest headers) group
        (let ((decls '()) (enums '()))
          (dolist (h headers)
            (let ((path (merge-pathnames h include)))
              (when (probe-file path)
                (let ((text (read-header path)))
                  (setf enums (append enums (parse-enums text)))
                  (dolist (d (parse-decls text h))
                    ;; Bind only what the library exports.
                    (when (and (gethash (decl-name d) exports)
                               (not (gethash (decl-name d) bound)))
                      (setf (gethash (decl-name d) bound) t)
                      (push d decls)))))))
          (setf decls (nreverse decls))
          (when (or decls enums)
            (with-open-file (s (merge-pathnames (format nil "~A.lisp" file) out)
                               :direction :output :if-exists :supersede)
              (format s ";;;; src/ffi/~A.lisp — DRAFTED by tools/gen-bindings.lisp.~%" file)
              (format s ";;;;~%;;;; Source: ~{~A~^, ~}~%" headers)
              (format s ";;;; Hand corrections below are expected and are kept; re-run the~%")
              (format s ";;;; generator into a clean tree and diff.~%~%")
              (format s "(in-package #:cd.ffi)~%")
              (dolist (e enums) (emit-enum s (first e) (second e)))
              (dolist (d decls) (emit-defcfun s d)))
            (format t "~&  ~A.lisp: ~D function~:P, ~D enum~:P~%"
                    file (length decls) (length enums))))))

    (with-open-file (s (merge-pathnames "manifest.lisp" out)
                       :direction :output :if-exists :supersede)
      (format s ";;;; src/ffi/manifest.lisp — DRAFTED by tools/gen-bindings.lisp.~%")
      (format s ";;;;~%;;;; Every C function this binding declares. The test suite checks at~%")
      (format s ";;;; RUNTIME that each still resolves in the loaded libcd, which is what~%")
      (format s ";;;; catches a binding to a function upstream has removed.~%~%")
      (format s "(in-package #:cd.ffi)~%~%(defparameter *bindings*~%  '(")
      (let ((first t))
        (maphash (lambda (sym v) (declare (ignore v))
                   (format s "~:[~%    ~;~]\"~A\"" first sym)
                   (setf first nil))
                 bound))
      (format s "))~%"))

    (coverage-report exports bound (public-declarations include))))

(defun coverage-report (exports bound public)
  "Print, and return, the PUBLIC symbols exported but not bound.

Exports that no public header declares are counted separately: they are driver
internals and legacy globals, and listing them as gaps would bury the ones
that matter."
  (let (missing internal)
    (maphash (lambda (sym v) (declare (ignore v))
               (unless (gethash sym bound)
                 (if (gethash sym public)
                     (push sym missing)
                     (push sym internal))))
             exports)
    (setf missing (sort missing #'string<))
    (format t "~&~%Coverage~%")
    (format t "  bound:                      ~D~%" (hash-table-count bound))
    (format t "  exported by libcd:          ~D~%" (hash-table-count exports))
    (format t "  internal / legacy (skipped): ~D~%" (length internal))
    (format t "  PUBLIC BUT UNBOUND:         ~D~%" (length missing))
    (dolist (m missing) (format t "    UNBOUND ~A~%" m))
    missing))
