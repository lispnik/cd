;;;; src/library.lisp — finding, opening and re-opening libcd.
;;;;
;;;; Simpler than IM's equivalent, because CD ships as one library: the
;;;; drivers -- PostScript, SVG, PDF, Cairo, Quartz, GL -- are compiled into
;;;; libcd according to CMake options rather than shipped as separate objects.
;;;; So there is nothing to choose between at load time, and the interesting
;;;; question is not which libraries opened but which drivers the one library
;;;; turned out to contain. See DRIVERS.
;;;;
;;;; The image dump and restore hooks are not simpler, and are not optional:
;;;; CFFI's record of an open library survives SAVE-LISP-AND-DIE, so without
;;;; them a restored image believes libcd is open, never calls dlopen, and
;;;; binds to whatever the loader happened to provide.

(in-package #:cd)

(export '(*library-path*
          library-pathname
          library-loaded-p
          load-libraries
          version
          version-date))

(defvar *library-path* nil
  "Directory to load libcd from, or NIL to search.

Overrides every other candidate. The CD_LIBRARY_PATH environment variable does
the same and is read at load time.")

(cffi:define-foreign-library lib-cd
  ;; Upstream builds libcd.5.14.0.dylib with libcd.dylib and libcd.5.dylib
  ;; beside it. Naming the unversioned link first keeps a source build and an
  ;; installed one on the same path; the versioned names are the fallback for
  ;; an install that ships only those.
  (:darwin (:or "libcd.dylib" "libcd.5.dylib"))
  (:unix (:or "libcd.so" "libcd.so.5"))
  (:windows (:or "cd.dll" "libcd.dll"))
  (t (:default "cd")))

(defvar *loaded* nil
  "Namestring libcd was loaded from, or NIL.")

(defun library-pathname () *loaded*)
(defun library-loaded-p () (and *loaded* t))

(defun %executable-library-directories ()
  "Directories to search relative to this executable, nearest first.

Both <exedir>/ and <exedir>/../lib/, so one binary works with a flat bundle or
with the bin/ + lib/ shape. NIL when running from source, where argv[0] is the
Lisp itself."
  (let ((argv0 (ignore-errors (uiop:argv0))))
    (when argv0
      (let* ((exe (ignore-errors (uiop:truename* argv0)))
             (dir (when exe (uiop:pathname-directory-pathname exe))))
        (when dir
          (remove nil
                  (list (uiop:truename* dir)
                        (let ((lib (uiop:merge-pathnames* #p"../lib/" dir)))
                          (when (uiop:directory-exists-p lib)
                            (uiop:truename* lib))))))))))

(defun %search-directories ()
  "Directories to look in, most specific first.

Deliberately contains no guess at anyone's home directory: the previous
binding's hardcoded paths made it load on one machine and fail everywhere
else, with the failure looking like a missing library rather than a wrong
assumption."
  (remove nil
          (append (list (when *library-path*
                          (uiop:ensure-directory-pathname *library-path*))
                        (let ((env (uiop:getenv "CD_LIBRARY_PATH")))
                          (when (and env (plusp (length env)))
                            (uiop:ensure-directory-pathname env))))
                  (%executable-library-directories))))

(defun %candidate-names ()
  "The file names CFFI would try here, for the LIBRARY-NOT-FOUND report.

A message that lists what was actually tried is the one thing that turns
\"cannot load the CD library\" into something the reader can go and check."
  (list #+darwin "libcd.dylib" #+darwin "libcd.5.dylib"
        #+(and unix (not darwin)) "libcd.so"
        #+(and unix (not darwin)) "libcd.so.5"
        #+windows "cd.dll" #+windows "libcd.dll"))

(defun load-libraries ()
  "Open libcd. Safe to call again."
  (let ((cffi:*foreign-library-directories*
          (append (%search-directories) cffi:*foreign-library-directories*)))
    (handler-case
        (let ((handle (cffi:load-foreign-library 'lib-cd)))
          (setf *loaded*
                (or (ignore-errors
                     (let ((p (cffi:foreign-library-pathname handle)))
                       (when p (namestring p))))
                    "libcd")))
      (cffi:load-foreign-library-error (e)
        (setf *loaded* nil)
        (cl:error 'library-not-found
                  :detail "libcd"
                  :candidates (append
                               (mapcar (lambda (d)
                                         (format nil "~A (directory searched)" d))
                                       (%search-directories))
                               (%candidate-names)
                               (list (princ-to-string e))))))))

;;; Version -------------------------------------------------------------------

(defun version ()
  "The CD library version as a string, e.g. \"5.14\"."
  (cd.ffi::%cd-version))

(defun version-date ()
  "The CD library's release date as a string."
  (cd.ffi::%cd-version-date))

;;; Image dump and restore ----------------------------------------------------

(defun %prepare-for-dump ()
  ;; Unload before dumping so the saved image carries no record of libcd.
  ;; Otherwise the loader reopens it by soname before any Lisp runs, and the
  ;; restore hook's open would be a no-op against whatever that found.
  (setf *loaded* nil)
  (ignore-errors (cffi:close-foreign-library 'lib-cd)))

(defun %reinitialize ()
  ;; Close first: CFFI may still believe the library is open from before the
  ;; dump, in which case LOAD-FOREIGN-LIBRARY returns without calling dlopen.
  (ignore-errors (cffi:close-foreign-library 'lib-cd))
  (setf *loaded* nil)
  (handler-case (load-libraries)
    (library-not-found (c)
      (format *error-output* "~&~A~%" c)
      (uiop:quit 1))))

(uiop:register-image-dump-hook '%prepare-for-dump)
(uiop:register-image-restore-hook '%reinitialize nil)

(load-libraries)
