;;;; phash.scm
;;;; Bindings to pHash

(module phash
  (dct-imagehash
   dct-image-hashes
   hamming-distance)

(import scheme chicken foreign)
(use lolevel)

(foreign-declare "#include <pHash.h>")
(foreign-declare "
DP ** ch_ph_dct_image_hashes(void **files, int count, int threads) {
  return ph_dct_image_hashes((char **)files, count, threads);
}")

(foreign-declare "void * mk_ptr(char * str) { return (void *)str; }")

(define (dct-imagehash filename)
  (let-location ((i unsigned-integer64 0))
                (if (= -1 (ph_dct_imagehash filename (location i)))
                    (abort "something went wrong")
                    i)))

(define (dct-image-hashes images)
  (let ((pv (make-pointer-vector (length images))))
    (let loop ((elem (car images))
               (rest (cdr images))
               (idx 0))
      (pointer-vector-set! pv idx (mk-ptr elem))
      (if (null? rest)
        (ph_dct_image_hashes pv (length images) 1)
        (loop (car rest) (cdr rest) (+ 1 idx))))))

(define-foreign-type DP (c-pointer "DP"))

(define mk-ptr (foreign-lambda c-pointer
                               "mk_ptr"
                               c-string))

(define ph_dct_image_hashes (foreign-lambda (c-pointer DP)
                                         "ch_ph_dct_image_hashes"
                                         pointer-vector
                                         int
                                         int))

(define ph_dct_imagehash (foreign-lambda int
                                         "ph_dct_imagehash"
                                         nonnull-c-string
                                         (ref unsigned-integer64)))

(define hamming-distance (foreign-lambda int
                                         "ph_hamming_distance"
                                         unsigned-integer64
                                         unsigned-integer64))

)
