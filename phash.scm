;;;; phash.scm
;;;; Bindings to pHash

(module phash
  (dct-imagehash
   hamming-distance)

(import scheme chicken foreign)
(use lolevel)

(foreign-declare "#include <pHash.h>")

(define (dct-imagehash filename)
  (let-location ((i unsigned-integer64 0))
                (if (= -1 (ph_dct_imagehash filename (location i)))
                    (abort "something went wrong")
                    i)))

(define ph_dct_imagehash (foreign-lambda int
                                         "ph_dct_imagehash"
                                         nonnull-c-string
                                         (ref unsigned-integer64)))

(define hamming-distance (foreign-lambda int
                                         "ph_hamming_distance"
                                         unsigned-integer64
                                         unsigned-integer64))

)
