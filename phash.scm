;;;; phash.scm
;;;; Bindings to pHash

(module phash
  (dct-imagehash
   dct-image-hashes
   datapoint.file
   datapoint.hash
   hamming-distance)

(import scheme chicken foreign)
(use lolevel)

(foreign-declare "#include <pHash.h>")
(foreign-declare "
DP ** ch_ph_dct_image_hashes(void **files, int count, int threads) {
  return ph_dct_image_hashes((char **)files, count, threads);
}")

(foreign-declare "DP * get_dp(void **dps, int i) { return (DP *)dps[i]; }")

(foreign-declare "void * mk_ptr(char * str) { return (void *)str; }")

(define-record-type datapoint
  (wrap-datapoint file hash)
  datapoint?
  (file datapoint.file)
  (hash datapoint.hash))

(define (dct-imagehash filename)
  (let-location ((i unsigned-integer64 0))
                (if (= -1 (ph_dct_imagehash filename (location i)))
                    (abort "something went wrong")
                    i)))

(define (make-datapoint ptr)
        (wrap-datapoint (dp->id ptr) (dp->hash ptr)))

(define (make-datapoints dp len)
  (let loop ((x len)
             (seed '()))
    (if (= 0 x)
        seed
        (let* ((ptr (get-dp dp (- x 1)))
               (point (make-datapoint ptr)))
          (ph_free_datapoint ptr)
          (loop (- x 1) (cons point seed))))))

(define (dct-image-hashes images)
  (let ((pv (make-pointer-vector (length images))))
    (let loop ((elem (car images))
               (rest (cdr images))
               (idx 0))
      (pointer-vector-set! pv idx (mk-ptr elem))
      (if (null? rest)
        (let* ((pointer (ph_dct_image_hashes pv (length images) 1))
               (points (make-datapoints pointer (length images))))
          (free pointer)
          points)
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

(define get-dp (foreign-lambda c-pointer
                                         "get_dp"
                                         (c-pointer c-pointer)
                                         int))

(define ph_free_datapoint (foreign-lambda void
                                         "ph_free_datapoint"
                                         DP))
(define dp->id (foreign-lambda* c-string
                                ((DP datapoint))
                                "C_return(datapoint->id);"))

(define dp->hash_length (foreign-lambda* unsigned-int32 
                                ((DP datapoint))
                                "C_return(datapoint->hash_length);"))

(define dp->hash (foreign-lambda* unsigned-integer64 
                                ((DP datapoint))
                                "C_return(*((ulong64*)datapoint->hash));"))

)
