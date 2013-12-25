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
(foreign-declare "#include <pthread.h>")
(foreign-declare "
struct thread_data {
  int index;
  int length;
  DP ** dps;
  pthread_mutex_t lock;
};

void * process_image(void * z) {
  struct thread_data * f = (struct thread_data *)z;

  int job;

  for(;;) {
    int res;
    ulong64 hash;
    DP *dp;

    if(f->index >= f->length)
      break;

    pthread_mutex_lock(&(f->lock));
    job = f->index;
    f->index++;
    pthread_mutex_unlock(&(f->lock));

    dp = f->dps[job];
    res = ph_dct_imagehash(dp->id, hash);
    dp->hash = (ulong64*)malloc(sizeof(hash));
    memcpy(dp->hash, &hash, sizeof(hash));
    dp->hash_length = 1;
  }
}

DP ** ch_ph_dct_image_hashes(void **files, int count, int threads) {
  pthread_t my_threads[threads];
  struct thread_data data;

  data.index  = 0;
  data.length = count;
  data.dps = (DP**)malloc(count*sizeof(DP*));

  pthread_mutex_init(&data.lock, NULL);

  for(int i = 0; i < count; ++i)
  {
    data.dps[i] = (DP *)malloc(sizeof(DP));
    data.dps[i]->id = strdup((char *)files[i]);
  }

  for(int i = 0; i < threads; i++) {
    pthread_create(&my_threads[i], NULL, process_image, &data);
  }
  for(int i = 0; i < threads; i++) {
    pthread_join(my_threads[i], NULL);
  }
  pthread_mutex_destroy(&data.lock);
  return data.dps;
}")

(foreign-declare "DP * get_dp(void **dps, int i) { return (DP *)dps[i]; }")

(foreign-declare "void * mk_ptr(char * str, int len) {
  void * s = calloc(len, sizeof(char));
  memcpy(s, str, len);
  return (void *)s;
}")

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
          (loop (- x 1) (cons point seed))))))

(define (dct-image-hashes images threads)
  (let ((pv (make-pointer-vector (length images))))
    (let loop ((elem (car images))
               (rest (cdr images))
               (idx 0))
      (pointer-vector-set! pv idx (mk-ptr elem (string-length elem)))
      (if (null? rest)
        (let* ((len (length images))
               (pointer (ph_dct_image_hashes pv len threads))
               (points (make-datapoints pointer len)))
          points)
        (loop (car rest) (cdr rest) (+ 1 idx))))))

(define-foreign-type DP (c-pointer "DP"))

(define mk-ptr (foreign-lambda c-pointer "mk_ptr" c-string int))

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
