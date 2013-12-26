;;;; phash.scm
;;;; Bindings to pHash

(module phash
  (dct-imagehash
   dct-image-hashes
   datapoint.file
   datapoint.hash
   hamming-distance)

(import scheme chicken foreign srfi-4)
(use lolevel srfi-4)

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

    pthread_mutex_lock(&(f->lock));
    if(f->index >= f->length) {
      pthread_mutex_unlock(&(f->lock));
      break;
    }
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
  (let ((vec (make-u32vector 2)))
    (if (= -1 (ph_dct_imagehash filename vec))
        (abort "something went wrong")
        vec)))

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

(define (pv-free pv len)
  (let loop ((stop len)
             (i 0))
    (if (not (= i len))
        (begin
          (free (pointer-vector-ref pv i))
          (loop len (+ 1 i))))))

(define (each-with-index cb collection)
  (let loop ((elem (car collection))
             (rest (cdr collection))
             (idx 0))
    (cb elem idx)
    (if (not (null? rest))
        (loop (car rest) (cdr rest) (+ 1 idx)))))

(define (dct-image-hashes images threads)
  (let* ((len (length images))
         (pv (make-pointer-vector len)))
    (each-with-index (lambda (elem idx)
                       (let ((pointer (mk-ptr elem (string-length elem))))
                         (pointer-vector-set! pv idx pointer)))
                     images)
    (let* ((pointer (ph_dct_image_hashes pv len threads))
           (points (make-datapoints pointer len)))
      (pv-free pv len)
      (free pointer)
      points)))

(define-foreign-type DP (c-pointer "DP"))

(define mk-ptr (foreign-lambda c-pointer "mk_ptr" c-string int))

(define ph_dct_image_hashes (foreign-lambda (c-pointer DP)
                                         "ch_ph_dct_image_hashes"
                                         pointer-vector
                                         int
                                         int))

(define ph_dct_imagehash (foreign-lambda* int 
                                ((nonnull-c-string file)
                                 (u32vector ret))
"
ulong64 hash;
int rv = ph_dct_imagehash(file, hash);
*ret = (hash >> 32) & 0xFFFFFFFF;
*(ret + 1) = hash & 0xFFFFFFFF;
C_return(rv);"))

(define hamming-distance (foreign-lambda* int
                                         ((u32vector lv)
                                          (u32vector rv))
"
ulong64 left;
ulong64 right;
left = *lv;
left <<= 32;
left += *(lv + 1);
right = *rv;
right <<= 32;
right += *(rv + 1);
C_return(ph_hamming_distance(left, right));
"))

(define get-dp (foreign-lambda c-pointer
                                         "get_dp"
                                         (c-pointer c-pointer)
                                         int))

(define dp->id (foreign-lambda* c-string
                                ((DP datapoint))
                                "C_return(datapoint->id);"))

(define (dp->hash dp)
  (let ((vec (make-u32vector 2)))
    (dp_hash dp vec)
    vec))

(define dp_hash (foreign-lambda* void 
                                ((DP datapoint)
                                 (u32vector ret))
                                "ulong64 hash = *(ulong64 *)datapoint->hash;
                                *ret = (hash >> 32) & 0xFFFFFFFF;
                                *(ret + 1) = hash & 0xFFFFFFFF;"))

)
