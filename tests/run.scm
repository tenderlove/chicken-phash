(use phash test srfi-1 posix)

(test-begin "phash")

(test-group "hashes"
  (test-assert (dct-imagehash "cremate.jpg"))
  (test-error (dct-imagehash "asdflkjhasdlkfjh"))

  (test-group "hamming distance"
    (let ((left (dct-imagehash "cremate.jpg"))
          (right (dct-imagehash "cremate.jpg")))
      (test 0 (hamming-distance left right)))
    (let ((left (dct-imagehash "cremate.jpg"))
          (right (dct-imagehash "cremate2.jpg")))
      (test-assert (hamming-distance left right)))))

(test-end)
(test-exit)
