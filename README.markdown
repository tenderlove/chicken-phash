# pHash wrapper for Chicken scheme

This wraps [libpHash](http://www.phash.org/) to allow calculating perceptual
hashes from Chicken.

## Usage:

```scheme
(use phash)

; Calculate a hash for two files
(let ((left (dct-imagehash "cremate.jpg"))
      (right (dct-imagehash "cremate2.jpg")))

; Find the hamming distance (how similar they are to each other)
    (hamming-distance left right))
```

Lower hamming distance means more similar images.
