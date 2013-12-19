# pHash wrapper for Chicken scheme

This wraps libpHash to allow calculating perceptual hashes from Chicken.

## Usage:

```scheme
(use phash)

; Calculate a hash for two files
(let ((left (dct-imagehash "cremate.jpg"))
      (right (dct-imagehash "cremate.jpg")))

; Find the hamming distance (how similar they are to each other)
    (hamming-distance left right))
```

Lower hamming distance means more similar images.
