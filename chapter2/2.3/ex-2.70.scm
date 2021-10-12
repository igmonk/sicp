;; Exercise 2.70
;;
;; The following eight-symbol alphabet with associated relative frequencies
;; was designed to efficiently encode the lyrics of 1950s rock songs.
;;
;; (Note that the 'symbols' of an 'alphabet' need not be individual letters.)
;;
;; A    2    NA  16
;; BOOM 1    SHA 3
;; GET  2    YIP 9
;; JOB  2    WAH 1
;;
;; Use generate-huffman-tree (ex. 2.69) to generate a corresponding Huffman tree,
;; and use encode (ex. 2.68) to encode the following message:
;;
;; Get a job
;; Sha na na na na na na na na
;; Get a job
;; Sha na na na na na na na na
;; Wah yip yip yip yip yip yip yip yip yip
;; Sha boom
;;
;; How many bits are required for the encoding?
;;
;; What is the smallest number of bits that would be needed to encode this song
;; if we used a fixed-length code for the eight-symbol alphabet?

(load "workbook.scm")
(load "ex-2.69.scm")
(load "ex-2.68.scm")

(define symbol-frequency-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2)
                                 (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define huffman-tree-1 (generate-huffman-tree symbol-frequency-pairs))

(define message '(Get a job
                      Sha na na na na na na na na
                      Get a job
                      Sha na na na na na na na na
                      Wah yip yip yip yip yip yip yip yip yip
                      Sha boom))

(define encoded (encode message huffman-tree-1))

;; Value (encoded):
;; 
;; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0
;;  0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1
;;  1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0
;;  1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
;;
;; (length encoded) ; 84

;; The smallest number of bits that is needed to encode a message
;; with a fixed-length code is found by the following formula:
;;
;; m * log2(n)
;;
;; Where:
;;   n is the size of the alphabet
;;   m is the number of symbols in a message
;;
;; Had we used a fixed-length code for the eight-symbol alphabet
;; to encode a message containing (length message) = 36 symbols,
;; it would have required
;;
;; 36 * log2(8) = 108 bits
