(in-package :cl-user)

(defun random-bits (nr-bits &key (density 0.5))
  "Creates a random bit vector of the specified size. DENSITY is the probability of a 1
bit in the result (should be 0.5 or less)."
  (assert (<= density 0.5))
  (let ((a (make-array nr-bits :element-type 'bit :initial-element 0))
        (n (round (/ 1.0 density))))
    (loop for i below nr-bits
         do (when (= 1 (random n))
              (setf (aref a i) 1)))
    a))

(defun save-bits (bits filename)
  (with-open-file (out filename :direction :output :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    ;; First save the number of bits (as 4 unsigned bytes, most significant byte first).
    (let ((nr-bits (length bits)))
      (write-byte (ldb (byte 8 24) nr-bits) out)
      (write-byte (ldb (byte 8 16) nr-bits) out)
      (write-byte (ldb (byte 8  8) nr-bits) out)
      (write-byte (ldb (byte 8  0) nr-bits) out)
      ;; Then save the actual bits.
      (loop for i below nr-bits by 8
         do (write-byte (fetch-byte bits i nr-bits) out))
      out)))

(defun fetch-byte (bitvec pos nr-bits)
  "Returns an 8-bit integer that corresponds to the 8 bits in BITVEC starting at position POS."
  (let ((byte (* 128 (aref bitvec pos))))
    (incf pos)
    (when (< pos nr-bits)
      (incf byte (* 64 (aref bitvec pos)))
      (incf pos)
      (when (< pos nr-bits)
        (incf byte (* 32 (aref bitvec pos)))
        (incf pos)
        (when (< pos nr-bits)
          (incf byte (* 16 (aref bitvec pos)))
          (incf pos)
          (when (< pos nr-bits)
            (incf byte (* 8 (aref bitvec pos)))
            (incf pos)
            (when (< pos nr-bits)
              (incf byte (* 4 (aref bitvec pos)))
              (incf pos)
              (when (< pos nr-bits)
                (incf byte (* 2 (aref bitvec pos)))
                (incf pos)
                (when (< pos nr-bits)
                  (incf byte (aref bitvec pos)))))))))
    byte))


(defun load-bits (filename)
  (with-open-file (in filename :direction :input :element-type '(unsigned-byte 8))
    ;; Read the number of bits in the bit vector.
    (let ((nr-bits 0))
      (setq nr-bits (dpb (read-byte in :eof-error-p t) (byte 8 24) nr-bits))
      (setq nr-bits (dpb (read-byte in :eof-error-p t) (byte 8 16) nr-bits))
      (setq nr-bits (dpb (read-byte in :eof-error-p t) (byte 8  8) nr-bits))
      (setq nr-bits (dpb (read-byte in :eof-error-p t) (byte 8  0) nr-bits))
      ;; Load the actual bits.
      (let ((a (make-array nr-bits :element-type 'bit :initial-element 0)))
        (loop for i below nr-bits by 8
           do (let ((byte (read-byte in nil nil)))
                (when byte
                  (put-byte a i byte nr-bits))))
        a))))


(defun put-byte (bitvec pos byte nr-bits)
  "Puts the 8 bits from BYTE in the BITVEC, starting at position POS."
  (when (logbitp 7 byte)
    (setf (aref bitvec pos) 1))
  (incf pos)
  (when (< pos nr-bits)
    (when (logbitp 6 byte)
      (setf (aref bitvec pos) 1))
    (incf pos)
    (when (< pos nr-bits)
      (when (logbitp 5 byte)
        (setf (aref bitvec pos) 1))
      (incf pos)
      (when (< pos nr-bits)
        (when (logbitp 4 byte)
          (setf (aref bitvec pos) 1))
        (incf pos)
        (when (< pos nr-bits)
          (when (logbitp 3 byte)
            (setf (aref bitvec pos) 1))
          (incf pos)
          (when (< pos nr-bits)
            (when (logbitp 2 byte)
              (setf (aref bitvec pos) 1))
            (incf pos)
            (when (< pos nr-bits)
              (when (logbitp 1 byte)
                (setf (aref bitvec pos) 1))
              (incf pos)
              (when (< pos nr-bits)
                (when (logbitp 0 byte)
                  (setf (aref bitvec pos) 1))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun test-compression ()
  "Create bit vectors with 1-bit densities varying from 50% to 0.1% and save each to a
separate file."
  (loop for density in '(0.5 0.2 0.1 0.05 0.02 0.01)
       do (let ((bits (random-bits (* 100 1000000) :density density))
                (filename (format nil "/tmp/bits~d.lsp" density)))
            (save-bits bits filename))))


(defun test-decompression (&key (nr-bits (* 100 1000000)))
  (dolist (density '(0.5 0.2 0.1 0.05 0.02 0.01))
    (let ((filename (format nil "/tmp/bits/bits~d.zip" density)))
      (format t "~&Loading ~S~%" filename)
      (let ((bytes (time (load-zip-file filename)))
            (a (make-array nr-bits :element-type 'bit :initial-element 0)))
        (time
         (loop for byte across bytes
            for i below nr-bits by 8
            do (put-byte a i byte)))))))

(defun test-uncompressed (&key (nr-bits (* 100 1000000)))
  (dolist (density '(0.5 0.2 0.1 0.05 0.02 0.01))
    (let ((filename (format nil "/tmp/bits/bits~d.lsp" density)))
      (format t "~&Loading ~S~%" filename)
      (test-load filename nr-bits))))
  
(defun load-zip-file (filename)
  "Returns an (UNSIGNED-BYTE 8) vector with the contents of the first entry in
the zip file called FILENAME."
  (zip:with-zipfile (zip filename)
    (zip:zipfile-entry-contents (first (zipfile-entries-as-list zip)))))

(defun zipfile-entries-as-list (zip)
  (let ((table (zip:zipfile-entries zip)))
    (loop for entry being the hash-value of table
         collect entry)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-bits (&key (n 2) (nr-bits 1000000))
  "Çreate N random bit vectors with the specified number of bits, then time
how long it takes to compute the BIT-AND of those vectors."
  (let ((vectors (loop repeat n
                      collect (random-bits nr-bits))))
    (time
     (let ((result (first vectors)))
       (loop for vector in (rest vectors)
            do (setq result (bit-and result vector)))))))

(defun test-save (filename &key (nr-bits 100000000))
  "Çreate a random bit vector with the specified number of bits and write
it to a file."
  (let ((a (random-bits nr-bits)))
    (time (save-bits a filename))))

(defun test-load (filename)
  (time (load-bits filename)))

(defun check-save-load (&key (filename "/tmp/bits.bits") (n 1000))
  "Test that LOAD-BITS returns the same bit vectors that SAVE-BITS saved."
  (loop for i below n do
       (let* ((size (random 10000))
              (bits1 (random-bits size)))
         (format t "~&~5D: ~5D bits~%" i size)
         (save-bits bits1 filename)
         (let ((bits2 (load-bits filename)))
         (assert (equalp bits1 bits2))))))


(defun test ()
  ;; If I try 1 billion bits, I get "1000000000 is an illegal dimension".
  ;; But 500 million works fine.
  (loop for nr-millions in '(1 10 100 500)
       do (test-bits :nr-bits (* nr-millions 1000000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Intersecting two bit vectors of 100M bits takes ~0.01 second on my Windows laptop.
Intersecting two bit vectors of 500M bits takes 0.05-0.06 seconds.

Writing a 100M bit vector to a file takes 3-4 seconds on my laptop.
Writing a 500M bit vector takes 15-16 seconds.

Loading a 100M bit vector from a file takes ~2.5 seconds.
Loading a 500M bit vector takes 12-13 seconds.

The compression rate of the saved bit files depends on the density of 1-bits:

 1%        1,601,070 bits0.01.zip
 2%        2,584,341 bits0.02.zip
 5%        4,695,754 bits0.05.zip
10%        7,018,392 bits0.1.zip
20%        9,706,111 bits0.2.zip
50%       12,503,935 bits0.5.zip

The 1-bit density is related to the number of possible values for the corresponding
predicate. The more possible values, the smaller the bit density will be on average.
E.g. if a predicate has 100 possible values, the average bit density for one bitset
\(which respresents the set of persons having one particular value) will be 1/100.

* Loading uncompressed vs. compressed bit vectors:

CL-USER> (test-uncompressed)
Loading "/tmp/bits/bits0.5.lsp"
; cpu time (non-gc) 2.312500 sec user, 0.031250 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  2.312500 sec user, 0.031250 sec system
; real time  2.348000 sec (99.82%)
; space allocation:
;  166 cons cells, 12,505,120 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.2.lsp"
; cpu time (non-gc) 1.703125 sec user, 0.046875 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.703125 sec user, 0.046875 sec system
; real time  1.760000 sec (99.43%)
; space allocation:
;  165 cons cells, 12,507,496 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.1.lsp"
; cpu time (non-gc) 1.500000 sec user, 0.078125 sec system
; cpu time (gc)     0.015625 sec user, 0.000000 sec system
; cpu time (total)  1.515625 sec user, 0.078125 sec system
; real time  1.579000 sec (100.9%)
; space allocation:
;  118 cons cells, 12,502,368 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.05.lsp"
; cpu time (non-gc) 1.390625 sec user, 0.078125 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.390625 sec user, 0.078125 sec system
; real time  1.476000 sec (99.51%)
; space allocation:
;  167 cons cells, 12,518,960 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.02.lsp"
; cpu time (non-gc) 1.359375 sec user, 0.046875 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.359375 sec user, 0.046875 sec system
; real time  1.409000 sec ( 99.8%)
; space allocation:
;  166 cons cells, 12,507,520 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.01.lsp"
; cpu time (non-gc) 1.343750 sec user, 0.031250 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.343750 sec user, 0.031250 sec system
; real time  1.378000 sec (99.78%)
; space allocation:
;  103 cons cells, 12,501,728 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)


CL-USER> (test-decompression)
Loading "/tmp/bits/bits0.5.zip"
; cpu time (non-gc) 0.703125 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  0.703125 sec user, 0.000000 sec system
; real time  0.711000 sec (98.89%)
; space allocation:
;  64 cons cells, 12,610,192 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
; cpu time (non-gc) 2.375000 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  2.375000 sec user, 0.000000 sec system
; real time  2.385000 sec (99.58%)
; space allocation:
;  81 cons cells, 34,368 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.2.zip"
; cpu time (non-gc) 6.437500 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  6.437500 sec user, 0.000000 sec system
; real time  6.428000 sec (100.1%)
; space allocation:
;  2,782,073 cons cells, 15,149,264 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
; cpu time (non-gc) 1.640625 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.640625 sec user, 0.000000 sec system
; real time  1.652000 sec (99.31%)
; space allocation:
;  81 cons cells, 34,192 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.1.zip"
; cpu time (non-gc) 4.218750 sec user, 0.000000 sec system
; cpu time (gc)     0.015625 sec user, 0.000000 sec system
; cpu time (total)  4.234375 sec user, 0.000000 sec system
; real time  4.237000 sec (99.94%)
; space allocation:
;  1,199,391 cons cells, 13,940,640 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
; cpu time (non-gc) 1.437500 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.437500 sec user, 0.000000 sec system
; real time  1.440000 sec (99.83%)
; space allocation:
;  78 cons cells, 14,272 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.05.zip"
; cpu time (non-gc) 3.000000 sec user, 0.000000 sec system
; cpu time (gc)     0.015625 sec user, 0.000000 sec system
; cpu time (total)  3.015625 sec user, 0.000000 sec system
; real time  3.019000 sec (99.89%)
; space allocation:
;  551,163 cons cells, 13,451,736 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
; cpu time (non-gc) 1.406250 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.406250 sec user, 0.000000 sec system
; real time  1.405000 sec (100.1%)
; space allocation:
;  64 cons cells, 5,576 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.02.zip"
; cpu time (non-gc) 1.953125 sec user, 0.000000 sec system
; cpu time (gc)     0.015625 sec user, 0.000000 sec system
; cpu time (total)  1.968750 sec user, 0.000000 sec system
; real time  1.982000 sec (99.33%)
; space allocation:
;  222,590 cons cells, 13,078,720 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
; cpu time (non-gc) 1.265625 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.265625 sec user, 0.000000 sec system
; real time  1.271000 sec (99.58%)
; space allocation:
;  64 cons cells, 5,576 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
Loading "/tmp/bits/bits0.01.zip"
; cpu time (non-gc) 1.421875 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.421875 sec user, 0.000000 sec system
; real time  1.416000 sec (100.4%)
; space allocation:
;  105,320 cons cells, 12,880,168 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
; cpu time (non-gc) 1.218750 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  1.218750 sec user, 0.000000 sec system
; real time  1.221000 sec (99.82%)
; space allocation:
;  0 cons cells, 0 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)

|#
