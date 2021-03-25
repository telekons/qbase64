(in-package :qbase64)

(defun char- (character offset)
  (- (char-code character) offset))

(defconstant +encode-shift-lut+
  (sse:setr-pi8
   (char- #\a 26) #1=(char- #\0 52) #1# #1# #1# #1#
   #1# #1# #1# #1# #1# (char- #\+ 62)
   (char- #\/ 63) (char-code #\A) 0 0))

(declaim (inline encode-lookup))
(defun encode-lookup (input)
  (let* ((result (sse:subs-pu8 input (sse:set1-pi8 51)))
         (less   (sse:>-pi8    (sse:set1-pi8 26) input))
         (result (sse:or-pi    result (sse:and-pi less (sse:set1-pi8 13))))
         (result (sse:shuffle-pi8 +encode-shift-lut+ result)))
    (sse:add-pi8 result input)))

(deftype array-index ()
  `(and unsigned-byte fixnum))

(defun %sse2-encode (input output)
  (declare ((simple-array (unsigned-byte 8) 1) input)
           (simple-base-string output)
           (optimize (speed 3) (safety 0)))
  (let ((bytes (length input))
        ;; A simple-base-string on SBCL stores one character per byte.
        (output-storage (sb-sys:vector-sap output))
        (shuf (sse:set-pi8 10 11 9 10
                           7 8 6 7
                           4 5 3 4
                           1 2 0 1)))
    (loop for i of-type array-index below bytes by 12
          for output-position of-type array-index from 0 by 16
          do (let* ((raw-in (sse:aref-pi input i))
                    (in (sse:shuffle-pi8 raw-in shuf))
                    (t0 (sse:and-pi     in (sse:set1-pi32 #x0fc0fc00)))
                    (t1 (sse:mulhi-pu16 t0 (sse:set1-pi32 #x04000040)))
                    (t2 (sse:and-pi     in (sse:set1-pi32 #x003f03f0)))
                    (t3 (sse:mullo-pi16 t2 (sse:set1-pi32 #x01000010)))
                    (indices (sse:or-pi t1 t3)))
               (setf (sse:mem-ref-pi output-storage output-position)
                     (encode-lookup indices))))))

(defconstant +lower-bound-lut+
  (sse:setr-pi8
      0    0 #x2b #x30
   #x41 #x50 #x61 #x70
      0    0    0    0
      0    0    0    0))

(defconstant +upper-bound-lut+
  (sse:setr-pi8
      0    0 #x2b #x39
   #x4f #x5a #x6f #x7a
      0    0    0    0
      0    0    0    0))

(defun 8- (a b)
  (ldb (byte 8 0) (- a b)))

(defconstant +decode-shift-lut+
  (sse:setr-pi8
   0 0 (8- #x3e #x2b) (8- #x34 #x30)
   (8- 0 #x41) (8- #x0f #x50) (8- #x1a #x61) (8- #x29 #x70)
   0 0 0 0
   0 0 0 0))

(declaim (inline detect-decode-error decode-lookup decode-pack))
(defun detect-decode-error (mask base-position)
  (loop for n below 16
        when (logtest mask (ash 1 n))
          do (error "invalid character at ~d"
                    (+ n base-position))))

(defun decode-lookup (input base-position)
  (let* ((higher-nibble (sse:and-pi (sse:srli-pi32 input 4)
                                    (sse:set1-pi8 #x0f)))
         (upper-bound (sse:shuffle-pi8 +upper-bound-lut+ higher-nibble))
         (lower-bound (sse:shuffle-pi8 +lower-bound-lut+ higher-nibble))
         (below       (sse:<-pi8 input lower-bound))
         (above       (sse:>-pi8 input upper-bound))
         (eq-2f       (sse:=-pi8 input (sse:set1-pi8 #x2f)))
         (outside     (sse:andnot-pi eq-2f (sse:or-pi above below)))
         (mask        (sse:movemask-pi8 outside)))
    (unless (zerop mask)
      (detect-decode-error mask base-position))
    (let* ((shift  (sse:shuffle-pi8 +decode-shift-lut+ higher-nibble))
           (t0     (sse:add-pi8 input shift)))
      (sse:add-pi8 t0 (sse:and-pi eq-2f (sse:set1-pi8 -3))))))

(defun decode-pack (vals)
  (let ((merge-ab-and-bc (sse:maddubs-pi16 vals (sse:set1-pi32 #x01400140))))
    (sse:madd-pi16 merge-ab-and-bc (sse:set1-pi32 #x00011000))))

(defun %sse2-decode (input output)
  (declare ((simple-array (unsigned-byte 8) 1) output)
           (simple-base-string input))
  (let ((input-storage  (sb-sys:vector-sap input))
        (output-storage (sb-sys:vector-sap output))
        (size (length input))
        (shuf (sse:setr-pi8 2 1 0 6 5 4 10 9 8 14 13 12 #xff #xff #xff #xff)))
    (loop for i of-type array-index below size by 16
          for out of-type array-index from 0 by 12
          do (let* ((in       (sse:mem-ref-pi input-storage i))
                    (vals     (decode-lookup in i))
                    (merged   (decode-pack vals))
                    (shuffled (sse:shuffle-pi8 merged shuf)))
               (setf (sse:mem-ref-pi output-storage out) shuffled)))))
