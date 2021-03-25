# qbase64

qbase64 provides a fast and flexible base64 encoder and decoder for
Common Lisp. It provides three interfaces for both encoding and
decoding:

* `ENCODE-BYTES` and `DECODE-STRING` are the easiest to use. They
  allow one to encode a byte vector and decode a base64 string in one
  go.

* `ENCODE-STREAM` and `DECODE-STREAM` are gray stream classes that
  allow one to write and read bytes from underlying character streams.

* `ENCODER` and `DECODER` provide the low level interface. The other
  interfaces are built on top of these.

## Table of Contents

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Why qbase64?](#why-qbase64)
- [Installation](#installation)
- [Usage](#usage)
  - [Encoding](#encoding)
  - [Decoding](#decoding)
  - [Advanced](#advanced)
- [Limits and Assertions](#limits-and-assertions)
- [Performance](#performance)
- [Additional Features](#additional-features)
  - [Encoding Schemes](#encoding-schemes)
  - [Linebreaks](#linebreaks)
- [API Reference](#api-reference)
- [Reporting Bugs](#reporting-bugs)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Why qbase64?

Given that a couple of Lisp libraries already exist for working with
base64 - [cl-base64][] and [s-base64][] - why might you want to pick
qbase64? There are two reasons:

1. Stream-based APIs - neither of the alternatives provide a (gray)
   stream implementation where you can write bytes to a binary stream
   that is automatically encoded to an underlying character stream and
   vice-versa.

2. Performance - qbase64 was written with the objective of being fast
   while keeping memory consumption independent of the input size. See
   the [Performance](#performance) section for benchmarks and other
   details.

[cl-base64]: http://www.cliki.net/cl-base64
[s-base64]: https://github.com/svenvc/s-base64

## Installation

Install using quicklisp:

```lisp
(ql:quickload :qbase64)
```

## Usage

### Encoding

The examples below use `ENCODE-BYTES` and `ENCODE-STREAM`.

```lisp
;;; ENCODE-BYTES
(qbase64:encode-bytes #(1 2 3 4 5 6 7 8))
=> "AQIDBAUGBwg="

;;; ENCODE-STREAM
(with-output-to-string (s)
  (with-open-stream (out (make-instance 'qbase64:encode-stream
                                        :underlying-stream s))
    (write-sequence #(1 2 3 4) out)
    (write-sequence #(5 6 7 8) out)))
=> "AQIDBAUGBwg="
```

### Decoding

The examples below use `DECODE-STRING` and `DECODE-STREAM`.

```lisp
;;; DECODE-STRING
(qbase64:decode-string "AQIDBAUGBwg=")
=> #(1 2 3 4 5 6 7 8)

;;; DECODE-STREAM
(with-input-from-string (s "AQIDBAUGBwg=")
  (with-open-stream (in (make-instance 'qbase64:decode-stream
                                       :underlying-stream s))
    (let ((bytes (make-array 4)))
      (loop
         for position = (read-sequence bytes in)
         do (print (subseq bytes 0 position))
         while (= position (length bytes))))))
; #(1 2 3 4) 
; #(5 6 7 8) 
; #() 
```

### Advanced

Normally you wouldn't need to use `ENCODER` and `DECODER` directly,
but if you do (say you want more control over memory management), you
can refer to the examples below.

In these examples, fixed length sequences are used for both input and
output, and any input buffered by the encoder/decoder is first cleared
before further input is fed to it. This allows very tight control over
how much memory gets used.

Refer to the doc strings for `ENCODER`, `ENCODE`, `DECODER` and
`DECODE` for more details.

Note that running the following examples requires
[FLEXI-STREAMS](http://weitz.de/flexi-streams/).

```lisp
;;; ENCODER
(flexi-streams:with-input-from-sequence (in #(1 2 3 4 5 6 7 8))
  (let* ((encoder (qbase64:make-encoder))
         (bytes (make-array 4))
         (string (make-string 5))
         (read-bytes t)
         (buffered nil)
         (eof nil))
    (loop
       while (or buffered (not eof))
       for end1 = (when read-bytes (read-sequence bytes in))
       if (and read-bytes (< end1 (length bytes))) do (setf eof t)
       do
         (multiple-value-bind (end2 pending)
             (if read-bytes
                 (qbase64:encode encoder bytes string :end1 end1 :finish eof)
                 (qbase64:encode encoder #() string :finish eof))
           (write-string string nil :end end2)
           (setf buffered pending
                 read-bytes (or (not pending) (zerop end2)))))))
; AQIDBAUGBwg=

;;; DECODER
(with-input-from-string (in "AQIDBAUGBwg=")
  (let* ((decoder (qbase64:make-decoder))
         (string (make-string 4))
         (bytes (make-array 5))
         (read-string t)
         (buffered nil)
         (eof nil))
    (loop
       while (or buffered (not eof))
       for end1 = (when read-string (read-sequence string in))
       if (and read-string (< end1 (length string))) do (setf eof t)
       do
         (multiple-value-bind (end2 pending)
             (if read-string
                 (qbase64:decode decoder string bytes :end1 end1)
                 (qbase64:decode decoder "" bytes))
           (print (subseq bytes 0 end2))
           (setf buffered pending
                 read-string (or (not pending) (zerop end2)))))))
; #(1 2 3)
; #(4 5 6)
; #(7 8)
; #()
```

## Limits and Assertions

The library relies on `(UNSIGNED-BYTE 8)` and fixnum arithmetic to
achieve good performance. Consequently,

* When providing bytes for encoding, ensure that each byte is of type
  `(UNSIGNED-BYTE 8)`. Although the `ARRAY-ELEMENT-TYPE` of the byte
  array can be `T`, the elements themselves must conform to this
  restriction.

* Max length of the byte vector that is used as encoding input or
  decoding output should never exceed `+MAX-BYTES-LENGTH+`.

* Max length of the string that is used as encoding output or decoding
  input should never exceed `+MAX-STRING-LENGTH+`.

* When using `:use-qbase64-sse2`, the decoder will not accept whitespace.

## Performance

A short test program can be used to test the throughput of qbase64.

When SSE2 is enabled, the encoder and decoder approach one cycle per byte:

```lisp
CL-USER> (defvar *input* (make-array 1048576 :element-type '(unsigned-byte 8)))
CL-USER> (loop for n below 1048576
               do (setf (aref *input* n) (random 256)))
CL-USER> (let ((str (make-string (ceiling (length *input*) 3/4) :element-type 'base-char)))
           (the-cost-of-nothing:bench (qbase64:encode (qbase64:make-encoder) *input* str)))
329.23 microseconds
CL-USER> (/ 1048576 329.23e-6)
3.1849344e9 ; bytes per second
```

With SSE2 disabled, the same test reports a runtime of 1.80 milliseconds, or
a throughput of 582 megabytes per second.

Encoding and decoding should be very fast under these conditions:

* The byte vector is a `SIMPLE-ARRAY` of element type `(UNSIGNED-BYTE 8)`.

* The string is a `SIMPLE-STRING`.
  
Encoding and decoding should be very very fast under further conditions:

* The string is a `SIMPLE-BASE-STRING` (so that we can pick up 16 characters
  in one load operation).
  
* `:start2` is zero, and `:end2` is the end of the output buffer. (Though
  it wouldn't be hard to lift these limits.)
  
* The scheme is `:original` (unless you want to re-derive the encoder and
  decoder stuff for the URI digit set).

That said, these are just the optimal conditions. You can safely use
any `STRING` or `VECTOR` with qbase64 if needed.

## Additional Features

### Encoding Schemes

Two base64 encoding schemes are supported: original (the default) and
URI.

URI encoding scheme is useful when base64 strings are used as GET or
POST values in an HTTP request.

The scheme can be set by using the `:SCHEME` keyword.

```lisp
(qbase64:encode-bytes #(251 252 253 254 255) :scheme :original)
=> "+/z9/v8="

(qbase64:encode-bytes #(251 252 253 254 255) :scheme :uri)
=> "-_z9_v8="
```

### Linebreaks

The encoded base64 stream can broken into multiple lines using the
`:LINEBREAK` keyword. By default it is 0, which means that no
newlines are output. Setting it to a positive integer indicates the
column number at which lines should be broken.

```lisp
(princ (qbase64:encode-bytes #(1 2 3 4 5 6 7 8) :linebreak 4))
; AQID
; BAUG
; Bwg=
```

During decoding, all whitespace (including newlines) is ignored.

## API Reference

At the moment, API reference is available in the form of doc
strings for all the exported symbols.

## Reporting Bugs

To report a bug in the library, create a [Github
issue](https://github.com/chaitanyagupta/qbase64/issues).
