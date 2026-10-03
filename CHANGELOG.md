# Changelog

## 0.1.2

- Standardized CI and release verification on Ubuntu x86_64 runners.

## 0.1.1

- Reject reserved RFC 1951 dynamic Huffman counts.
- Reject unsupported one-shot and asynchronous flush options instead of ignoring them.

## 0.1.0

- Added pure Common Lisp RFC 1951 DEFLATE, RFC 1950 zlib, and RFC 1952 gzip codecs.
- Added bounded decompression with explicit output-limit conditions.
- Added one-shot and chunk-accumulating stream APIs.
- Added checksum validation, malformed-input checks, external gzip interoperability tests, and Nix-based CI.
