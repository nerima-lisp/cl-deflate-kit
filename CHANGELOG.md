# Changelog

## 0.3.0

- Improve raw DEFLATE and INFLATE throughput against the `aitools` codec at
  revision `1b20c3ab` on 2 MiB high-entropy and text inputs.
- Add explicit `:unbounded t` mode to raw INFLATE, streaming INFLATE, and gzip decode while retaining the 16 MiB default limit.
- Add gzip header parsing at an input offset and bounded first-member prefix decoding.
- Add malformed-input, output-limit, concatenated-member, and property-based round-trip tests.

## 0.2.0

- Expose parsed gzip member headers as the second value from `gzip-decode`.
- Add opt-in support for trailing padding made only of zero-valued bytes after
  one or more gzip members.

## 0.1.2

- Standardized CI and release verification on Ubuntu x86_64 runners.

## 0.1.1

- Reject reserved RFC 1951 dynamic Huffman counts.
- Reject `:raw nil` and `:sync-flush t` in one-shot `deflate`; `deflater-flush`
  supports only the default `:sync t`.

## 0.1.0

- Added pure Common Lisp RFC 1951 DEFLATE, RFC 1950 zlib, and RFC 1952 gzip codecs.
- Added bounded decompression with explicit output-limit conditions.
- Added one-shot and chunk-accumulating stream APIs.
- Added checksum validation, malformed-input checks, external gzip interoperability tests, and Nix-based CI.
