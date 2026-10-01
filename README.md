# cl-deflate-kit

Pure Common Lisp RFC 1951 DEFLATE, RFC 1950 zlib, and RFC 1952 gzip codecs.
The public package is `deflate-kit`. It has no Lisp dependencies and supports
bounded decompression, checksums, one-shot APIs, and chunk-accumulating stream
objects.

`deflate` accepts octet vectors, strings, or octet lists and returns a raw
DEFLATE octet vector. Compression levels 1-3 use fixed Huffman blocks; levels
4-9 use LZ77 hash-chain matching and dynamic Huffman blocks for inputs of at
least 32 bytes. Level 0 uses stored blocks. `gzip-compress` and `zlib-compress`
wrap the same raw codec.

`inflate`, `gzip-decompress`, and `zlib-decompress` accept
`:max-output-bytes` (also available as `:max-output` on the container APIs).
The default decompression limit is 16 MiB. Exceeding it signals
`inflate-size-limit-exceeded`, a subtype of `deflate-output-limit`.
Malformed streams signal `inflate-invalid-data`; checksum failures signal
`checksum-error`.

Performance was measured on macOS arm64 with SBCL 2.6.0 using a 1 MiB
deterministic pseudo-random octet vector and one-shot level 6 APIs. The
The observed throughput was 0.44 MiB/s for deflate and 7.57 MiB/s for inflate;
these are baseline measurements, not guarantees.
