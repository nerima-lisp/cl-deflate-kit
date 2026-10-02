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

`inflate`, `gzip-decompress`, and `zlib-decompress` accept `:max-output-bytes`.
The container APIs also accept the compatibility spelling `:max-output`.
The default decompression limit is 16 MiB. Exceeding it signals
`inflate-size-limit-exceeded`, a subtype of `deflate-output-limit`.
Malformed streams signal `inflate-invalid-data`; checksum failures signal
`checksum-error`.

Performance was measured on macOS arm64 with SBCL 2.6.0 using one-shot level 6
APIs and deterministic 1 MiB inputs. A repeated-byte input compressed to 1,064
bytes (0.1015%) at 23.60 MiB/s and inflated at 84.60 MiB/s. The deterministic
pseudo-random input compressed to 1,049,370 bytes (100.0763%) at 2.79 MiB/s
and inflated at 1.47 MiB/s. These are baseline measurements, not guarantees.
