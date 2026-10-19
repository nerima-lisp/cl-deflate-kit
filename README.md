# cl-deflate-kit

Pure Common Lisp implementations of RFC 1951 DEFLATE, RFC 1950 zlib, and RFC
1952 gzip, with RFC 7932 Brotli decompression. The public package is
`deflate-kit`. It has no Lisp dependencies and supports bounded decompression,
checksums, one-shot APIs, and stream APIs.

This README describes the current source tree. The changelog marks changes that
have not yet appeared in a tagged release.

With the system available to ASDF, a raw DEFLATE round trip looks like this:

```lisp
(require :asdf)
(asdf:load-system "cl-deflate-kit")

(let* ((input (make-array 3 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3)))
       (compressed (deflate-kit:deflate input)))
  (assert (equalp input (deflate-kit:inflate compressed))))
```

`deflate` accepts octet vectors, strings, or octet lists and returns a raw
DEFLATE octet vector. Compression levels 1-3 use fixed Huffman blocks; levels
4-9 use LZ77 hash-chain matching and dynamic Huffman blocks for inputs of at
least 32 bytes. Level 0 uses stored blocks. `gzip-compress` and `zlib-compress`
wrap the same raw codec.

Strings map each character directly to its character code as one octet, so
character codes above 255 are rejected.

`inflate` accepts `:max-output-bytes`, `:max-output`, `:truncate-at`, and
`:size-hint`. Its default output limit is 16 MiB. Pass `:unbounded t` to disable
the limit explicitly; `:unbounded` cannot be combined with an output limit.
`:max-output` is a compatibility alias for `:max-output-bytes`; pass only one.
`:truncate-at` stops decoding after that many output bytes and returns `NIL` as
the consumed-input value. `:size-hint` reserves output capacity without
limiting the decoded size. Otherwise, the second value from `inflate` is the
number of input bytes consumed from `:start`.
The limit keywords differ by API:

| API | Limit keywords |
| --- | --- |
| `inflate` | `:max-output-bytes`, `:max-output`, `:unbounded` |
| `make-inflate-stream` | `:max-output-bytes`, `:max-output`, `:unbounded` |
| `gzip-decode` | `:max-output-bytes`, `:max-output`, `:unbounded` |
| `gzip-decode-prefix` | Required `:max-output` |
| `zlib-decode` | `:max-output-bytes`, `:max-output` |
| `brotli-decode` | `:max-output-bytes`, `:max-output`, `:unbounded` |
| `make-brotli-stream` | `:max-output-bytes`, `:max-output`, `:unbounded`, `:max-input-bytes` |

All APIs except `gzip-decode-prefix` default to a 16 MiB output limit. The gzip
limit applies to the aggregate output of all decoded members. `gzip-decode`
also accepts `:allow-trailing-padding`, which defaults to `NIL`.

`brotli-decode` returns decoded octets. For chunked input, use
`make-brotli-stream`, pass chunks to `brotli-stream-push`, and call
`brotli-stream-finish` to decode them. Brotli output and stream input limits
default to 16 MiB. `:max-input-bytes` bounds the compressed bytes retained by a
Brotli stream. `:max-output` aliases `:max-output-bytes`; specify only one.
Pass `:unbounded t` to disable the output limit; it cannot be combined with an
output-limit keyword.

`gzip-decode` returns the concatenated decoded octets from all members as its
primary value and a list of `gzip-member-header` structures as its second
value. The list contains one header per member, in input order.
The structure exposes these accessors:

| Accessor | Value |
| --- | --- |
| `gzip-member-header-flags` | Header flags byte. |
| `gzip-member-header-mtime` | Modification time as an unsigned 32-bit value. |
| `gzip-member-header-xfl` | XFL byte. |
| `gzip-member-header-extra-flags` | Alias for `gzip-member-header-xfl`. |
| `gzip-member-header-os` | Operating system byte. |
| `gzip-member-header-operating-system` | Alias for `gzip-member-header-os`. |
| `gzip-member-header-fextra` | Raw FEXTRA octets. |
| `gzip-member-header-extra` | Alias for `gzip-member-header-fextra`. |
| `gzip-member-header-fname` | Raw FNAME octets. |
| `gzip-member-header-fcomment` | Raw FCOMMENT octets. |
| `gzip-member-header-fhcrc` | Validated 16-bit FHCRC value. |

FEXTRA, FNAME, and FCOMMENT accessors return `NIL` when their field is absent.
They return an empty octet vector when the field is present but empty. Header
FEXTRA contains only the field payload, without its two-byte length. FNAME and
FCOMMENT omit their terminating NUL. Header text fields remain raw bytes and
are not decoded as strings. FHCRC returns `NIL` when the header has no FHCRC.

`gzip-member-header-at` accepts `:start` and `:end` and returns the parsed
header and the absolute start of its DEFLATE payload. `gzip-decode-prefix`
requires `:max-output` and decodes only the first member. It returns the
decoded prefix, its header, and the absolute end position; the position is
`NIL` when decoding reaches the output bound, including when the member's
decoded length exactly equals that bound. The member trailer is validated only
when decoding completes before the bound.

With `:allow-trailing-padding t`, `gzip-decode` accepts a suffix only when at
least one member has decoded and every remaining byte is zero. Padding adds no
member or header. Empty or all-zero input, zero-valued bytes between members,
and nonzero or mixed trailing data still signal `invalid-container-error`.

Exceeding gzip's aggregate 16 MiB output limit signals
`inflate-size-limit-exceeded`, a subtype of `deflate-output-limit`.
Malformed raw DEFLATE payloads signal `inflate-invalid-data`; malformed gzip or
zlib framing signals `invalid-container-error`. Checksum failures signal
`checksum-error`.

`brotli-invalid-data` reports malformed Brotli data and is a subtype of
`deflate-error`. `brotli-size-limit-exceeded` is a subtype of
`deflate-output-limit`; `brotli-input-limit-exceeded` is a subtype of
`deflate-input-limit`. The limit conditions expose `:output-limit` and
`:input-limit` through `inflate-error-reason`, respectively. Their readers are:

| Condition | Additional readers |
| --- | --- |
| `brotli-invalid-data` | `inflate-error-reason`, `inflate-error-detail` |
| `brotli-size-limit-exceeded` | `deflate-output-limit-limit`, `inflate-error-reason`, `inflate-error-observed` |
| `brotli-input-limit-exceeded` | `deflate-input-limit-limit`, `inflate-error-reason`, `inflate-error-observed` |

All three conditions also inherit `deflate-error-message` and
`deflate-error-position`.

The public API covers raw DEFLATE, zlib, gzip, Brotli, CRC-32, Adler-32, and
stream codecs. `deflater-write` returns newly available complete compressed bytes;
`deflater-finish` returns the remaining bytes. `inflate-stream-push` retains
input chunks, and `inflate-stream-finish` decodes them. ZIP and TAR container
APIs remain with callers such as aitools; gzip can therefore be used as the
codec for `tar.gz` without making archive traversal decisions in this library.

Performance measurements used SBCL 2.6.8 on macOS arm64 and 2 MiB
high-entropy and text inputs. The reference was the `aitools` codec at revision
`1b20c3ab`. Calls alternated between this kit and the reference in the same
process. A full GC ran before each timed call. The table reports medians of
seven paired measurements, the kit's paired wins, and the reference-to-kit
speed ratio. Inflate used the same reference-compressed input for both decoders.

| Payload | Operation | Kit median | Reference median | Speed ratio | Kit wins |
| --- | --- | ---: | ---: | ---: | ---: |
| High entropy | DEFLATE | 169.832 ms | 341.083 ms | 2.008x | 7/7 |
| High entropy | INFLATE | 159.701 ms | 177.533 ms | 1.112x | 7/7 |
| Text | DEFLATE | 17.189 ms | 40.095 ms | 2.333x | 7/7 |
| Text | INFLATE | 14.494 ms | 21.755 ms | 1.501x | 7/7 |
| High entropy | CRC-32 | 10.572 ms | 10.600 ms | 1.003x | 4/7 |
| Text | CRC-32 | 10.947 ms | 10.859 ms | 0.992x | 4/7 |

Five A/A pairs per arm measured relative timing differences between consecutive
calls to the same implementation. The values below are median / maximum percent
difference; they record the observed timing variation for this run.

| Payload | Operation | Kit A/A | Reference A/A |
| --- | --- | ---: | ---: |
| High entropy | DEFLATE | 4.37 / 8.50% | 3.17 / 7.82% |
| High entropy | INFLATE | 3.34 / 6.22% | 3.77 / 16.78% |
| High entropy | CRC-32 | 0.95 / 3.76% | 0.11 / 3.68% |
| Text | DEFLATE | 10.19 / 179.11% | 4.01 / 8.19% |
| Text | INFLATE | 4.05 / 11.50% | 8.48 / 41.87% |
| Text | CRC-32 | 6.14 / 16.14% | 1.39 / 13.07% |

CRC-32 timings were close to parity and within the observed A/A spread, so they
do not establish a performance improvement. These measurements describe one
machine and are not performance guarantees.

The benchmark harness is `bench/compare-aitools.lisp`. Its high-entropy input
uses a 32-bit linear congruential generator seeded with `2463534242`; each byte
is the high byte of the updated state. Its text input repeats the ASCII text in
`%text-octets`. The harness checks both codecs' round trips and CRC results
before timing, runs seven alternating pairs and five A/A pairs per arm, and
compiles the `aitools` source files into the system temporary directory.
Inflate uses the same `aitools`-compressed bytes for both decoders.

Set `AITOOOLS_ROOT` to an `aitools` checkout at revision `1b20c3ab`, then run
this command from the repository root:

```sh
CL_SOURCE_REGISTRY="$PWD//:/" nix develop -c sbcl --script bench/compare-aitools.lisp
```

The release workflow runs on Ubuntu 24.04. It verifies that the tag matches the
ASDF system version, then runs these checks before creating the GitHub Release:

```sh
nix develop -c bash ci/lint.sh
nix develop -c sbcl --script ci/coverage.lisp
nix flake check
```
