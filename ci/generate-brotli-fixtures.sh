#!/usr/bin/env bash
set -euo pipefail

script_dir=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
fixture_dir="$script_dir/../t/fixtures/brotli"
mkdir -p "$fixture_dir"

if command -v brotli >/dev/null 2>&1; then
  brotli_cmd=(brotli)
elif command -v nix >/dev/null 2>&1; then
  brotli_cmd=(nix run nixpkgs#brotli --)
else
  printf 'brotli or nix is required\n' >&2
  exit 1
fi

: > "$fixture_dir/empty"
cat > "$fixture_dir/simple.txt" <<'EOF'
Brotli decoding returns the original bytes.
EOF
cat > "$fixture_dir/dictionary.txt" <<'EOF'
The quick brown fox jumps over the lazy dog.
The quick brown fox jumps over the lazy dog.
The answer to the question is a simple example of text compression.
The answer to the question is a simple example of text compression.
Read the book, follow the link, and write a note about the page.
Read the book, follow the link, and write a note about the page.
EOF
awk 'BEGIN {
  for (i = 0; i < 16384; i++) {
    printf "%08d:%02d:%06d:%08d:%04d:%030d\n", i, i % 31, i * 17 % 1000000, i * 7919 % 100000000, i % 9999, i * i * 97
  }
}' > "$fixture_dir/large.txt"

for input in empty simple.txt dictionary.txt large.txt; do
  output="$fixture_dir/$input.br"
  "${brotli_cmd[@]}" --quality=11 --no-copy-stat --force --output="$output" "$fixture_dir/$input"
  "${brotli_cmd[@]}" --test "$output"
done
