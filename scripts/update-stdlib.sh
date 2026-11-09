#!/bin/bash
# Updates the bundled copy of the Zinnia standard library in
# packages/server/stdlib from a commit, branch or tag of jeffmanzione/zinnia.
#
# Usage: scripts/update-stdlib.sh [ref]   (default: master)
set -euo pipefail

ref="${1:-master}"
root="$(cd "$(dirname "$0")/.." && pwd)"
dest="$root/packages/server/stdlib"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

sha="$(git ls-remote https://github.com/jeffmanzione/zinnia.git "$ref" | cut -f1 | head -n1)"
if [[ -z "$sha" ]]; then
  sha="$ref"  # Assume ref is already a commit.
fi

curl -sSfL -o "$tmp/zinnia.tar.gz" \
  "https://github.com/jeffmanzione/zinnia/archive/$sha.tar.gz"
# The library is in zinnia/stdlib, or zinnia/lib in older commits.
dir=stdlib
# (grep reads all of tar's output: with -q, tar's SIGPIPE would fail the pipe.)
if ! tar -tzf "$tmp/zinnia.tar.gz" | grep '/zinnia/stdlib/.*\.zn$' >/dev/null; then
  dir=lib
fi
tar -xzf "$tmp/zinnia.tar.gz" -C "$tmp" --wildcards "*/zinnia/$dir/*.zn"

rm -f "$dest"/*.zn
cp "$tmp"/*/zinnia/$dir/*.zn "$dest/"
echo "$sha" > "$dest/ZINNIA_COMMIT"
echo "Updated $dest to zinnia $sha."
