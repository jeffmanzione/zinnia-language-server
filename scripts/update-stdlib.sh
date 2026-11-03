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

curl -sSfL "https://github.com/jeffmanzione/zinnia/archive/$sha.tar.gz" |
  tar -xz -C "$tmp" --wildcards '*/zinnia/lib/*.zn'

rm -f "$dest"/*.zn
cp "$tmp"/*/zinnia/lib/*.zn "$dest/"
echo "$sha" > "$dest/ZINNIA_COMMIT"
echo "Updated $dest to zinnia $sha."
