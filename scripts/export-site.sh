#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
OUT_DIR="${ROOT_DIR}/dist-site"

echo "[kai] Building project (stack build)"
cd "${ROOT_DIR}"
stack build > /dev/null

echo "[kai] Starting server to snapshot HTML..."
stack exec kai-website > /dev/null 2>&1 &
SERVER_PID=$!

# Wait for server
for i in {1..50}; do
  if curl -sSf http://localhost:3000 > /dev/null; then
    break
  fi
  sleep 0.2
done

mkdir -p "${OUT_DIR}"
mkdir -p "${OUT_DIR}/static"

echo "[kai] Fetching index.html"
curl -s http://localhost:3000 > "${OUT_DIR}/index.html"

echo "[kai] Copying static assets"
cp -R "${ROOT_DIR}/website/static/"* "${OUT_DIR}/static/" 2>/dev/null || true

# Rewrite absolute static URLs to relative paths for file:// viewing and Pages
if command -v perl >/dev/null 2>&1; then
  perl -0777 -pe 's~https?://localhost:[0-9]+/static/~static/~g; s~"/static/~"static/~g' -i "${OUT_DIR}/index.html"
else
  # Fallback to sed (may vary on BSD vs GNU); best-effort
  sed 's#http://localhost:3000/static/#static/#g; s#"/static/#"static/#g' "${OUT_DIR}/index.html" > "${OUT_DIR}/index.tmp" && mv "${OUT_DIR}/index.tmp" "${OUT_DIR}/index.html"
fi

echo "[kai] Stopping server (${SERVER_PID})"
kill "${SERVER_PID}" >/dev/null 2>&1 || true

echo "[kai] Static site exported to: ${OUT_DIR}"
echo "      Open ${OUT_DIR}/index.html in your browser."
