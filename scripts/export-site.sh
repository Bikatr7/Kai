#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
OUT_DIR="${KAI_SITE_EXPORT_OUT_DIR:-${ROOT_DIR}/dist-site}"
STACK_BIN="${KAI_SITE_EXPORT_STACK_BIN:-stack}"
CURL_BIN="${KAI_SITE_EXPORT_CURL_BIN:-curl}"
SERVER_PID=""
SERVER_PORT=""
EXPORT_TOKEN="kai-export-$$-${RANDOM}-${RANDOM}"
WORK_DIR="$(mktemp -d "${TMPDIR:-/tmp}/kai-site-export.XXXXXX")"
SERVER_LOG="${WORK_DIR}/server.log"
HEADERS_FILE="${WORK_DIR}/headers"
HTML_FILE="${WORK_DIR}/index.html"
STAGED_SITE="${WORK_DIR}/site"

stop_server() {
  if [[ -z "${SERVER_PID}" ]]; then
    return
  fi

  if kill -0 "${SERVER_PID}" >/dev/null 2>&1; then
    kill "${SERVER_PID}" >/dev/null 2>&1 || true
    for ((i = 0; i < 20; i++)); do
      if ! kill -0 "${SERVER_PID}" >/dev/null 2>&1; then
        break
      fi
      sleep 0.05
    done
    if kill -0 "${SERVER_PID}" >/dev/null 2>&1; then
      kill -KILL "${SERVER_PID}" >/dev/null 2>&1 || true
    fi
  fi

  wait "${SERVER_PID}" >/dev/null 2>&1 || true
  SERVER_PID=""
}

cleanup() {
  status=$?
  trap - EXIT
  stop_server
  rm -rf "${WORK_DIR}"
  exit "${status}"
}

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

valid_port() {
  [[ "$1" =~ ^[0-9]{1,5}$ ]] && ((10#$1 >= 1 && 10#$1 <= 65535))
}

start_server() {
  SERVER_PORT="$1"
  export PORT="${SERVER_PORT}"
  export KAI_SITE_EXPORT_TOKEN="${EXPORT_TOKEN}"
  : > "${SERVER_LOG}"
  "${STACK_BIN}" exec kai-website > "${SERVER_LOG}" 2>&1 &
  SERVER_PID=$!
}

response_belongs_to_server() {
  tr -d '\r' < "${HEADERS_FILE}" | grep -Fqx "X-Kai-Export-Token: ${EXPORT_TOKEN}" &&
    grep -Fq '<meta name="kai-site" content="kai-language">' "${HTML_FILE}"
}

fetch_owned_site() {
  : > "${HEADERS_FILE}"
  : > "${HTML_FILE}"

  for ((i = 0; i < 50; i++)); do
    if ! kill -0 "${SERVER_PID}" >/dev/null 2>&1; then
      return 1
    fi

    if "${CURL_BIN}" -sS --fail --connect-timeout 1 --max-time 2 \
      -D "${HEADERS_FILE}" -o "${HTML_FILE}" \
      "http://127.0.0.1:${SERVER_PORT}/" 2>/dev/null; then
      if ! kill -0 "${SERVER_PID}" >/dev/null 2>&1; then
        return 1
      fi
      if response_belongs_to_server; then
        return 0
      fi
      return 2
    fi

    sleep 0.2
  done

  return 1
}

publish_site() {
  backup_dir="${WORK_DIR}/previous-site"
  if [[ -e "${OUT_DIR}" ]]; then
    mv "${OUT_DIR}" "${backup_dir}"
  fi

  if mv "${STAGED_SITE}" "${OUT_DIR}"; then
    return 0
  fi

  if [[ -e "${backup_dir}" ]]; then
    mv "${backup_dir}" "${OUT_DIR}"
  fi
  return 1
}

echo "[kai] Building project (stack build)"
cd "${ROOT_DIR}"
"${STACK_BIN}" build > /dev/null

configured_port="${KAI_SITE_EXPORT_PORT:-}"
if [[ -n "${configured_port}" ]] && ! valid_port "${configured_port}"; then
  echo "[kai] KAI_SITE_EXPORT_PORT must be an integer between 1 and 65535" >&2
  exit 1
fi

port_seed=$((49152 + ((RANDOM + $$) % 16384)))
attempts=20
if [[ -n "${configured_port}" ]]; then
  attempts=1
fi

echo "[kai] Starting server to snapshot HTML..."
site_ready=false
last_failure="server did not become ready"
for ((attempt = 0; attempt < attempts; attempt++)); do
  if [[ -n "${configured_port}" ]]; then
    candidate_port=$((10#${configured_port}))
  else
    candidate_port=$((49152 + ((port_seed - 49152 + attempt) % 16384)))
  fi

  start_server "${candidate_port}"
  if fetch_owned_site; then
    site_ready=true
    break
  else
    fetch_status=$?
    if [[ "${fetch_status}" -eq 2 ]]; then
      last_failure="port ${candidate_port} returned a response not owned by this exporter"
    else
      last_failure="server failed to start on port ${candidate_port}"
    fi
  fi
  stop_server
done

if [[ "${site_ready}" != true ]]; then
  echo "[kai] Unable to export site: ${last_failure}" >&2
  if [[ -s "${SERVER_LOG}" ]]; then
    tail -n 20 "${SERVER_LOG}" >&2
  fi
  exit 1
fi

mkdir -p "${STAGED_SITE}/static"
mv "${HTML_FILE}" "${STAGED_SITE}/index.html"

echo "[kai] Copying static assets"
cp -R "${ROOT_DIR}/website/static/." "${STAGED_SITE}/static/"

# Rewrite absolute static URLs to relative paths for file:// viewing and Pages
if command -v perl >/dev/null 2>&1; then
  perl -0777 -pe 's~https?://(?:localhost|127\.0\.0\.1):[0-9]+/static/~static/~g; s~"/static/~"static/~g' \
    -i "${STAGED_SITE}/index.html"
else
  sed -e "s#http://localhost:${SERVER_PORT}/static/#static/#g" \
    -e "s#http://127.0.0.1:${SERVER_PORT}/static/#static/#g" \
    -e 's#"/static/#"static/#g' \
    "${STAGED_SITE}/index.html" > "${STAGED_SITE}/index.tmp"
  mv "${STAGED_SITE}/index.tmp" "${STAGED_SITE}/index.html"
fi

if ! grep -Fq '<meta name="kai-site" content="kai-language">' "${STAGED_SITE}/index.html"; then
  echo "[kai] Refusing to publish an unrecognized site response" >&2
  exit 1
fi

stop_server
publish_site

echo "[kai] Static site exported to: ${OUT_DIR}"
echo "      Open ${OUT_DIR}/index.html in your browser."
