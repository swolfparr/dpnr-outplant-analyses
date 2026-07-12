#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
QUARTO="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto"
INDEX_HTML="${PROJECT_ROOT}/docs/index.html"

if [[ ! -x "${QUARTO}" ]]; then
  echo "Could not find RStudio's bundled Quarto at:"
  echo "${QUARTO}"
  exit 1
fi

cd "${PROJECT_ROOT}"
echo "Rendering project: ${PROJECT_ROOT}"
"${QUARTO}" render

if [[ ! -f "${INDEX_HTML}" ]]; then
  echo "Render finished, but docs/index.html was not found."
  exit 1
fi

if ! grep -q "site-hero" "${INDEX_HTML}"; then
  echo "docs/index.html rendered, but the custom homepage was not found."
  exit 1
fi

echo
echo "Rendered the pretty homepage successfully."
echo "Open this file to preview it locally:"
echo "${INDEX_HTML}"
echo
echo "To update the public GitHub Pages site, commit and push the changed files."
