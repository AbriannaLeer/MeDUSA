#!/usr/bin/env bash
set -e

echo "============================================"
echo "     MeDUSA Uninstaller (macOS / Linux)     "
echo "============================================"
echo

script_dir="$(cd "$(dirname "$0")" && pwd)"
project_root="$(cd "$script_dir/.." && pwd)"

if ! command -v Rscript >/dev/null 2>&1; then
    echo "ERROR: R is not installed or not in your PATH."
    exit 1
fi

cd "$project_root"
if ! Rscript "$script_dir/uninstall.R" "$@"; then
    echo
    echo "De-installation encountered errors. Please check the messages above."
    exit 1
fi

echo
echo "De-installation successful!"
