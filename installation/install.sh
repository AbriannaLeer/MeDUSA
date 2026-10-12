#!/usr/bin/env bash
set -e

echo "============================================"
echo "       MeDUSA Installer (macOS / Linux)     "
echo "============================================"
echo

script_dir="$(cd "$(dirname "$0")" && pwd)"

# Check for Rscript
if ! command -v Rscript &>/dev/null; then
    echo "ERROR: R is not installed or not in your PATH."
    echo
    echo "Install R from: https://cran.r-project.org/"
    echo "  macOS:   brew install r       (or download from CRAN)"
    echo "  Ubuntu:  sudo apt install r-base"
    echo "  Fedora:  sudo dnf install R"
    exit 1
fi

echo "R found. Starting installation..."
echo

# Move to parent folder (where MeDUSA resides)
cd "$script_dir/.."

# Run install.R from the original script folder
if ! Rscript "$script_dir/install.R" "$@"; then
    echo
    echo "Installation encountered errors. Please check the messages above."
    exit 1
fi

echo
echo "Installation successful!"
