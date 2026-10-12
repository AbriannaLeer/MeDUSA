set -e

## Set default to current working directory
TARGET="${1:-.}"

Rscript -e "
    if (!requireNamespace('styler', quietly = TRUE)) {
        install.packages('styler', quiet = TRUE)
    }
"

if [ -f "$TARGET" ]; then
    Rscript -e "styler::style_file('$TARGET')"

elif [ -d "$TARGET" ]; then
    Rscript -e "styler::style_dir(path = '$TARGET', recursive = TRUE, filetype='R')"
else
    echo "ERROR: '$TARGET' is not a valid file or directory"
    exit 1
fi