#!/usr/bin/env bash
# CI script to check if code blocks in config.org are properly formatted

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Run the formatting check using normal init.el loading
echo "Checking formatting..."
emacs --batch \
      --eval "(setq user-emacs-directory \"$SCRIPT_DIR/\")" \
      --load "$SCRIPT_DIR/init.el" \
      --load "$SCRIPT_DIR/check-formatting.el"
