#!/bin/zsh
set -e
set -o pipefail

tree -a -h -s --dirsfirst src >| log/tree.txt