#!/bin/zsh

# カレントディレクトリの絶対パスを取得
ROOT_DIR=$(realpath .)

# 外部依存ライブラリ用ディレクトリ
THIRD_PARTY_DIR="$ROOT_DIR/third_party"
STDLIB_DIR="$THIRD_PARTY_DIR/stdlib"
INSTALL_PREFIX="$THIRD_PARTY_DIR/.local"

# third_party ディレクトリが無ければ作成
if [ ! -d "$THIRD_PARTY_DIR" ]; then
    mkdir -p "$THIRD_PARTY_DIR"
fi
cd "$THIRD_PARTY_DIR"

# 既存のstdlibディレクトリがあれば削除
if [ -d "$STDLIB_DIR" ]; then
    rm -rf "$STDLIB_DIR"
fi

# GitHub から stdlib をクローン
git clone https://github.com/fortran-lang/stdlib
cd "$STDLIB_DIR"

# コンパイラオプション設定
export FFLAGS="-O3 -xCORE-AVX2"

# CMake構成
cmake -B build \
      -G Ninja \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DCMAKE_VERBOSE_MAKEFILE=On \
      -DCMAKE_BUILD_TYPE=Release

# ビルド
cmake --build build

# インストール
cmake --install build --prefix "$INSTALL_PREFIX"
