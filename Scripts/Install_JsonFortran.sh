#!/bin/zsh

# カレントディレクトリの絶対パスを取得
ROOT_DIR=$(realpath .)

# 外部依存ライブラリ用ディレクトリ
THIRD_PARTY_DIR="$ROOT_DIR/third_party"
JSONFORTRAN_DIR="$THIRD_PARTY_DIR/json-fortran"
INSTALL_PREFIX="$THIRD_PARTY_DIR/.local"

# third_party ディレクトリが無ければ作成
if [ ! -d "$THIRD_PARTY_DIR" ]; then
    mkdir -p "$THIRD_PARTY_DIR"
fi
cd "$THIRD_PARTY_DIR"

# 既存のjson-fortranディレクトリがあれば削除
if [ -d "$JSONFORTRAN_DIR" ]; then
    rm -rf "$JSONFORTRAN_DIR"
fi

# GitHub から json-fortran をクローン
git clone https://github.com/jacobwilliams/json-fortran.git
cd "$JSONFORTRAN_DIR"

# コンパイラオプション設定
export FFLAGS="-O3 -xCORE-AVX2"

# CMake構成
cmake -B build \
      -G Ninja \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DCMAKE_VERBOSE_MAKEFILE=On \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_Fortran_MODULE_DIRECTORY="$JSONFORTRAN_DIR/build/include"

# ビルド
cmake --build build

# インストール
cmake --install build --prefix "$INSTALL_PREFIX"
