#!/bin/zsh

set -e  # エラー時に即停止

# カレントディレクトリの絶対パスを取得
ROOT_DIR=$(realpath .)

# 外部依存ライブラリ用ディレクトリ
THIRD_PARTY_DIR="$ROOT_DIR/third_party"
VTKFortran_DIR="$THIRD_PARTY_DIR/VTKFortran"
INSTALL_PREFIX="$THIRD_PARTY_DIR/.local"

# 必要なツールをインストール
pip install FoBiS.py --root-user-action=ignore

# third_party ディレクトリを作成
mkdir -p "$THIRD_PARTY_DIR"

# VTKFortran を取得（コメントアウト解除で自動取得可能）
cd "$THIRD_PARTY_DIR"
rm -rf "$VTKFortran_DIR"
wget $(curl -s https://api.github.com/repos/szaghi/VTKFortran/releases/latest | grep 'browser_' | cut -d\" -f4 | grep -i tar.gz)
tar xf VTKFortran*.tar.gz
mv VTKFortran* "$VTKFortran_DIR"
rm -f VTKFortran*.tar.gz

cd "$VTKFortran_DIR"

# FoBiS用 config.fobis を ifx 用に修正
sed -i 's/^compiler     = intel$/compiler     = intel_nextgen/' fobis

# === FoBiSビルド ===
python3 /usr/local/bin/FoBiS.py build -mode static-intel

# === インストール用ディレクトリ作成 ===
mkdir -p "$INSTALL_PREFIX/include"
mkdir -p "$INSTALL_PREFIX/lib"

# === .mod と .a をコピー ===
cp -v static/mod/*.mod "$INSTALL_PREFIX/include/"
cp -v static/*.a "$INSTALL_PREFIX/lib/"
