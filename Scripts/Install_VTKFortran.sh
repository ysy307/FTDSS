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

# --- VTKFortranの取得と展開 ---
echo "Fetching and extracting VTKFortran..."
rm -rf "$VTKFortran_DIR"
mkdir -p "$VTKFortran_DIR"

wget -qO- $(curl -s https://api.github.com/repos/szaghi/VTKFortran/releases/latest | grep 'browser_' | cut -d\" -f4 | grep -i tar.gz) | tar xz -C "$VTKFortran_DIR" --strip-components=1

# VTKFortran ディレクトリに移動
cd "$VTKFortran_DIR"
echo "Successfully changed directory to $(pwd)"

# --- FoBiS用設定ファイルのパスを修正 ---
CONFIG_FILE="fobos"
if [ -f "$CONFIG_FILE" ]; then
    echo "Modifying $CONFIG_FILE for intel_nextgen compiler..."
    
# FoBiS用 config.fobis を ifx 用に修正
sed -i 's/^compiler     = intel$/compiler     = intel_nextgen/' "$CONFIG_FILE"
else
    echo "Error: $CONFIG_FILE not found!"
    exit 1
fi

# === FoBiSビルド ===
echo "Building VTKFortran with FoBiS..."
python3 /usr/local/bin/FoBiS.py build -mode static-intel

# === .mod と .a をコピー ===
echo "Installing modules and library..."

# 必要なインストールディレクトリを作成
mkdir -p "$INSTALL_PREFIX/lib"

# 既存のVTKFortranモジュールディレクトリをクリーンアップし、再作成
echo "Preparing VTKFortran include directory..."
rm -rf "$INSTALL_PREFIX/include/VTKFortran"
mkdir -p "$INSTALL_PREFIX/include/VTKFortran"

# .modファイルをVTKFortranサブディレクトリにコピー
echo "Copying module files..."
cp -v static/mod/*.mod "$INSTALL_PREFIX/include/VTKFortran/"

# .aファイルをlibディレクトリにコピー
echo "Copying library file..."
cp -v static/*.a "$INSTALL_PREFIX/lib/"

echo "Installation complete."

