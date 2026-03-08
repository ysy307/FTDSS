#!/bin/zsh

set -e

ROOT_DIR=$(realpath .)
THIRD_PARTY_DIR="${ROOT_DIR}/third_party"
INSTALL_PREFIX="${THIRD_PARTY_DIR}/.local"

# Add user local bin to PATH
export PATH="$HOME/.local/bin:$PATH"
pip install FoBiS.py --root-user-action=ignore

mkdir -p "${THIRD_PARTY_DIR}"
cd "${THIRD_PARTY_DIR}"

# Remove existing directory
rm -rf VTKFortran

export FC=ifx
export LC_ALL=C

# Run download and build using install.sh
bash "${ROOT_DIR}/scripts/install.sh" --repo szaghi/VTKFortran --download git --build cmake

cd VTKFortran
ctest --test-dir build
cmake --install build --prefix "${INSTALL_PREFIX}"

# Copy all generated module files
mkdir -p "${INSTALL_PREFIX}/include/VTKFortran"
find build -name "*.mod" -exec cp {} "${INSTALL_PREFIX}/include/VTKFortran/" \;

# Merge all third-party libraries into a single libvtkfortran.a
cd "${INSTALL_PREFIX}/lib"
find "${THIRD_PARTY_DIR}/VTKFortran/build" -name "*.a" -exec cp {} . \;

# Create an MRI script for ar to merge static libraries safely
echo "create libvtkfortran_merged.a" > merge.mri
for lib in *.a; do
    if [ -f "$lib" ]; then
        echo "addlib $lib" >> merge.mri
    fi
done
echo "save" >> merge.mri
echo "end" >> merge.mri

ar -M < merge.mri
mv libvtkfortran_merged.a libvtkfortran.a
rm merge.mri