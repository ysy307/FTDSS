# Ubuntu 22.04をベースに使用
FROM ubuntu:22.04

# --- 環境変数とビルド引数 ---
# 日本語環境とタイムゾーンを設定
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8 \
    TZ=JST-9

# デフォルトシェルをZshに設定
ENV SHELL=/usr/bin/zsh

# ビルド引数: ユーザー名やUID/GIDを指定可能にする
ARG USERNAME=devuser
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# --- 1. 基本パッケージとリポジトリキーのインストール ---
# このレイヤーは変更頻度が低いため、最初に実行してキャッシュさせる
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
    wget gnupg lsb-release curl build-essential ca-certificates sudo vim git \
    gfortran python3 python3-pip python3-dev zsh sed software-properties-common \
    ninja-build graphviz linux-tools-common linux-tools-generic \
    libx11-dev libxext-dev libxrender-dev libxt-dev \
    adwaita-icon-theme-full gnuplot valgrind kcachegrind && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# --- 2. 外部リポジトリの追加 ---
# Intel, NVIDIA, Kitwareのリポジトリキーと設定を追加する
# この処理も変更頻度が低いため、早めに実行する
RUN wget -qO- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor > /usr/share/keyrings/oneapi-archive-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" > /etc/apt/sources.list.d/oneAPI.list && \
    curl https://developer.download.nvidia.com/hpc-sdk/ubuntu/DEB-GPG-KEY-NVIDIA-HPC-SDK | gpg --dearmor > /usr/share/keyrings/nvidia-hpcsdk-archive-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/nvidia-hpcsdk-archive-keyring.gpg] https://developer.download.nvidia.com/hpc-sdk/ubuntu/amd64 /" > /etc/apt/sources.list.d/nvhpc.list && \
    wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor > /usr/share/keyrings/kitware-archive-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ $(lsb_release -cs) main" > /etc/apt/sources.list.d/kitware.list

# --- 3. コンパイラとCMakeのインストール ---
# 外部リポジトリから主要なツールをインストールする
# パッケージのバージョンが変わらない限り、このレイヤーはキャッシュされる
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
    intel-oneapi-compiler-fortran \
    intel-oneapi-hpc-toolkit \
    nvhpc-25-3 \
    cmake && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# --- 4. システムワイドな環境変数の設定 ---
# 全ユーザーに適用される環境変数を設定
RUN echo 'export NVARCH=$(uname -s)_$(uname -m)' > /etc/profile.d/nvhpc.sh && \
    echo 'export NVCOMPILERS=/opt/nvidia/hpc_sdk' >> /etc/profile.d/nvhpc.sh && \
    echo 'export PATH=$NVCOMPILERS/$NVARCH/25.3/compilers/bin:$PATH' >> /etc/profile.d/nvhpc.sh && \
    echo 'export MANPATH=$MANPATH:$NVCOMPILERS/$NVARCH/25.3/compilers/man' >> /etc/profile.d/nvhpc.sh && \
    echo 'export PATH=$NVCOMPILERS/$NVARCH/25.3/comm_libs/mpi/bin:$PATH' >> /etc/profile.d/nvhpc.sh && \
    echo 'export MANPATH=$MANPATH:$NVCOMPILERS/$NVARCH/25.3/comm_libs/mpi/man' >> /etc/profile.d/nvhpc.sh && \
    # oneAPIの環境変数は .zprofile で読み込む
    echo '[ -f /opt/intel/oneapi/setvars.sh ] && source /opt/intel/oneapi/setvars.sh' > /etc/profile.d/oneapi.sh

# --- 5. Pythonツールのインストール ---
# pipでのインストールは、コンパイラ等より変更頻度が高いため、後のレイヤーに配置
RUN pip3 install --upgrade pip && \
    pip3 install --force-reinstall git+https://github.com/fortran-lang/fortls && \
    pip3 install --upgrade fprettify fypp ford

# --- 6. 一般ユーザーの作成と設定 ---
# rootではなく、一般ユーザーで作業を行う
RUN groupadd --gid $USER_GID $USERNAME && \
    useradd --uid $USER_UID --gid $USER_GID -m -s $SHELL $USERNAME && \
    echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME && \
    chmod 0440 /etc/sudoers.d/$USERNAME

# ユーザーを切り替える
USER $USERNAME
WORKDIR /home/$USERNAME

# --- 7. ZshとPreztoの設定 ---
# USERを切り替えた後、ホームディレクトリに対して設定を行う
RUN git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto" && \
    git clone --recurse https://github.com/belak/prezto-contrib "${ZDOTDIR:-$HOME}/.zprezto/contrib"

# Preztoのセットアップスクリプトを Zsh で実行する
RUN zsh -c 'setopt EXTENDED_GLOB && \
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do \
    ln -sf "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"; \
    done'

# .zpreztorc の設定を sed で変更
RUN sed -i "/'prompt'/c \\\ 'contrib-prompt' \\\\\n  'prompt'" .zpreztorc && \
    sed -i "s/theme 'sorin'/theme 'bart'/g" .zpreztorc

# .zshrc と .zprofile の設定
RUN echo 'export PS1="%~ %# "' >> .zshrc && \
    echo "export FC=ifx" >> .zshrc && \
    echo "export F90=ifx" >> .zshrc && \
    echo "export F77=ifx" >> .zshrc && \
    echo "export CC=icx" >> .zshrc && \
    echo "export CXX=icpx" >> .zshrc && \
    echo "export MPIFC=mpiifx" >> .zshrc && \
    echo "export MPIF90=mpiifx" >> .zshrc && \
    echo "export MPIF77=mpiifx" >> .zshrc && \
    echo "export MPICC=mpiicc" >> .zshrc && \
    echo "export MPICXX=mpiicpc" >> .zshrc && \
    echo "ulimit -s unlimited" >> .zshrc && \
    echo '[ -f /etc/profile.d/oneapi.sh ] && source /etc/profile.d/oneapi.sh' >> .zprofile && \
    echo '[ -f /etc/profile.d/nvhpc.sh ] && source /etc/profile.d/nvhpc.sh' >> .zprofile

# --- コンテナ起動時のデフォルトコマンド ---
CMD ["/usr/bin/zsh"]
