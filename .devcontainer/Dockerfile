# Ubuntu 22.04をベースに使用
FROM ubuntu:22.04

# --- 環境変数とビルド引数 ---
ENV LANG=ja_JP.UTF-8 \
    LANGUAGE=ja_JP:ja \
    LC_ALL=ja_JP.UTF-8 \
    TZ=JST-9

ENV SHELL=/usr/bin/zsh

ARG USERNAME=devuser
ARG USER_UID=1000
ARG USER_GID=${USER_UID}

# --- 1. 基本パッケージのインストール ---
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
    wget gnupg htop lsb-release curl build-essential ca-certificates sudo vim git \
    gfortran python3 python3-pip python3-dev zsh sed software-properties-common \
    ninja-build graphviz linux-tools-common linux-tools-generic \
    libx11-dev libxext-dev libxrender-dev libxt-dev \
    adwaita-icon-theme-full gnuplot valgrind kcachegrind gdb  && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# --- 2. 外部リポジトリの追加 ---
RUN wget -qO- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor > /usr/share/keyrings/oneapi-archive-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" > /etc/apt/sources.list.d/oneAPI.list && \
    curl https://developer.download.nvidia.com/hpc-sdk/ubuntu/DEB-GPG-KEY-NVIDIA-HPC-SDK | gpg --dearmor > /usr/share/keyrings/nvidia-hpcsdk-archive-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/nvidia-hpcsdk-archive-keyring.gpg] https://developer.download.nvidia.com/hpc-sdk/ubuntu/amd64 /" > /etc/apt/sources.list.d/nvhpc.list && \
    wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor > /usr/share/keyrings/kitware-archive-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ $(lsb_release -cs) main" > /etc/apt/sources.list.d/kitware.list

# --- 3. コンパイラとCMakeのインストール ---
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
    intel-oneapi-compiler-fortran \
    intel-oneapi-hpc-toolkit \
    nvhpc-25-3 \
    cmake && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# --- 4. 環境変数スクリプト ---
RUN echo 'export NVARCH=$(uname -s)_$(uname -m)' > /etc/profile.d/nvhpc.sh && \
    echo 'export NVCOMPILERS=/opt/nvidia/hpc_sdk' >> /etc/profile.d/nvhpc.sh && \
    echo 'export PATH=$NVCOMPILERS/$NVARCH/25.3/compilers/bin:$PATH' >> /etc/profile.d/nvhpc.sh && \
    echo 'export MANPATH=$MANPATH:$NVCOMPILERS/$NVARCH/25.3/compilers/man' >> /etc/profile.d/nvhpc.sh && \
    echo 'export PATH=$NVCOMPILERS/$NVARCH/25.3/comm_libs/mpi/bin:$PATH' >> /etc/profile.d/nvhpc.sh && \
    echo 'export MANPATH=$MANPATH:$NVCOMPILERS/$NVARCH/25.3/comm_libs/mpi/man' >> /etc/profile.d/nvhpc.sh && \
    echo '[ -f /opt/intel/oneapi/setvars.sh ] && source /opt/intel/oneapi/setvars.sh' > /etc/profile.d/oneapi.sh

# --- 5. Pythonツールのインストール ---
RUN pip3 install --upgrade pip && \
    pip3 install --force-reinstall git+https://github.com/fortran-lang/fortls && \
    pip3 install --upgrade fprettify fypp ford

# --- 6. 一般ユーザーの作成 ---
RUN groupadd --gid ${USER_GID} ${USERNAME} && \
    useradd --uid ${USER_UID} --gid ${USER_GID} -m -s ${SHELL} ${USERNAME} && \
    echo ${USERNAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USERNAME} && \
    chmod 0440 /etc/sudoers.d/${USERNAME}

# --- 7. ZshとPreztoの設定（devuserで実行）---
USER ${USERNAME}
WORKDIR /home/${USERNAME}

RUN git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto" && \
    git clone --recurse https://github.com/belak/prezto-contrib "${ZDOTDIR:-$HOME}/.zprezto/contrib"

RUN zsh -c 'setopt EXTENDED_GLOB && \
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do \
    ln -sf "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"; \
    done'

RUN sed -i "/'prompt'/c \\\ 'contrib-prompt' \\\\\n  'prompt'" .zpreztorc && \
    sed -i "s/theme 'sorin'/theme 'bart'/g" .zpreztorc

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

# --- 8. 所有権の修正（重要）---
USER root
RUN chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}
USER ${USERNAME}

# --- 9. コンテナ起動時のデフォルトコマンド ---
CMD ["/usr/bin/zsh"]