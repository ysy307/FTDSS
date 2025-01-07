# Ubuntu 22.04をベースに使用
FROM ubuntu:22.04

# 作業ディレクトリを設定
WORKDIR /usr/src/app

# 必要なツールと依存パッケージをインストール
RUN apt-get update -y && apt-get install -y \
    wget \
    gnupg \
    lsb-release \
    curl \
    build-essential \
    ca-certificates \
    sudo \
    vim \
    git \
    gfortran \
    python3 \
    python3-pip \
    python3-dev \
    && apt-get clean

# Intel OneAPI のリポジトリを追加
RUN wget -qO- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null && \
    echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list && \
    sudo apt update && \
    sudo apt install -y intel-oneapi-compiler-fortran && \
    sudo apt install -y intel-oneapi-hpc-toolkit && \
    echo 'source /opt/intel/oneapi/setvars.sh' >> /root/.bashrc

# Intel OneAPI のパスを環境変数に追加
ENV PATH="/opt/intel/oneapi/compiler/latest/linux/bin/intel64:${PATH}"

# Pythonツールをインストール
RUN pip3 install --upgrade pip && \
    pip3 install --force-reinstall git+https://github.com/fortran-lang/fortls && \
    pip3 install --upgrade fprettify && \
    echo '{ "include_dirs": ["/workspaces/FTDSS/build/mod"] }' > /root/.fortls

# .profile に設定を追加
RUN echo '\
    # BASH_VERSION が存在する場合、.bashrc を読み込む\n\
    if [ -n "$BASH_VERSION" ]; then\n\
    if [ -f "$HOME/.bashrc" ]; then\n\
    . "$HOME/.bashrc"\n\
    fi\n\
    fi\n\
    \n\
    # ユーザーの private bin を PATH に追加\n\
    if [ -d "$HOME/bin" ] ; then\n\
    PATH="$HOME/bin:$PATH"\n\
    fi\n\
    \n\
    # ユーザーの .local/bin を PATH に追加\n\
    if [ -d "$HOME/.local/bin" ] ; then\n\
    PATH="$HOME/.local/bin:$PATH"\n\
    fi\n\
    \n\
    # DISPLAY 環境変数を設定\n\
    export DISPLAY=:0\n\
    \n\
    # スタックサイズ制限を無制限に設定\n\
    ulimit -s unlimited\n\
    ' >> /root/.profile

# 作業ディレクトリを設定
WORKDIR /usr/src/app

# コンパイルや実行に使うデフォルトのコマンド
CMD ["/bin/bash"]
