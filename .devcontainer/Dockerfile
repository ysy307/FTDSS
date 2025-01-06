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
    gfortran\
    && apt-get clean

# Intel OneAPI のリポジトリを追加
RUN wget -qO- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null && \
    echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list && \
    sudo apt update && \
    sudo apt install -y intel-oneapi-compiler-fortran && \
    sudo apt install -y intel-oneapi-hpc-toolkit && \
    PATH=/opt/intel/oneapi/compiler/latest/linux/bin/intel64:$PATH && \
    export PS1='\[\e[32m\]\u@\h:\[\e[34m\]\w\[\e[0m\]\$ '


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
    # Intel oneAPI のセットアップスクリプトを実行\n\
    source /opt/intel/oneapi/setvars.sh\n\
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
