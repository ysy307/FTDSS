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
    zsh \
    sed \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Intel OneAPI のリポジトリを追加
RUN wget -qO- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null && \
    echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list && \
    apt-get update && \
    apt-get install -y intel-oneapi-compiler-fortran intel-oneapi-hpc-toolkit && \
    touch /root/.zprofile && \
    echo 'source /opt/intel/oneapi/setvars.sh' >> /root/.zprofile

# Pythonツールをインストール
RUN pip3 install --upgrade pip && \
    pip3 install --force-reinstall git+https://github.com/fortran-lang/fortls && \
    pip3 install --upgrade fprettify && \
    echo '{ "include_dirs": ["/workspaces/FTDSS/build/mod", "/opt/intel/oneapi/mpi/2021.14/include/mpi","/opt/intel/oneapi/2025.0/include/mpi"] }' > /root/.fortls

# ZshとPreztoの設定
ENV SHELL=/usr/bin/zsh
SHELL ["/bin/zsh", "-c"]

# Preztoをクローンして設定
RUN git clone --recursive https://github.com/sorin-ionescu/prezto.git $HOME/.zprezto && \
    git clone --recurse https://github.com/belak/prezto-contrib $HOME/.zprezto/contrib && \
    setopt EXTENDED_GLOB && \
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do \
    ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"; \
    done && \
    sed -i "/'prompt'/c \\\ 'contrib-prompt' \\\\\n  'prompt'" /root/.zpreztorc && \
    sed -i "s/theme 'sorin'/theme 'bart'/g" /root/.zpreztorc && \
    echo 'export PS1="%~ %# "' >> /root/.zshrc && \
    echo 'export PATH=$PATH:/workspaces/FTDSS/fpm' >> ~/.zshrc


# デフォルトのシェルをZshに設定
RUN chsh -s /usr/bin/zsh

CMD ["/usr/bin/zsh"]
