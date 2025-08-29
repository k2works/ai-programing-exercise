# ベースイメージとして Ubuntu 22.04 を使用
FROM ubuntu:22.04 AS base

# 環境変数の設定
ENV DEBIAN_FRONTEND=noninteractive \
    LANG=ja_JP.UTF-8 \
    LC_ALL=ja_JP.UTF-8 \
    LC_CTYPE=ja_JP.UTF-8 \
    JAVA_VER=21.0.2-tem \
    MAVEN_VER=3.9.4 \
    GRADLE_VER=8.10.2 \
    SCALA_VER=3.4.0 \
    KOTLIN_VER=2.0.0 \
    NODE_VER=22 \
    RUBY_VER=3.4.4 \
    BUNDLER_VER=2.6.7 \
    PYTHON_VER=3.12 \
    PHP_VER=8.1 \
    GHC_VER=9.4.8 \
    GO_VER=1.22.0 \
    DOTNET_VER=9.0 \
    RUST_VER=stable \
    C_VER=11 \
    CPP_VER=20 \
    ERLANG_VER=26.2.1 \
    ELIXIR_VER=1.16.1

# ユーザーの設定
ARG USERNAME=developer
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    #
    # [Optional] Add sudo support. Omit if you don't need to install software after connecting.
    && apt-get update \
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

# ロケールのセットアップ
RUN apt-get update && apt-get install -y \
    language-pack-ja-base \
    language-pack-ja \
    && update-locale LANG=ja_JP.UTF-8 LANGUAGE=ja_JP:ja \
    && rm -rf /var/lib/apt/lists/*

# 基本的なパッケージのインストール
 RUN apt-get update && \
     apt-get install -y \
            build-essential \
            zip \
            unzip \
            git \
            curl \
            wget \
            vim \
            tmux \
            && apt-get clean \
            && rm -rf /var/lib/apt/lists/*

 # C/C++言語開発ツールのインストール
 RUN apt-get update && apt-get install -y \
     gcc \
     g++ \
     gdb \
     make \
     cmake \
     libc6-dev \
     libncurses5-dev \
     clang \
     clang-format \
     clang-tidy \
     valgrind \
     libboost-all-dev \
     libfmt-dev \
     libjsoncpp-dev \
     libgtest-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# SDKMANのインストール
RUN apt-get update && apt-get install -y bash && apt-get clean \
    && curl -s "https://get.sdkman.io" | HOME=/home/$USERNAME bash \
    && chown -R $USERNAME:$USERNAME /home/$USERNAME/.sdkman \
    && echo '#!/bin/bash\n\
source "/home/'$USERNAME'/.sdkman/bin/sdkman-init.sh"\n\
sdk selfupdate\n\
sdk install java "'$JAVA_VER'"\n\
sdk install maven "'$MAVEN_VER'"\n\
sdk install gradle "'$GRADLE_VER'"\n\
sdk install scala "'$SCALA_VER'"\n\
sdk install kotlin "'$KOTLIN_VER'"' > /tmp/sdkman_install.sh \
    && chmod +x /tmp/sdkman_install.sh \
    && su - $USERNAME -c /tmp/sdkman_install.sh \
    && rm /tmp/sdkman_install.sh

# Clojureのインストール
RUN apt-get update && apt-get install -y curl bash rlwrap && apt-get clean \
    && curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh \
    && chmod +x linux-install.sh \
    && ./linux-install.sh \
    && rm linux-install.sh \
    && echo '#!/bin/bash\n\
source "/home/'$USERNAME'/.sdkman/bin/sdkman-init.sh"\n\
sdk selfupdate\n\
if [ ! -z "'$LEININGEN_VER'" ]; then sdk install leiningen "'$LEININGEN_VER'"; fi' > /tmp/clojure_install.sh \
    && chmod +x /tmp/clojure_install.sh \
    && su - $USERNAME -c /tmp/clojure_install.sh \
    && rm /tmp/clojure_install.sh

# Node.jsのインストール
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g yarn \
    && mkdir -p /home/$USERNAME/.nvm \
    && chown -R $USERNAME:$USERNAME /home/$USERNAME/.nvm \
    && echo '#!/bin/bash\n\
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash\n\
. /home/'$USERNAME'/.nvm/nvm.sh\n\
nvm install "'$NODE_VER'"\n\
nvm use "'$NODE_VER'"' > /tmp/nvm_install.sh \
    && chmod +x /tmp/nvm_install.sh \
    && su - $USERNAME -c /tmp/nvm_install.sh \
    && rm /tmp/nvm_install.sh

# Rubyのインストール用の依存パッケージをインストール
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libreadline-dev \
    zlib1g-dev \
    autoconf \
    bison \
    libyaml-dev \
    libncurses5-dev \
    libffi-dev \
    libgdbm-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Rubyのインストール
RUN mkdir -p /home/$USERNAME/.rbenv \
    && chown -R $USERNAME:$USERNAME /home/$USERNAME/.rbenv \
    && echo '#!/bin/bash\n\
git clone https://github.com/sstephenson/rbenv /home/'$USERNAME'/.rbenv\n\
git clone https://github.com/sstephenson/ruby-build.git /home/'$USERNAME'/.rbenv/plugins/ruby-build\n\
/home/'$USERNAME'/.rbenv/plugins/ruby-build/install.sh\n\
echo "export PATH=\"/home/'$USERNAME'/.rbenv/bin:\$PATH\"" >> /home/'$USERNAME'/.bashrc\n\
echo "eval \"\$(rbenv init -)\"" >> /home/'$USERNAME'/.bashrc\n\
export PATH="/home/'$USERNAME'/.rbenv/bin:$PATH"\n\
eval "$(rbenv init -)"\n\
rbenv install "'$RUBY_VER'"\n\
rbenv global "'$RUBY_VER'"' > /tmp/rbenv_install.sh \
    && chmod +x /tmp/rbenv_install.sh \
    && su - $USERNAME -c /tmp/rbenv_install.sh \
    && rm /tmp/rbenv_install.sh

# Pythonの依存パッケージをインストール
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-dev \
    python3-venv \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# uvパッケージマネージャーのインストール
RUN pip3 install --upgrade pip \
    && pip3 install uv \
    && uv --version

# PHPのインストール
RUN apt-get update && apt-get install -y \
    software-properties-common \
    && add-apt-repository -y ppa:ondrej/php \
    && apt-get update \
    && apt-get install -y \
    php${PHP_VER} \
    php${PHP_VER}-cli \
    php${PHP_VER}-fpm \
    php${PHP_VER}-common \
    php${PHP_VER}-mysql \
    php${PHP_VER}-zip \
    php${PHP_VER}-gd \
    php${PHP_VER}-mbstring \
    php${PHP_VER}-curl \
    php${PHP_VER}-xml \
    php${PHP_VER}-bcmath \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Composerのインストール
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

# Haskellのインストール
RUN apt-get update && apt-get install -y \
    libffi-dev \
    libgmp-dev \
    libtinfo-dev \
    zlib1g-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /home/$USERNAME/.ghcup \
    && chown -R $USERNAME:$USERNAME /home/$USERNAME/.ghcup \
    && echo '#!/bin/bash\n\
BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION='$GHC_VER' \
curl --proto "=https" --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh\n\
. /home/'$USERNAME'/.ghcup/env\n\
ghcup install ghc '$GHC_VER'\n\
ghcup set ghc '$GHC_VER'\n\
ghcup install cabal latest\n\
ghcup install stack latest\n\
ghcup install hls latest\n\
echo "export PATH=\"\$HOME/.ghcup/bin:\$PATH\"" >> /home/'$USERNAME'/.bashrc' > /tmp/ghcup_install.sh \
    && chmod +x /tmp/ghcup_install.sh \
    && su - $USERNAME -c /tmp/ghcup_install.sh \
    && rm /tmp/ghcup_install.sh

# Goのインストール
RUN wget https://golang.org/dl/go${GO_VER}.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf go${GO_VER}.linux-amd64.tar.gz \
    && rm go${GO_VER}.linux-amd64.tar.gz \
    && echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc

# Rustのインストール
RUN apt-get update && apt-get install -y \
    build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /home/$USERNAME/.cargo \
    && chown -R $USERNAME:$USERNAME /home/$USERNAME/.cargo \
    && echo '#!/bin/bash\n\
curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain "'$RUST_VER'"\n\
echo "source /home/'$USERNAME'/.cargo/env" >> /home/'$USERNAME'/.bashrc' > /tmp/rust_install.sh \
    && chmod +x /tmp/rust_install.sh \
    && su - $USERNAME -c /tmp/rust_install.sh \
    && rm /tmp/rust_install.sh

# .NET SDKのインストール
RUN apt-get update && apt-get install -y \
    wget \
    apt-transport-https \
    && wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && rm packages-microsoft-prod.deb \
    && apt-get update \
    && apt-get install -y dotnet-sdk-${DOTNET_VER} \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# ErlangとElixirのインストール
RUN apt-get update && apt-get install -y \
    build-essential \
    autoconf \
    libncurses5-dev \
    libwxgtk3.0-gtk3-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libpng-dev \
    libssh-dev \
    unixodbc-dev \
    xsltproc \
    fop \
    libxml2-utils \
    libncurses-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /home/$USERNAME/.asdf \
    && chown -R $USERNAME:$USERNAME /home/$USERNAME/.asdf \
    && echo '#!/bin/bash\n\
git clone https://github.com/asdf-vm/asdf.git /home/'$USERNAME'/.asdf --branch v0.13.1\n\
echo ". \"/home/'$USERNAME'/.asdf/asdf.sh\"" >> /home/'$USERNAME'/.bashrc\n\
echo ". \"/home/'$USERNAME'/.asdf/completions/asdf.bash\"" >> /home/'$USERNAME'/.bashrc\n\
source "/home/'$USERNAME'/.asdf/asdf.sh"\n\
asdf plugin add erlang\n\
asdf plugin add elixir\n\
asdf install erlang "'$ERLANG_VER'"\n\
asdf global erlang "'$ERLANG_VER'"\n\
asdf install elixir "'$ELIXIR_VER'"\n\
asdf global elixir "'$ELIXIR_VER'"\n\
mix local.hex --force\n\
mix local.rebar --force' > /tmp/asdf_install.sh \
    && chmod +x /tmp/asdf_install.sh \
    && su - $USERNAME -c /tmp/asdf_install.sh \
    && rm /tmp/asdf_install.sh

# Prologのインストール
RUN apt-get update && apt-get install -y \
    swi-prolog \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && echo 'alias prolog="swipl"' >> /home/$USERNAME/.bashrc

# Gemini CLIのインストール
RUN npm install -g @google/gemini-cli

# Claude Codeのインストール
RUN npm install -g @anthropic-ai/claude-code

# すべてのインストールが完了した後、ユーザーのホームディレクトリの所有権を確保
RUN chown -R $USERNAME:$USERNAME /home/$USERNAME

# パスの設定
ENV PATH="/home/$USERNAME/.cargo/bin:/usr/local/go/bin:/home/$USERNAME/.ghcup/bin:/home/$USERNAME/.sdkman/candidates/java/current/bin:/home/$USERNAME/.sdkman/candidates/maven/current/bin:/home/$USERNAME/.sdkman/candidates/gradle/current/bin:/home/$USERNAME/.sdkman/candidates/scala/current/bin:/home/$USERNAME/.sdkman/candidates/kotlin/current/bin:/home/$USERNAME/.rbenv/shims:/usr/share/dotnet:/usr/share/dotnet/tools:/usr/local/bin:/usr/lib/elixir/bin:/usr/local/bin:$PATH"

# 作業ディレクトリの設定
WORKDIR /srv

# ユーザーを設定したユーザーに切り替える
USER $USERNAME

# デフォルトのシェルを bash に設定
SHELL ["/bin/bash", "-c"]