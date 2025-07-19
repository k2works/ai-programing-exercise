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
    RUST_VER=stable \
    C_VER=11 \
    CPP_VER=20

# ロケールのセットアップ
RUN apt-get update && apt-get install -y \
    language-pack-ja-base \
    language-pack-ja \
    && update-locale LANG=ja_JP.UTF-8 LANGUAGE=ja_JP:ja \
    && rm -rf /var/lib/apt/lists/*

# 基本的なパッケージのインストール
 RUN apt-get update && \
     apt-get install -y \
            sudo \
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
    && curl -s "https://get.sdkman.io" | bash \
    && bash -c 'source "$HOME/.sdkman/bin/sdkman-init.sh" \
       && sdk selfupdate \
       && sdk install java "$JAVA_VER" \
       && sdk install maven "$MAVEN_VER" \
       && sdk install gradle "$GRADLE_VER" \
       && sdk install scala "$SCALA_VER" \
       && sdk install kotlin "$KOTLIN_VER"'

# Clojureのインストール
RUN apt-get update && apt-get install -y curl bash rlwrap && apt-get clean \
    && curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh \
    && chmod +x linux-install.sh \
    && ./linux-install.sh \
    && rm linux-install.sh \
    && bash -c 'source "$HOME/.sdkman/bin/sdkman-init.sh" \
       && sdk selfupdate \
       && sdk install leiningen "$LEININGEN_VER"'

# Node.jsのインストール
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g yarn \
    && curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash \
    && . $HOME/.nvm/nvm.sh \
    && nvm install $NODE_VER \
    && nvm use $NODE_VER

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
RUN git clone https://github.com/sstephenson/rbenv ~/.rbenv \
    && git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build \
    && ~/.rbenv/plugins/ruby-build/install.sh \
    && echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc \
    && echo 'eval "$(rbenv init -)"' >> ~/.bashrc \
    && bash -c 'export PATH="$HOME/.rbenv/bin:$PATH" \
       && eval "$(rbenv init -)" \
       && rbenv install $RUBY_VER \
       && rbenv global $RUBY_VER'

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
    && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh \
    && . /root/.ghcup/env \
    && ghcup install ghc $GHC_VER \
    && ghcup set ghc $GHC_VER \
    && ghcup install cabal latest \
    && ghcup install stack latest \
    && ghcup install hls latest

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
    && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain $RUST_VER \
    && echo 'source $HOME/.cargo/env' >> ~/.bashrc

# .NET SDKのインストール
RUN apt-get update && apt-get install -y \
    wget \
    apt-transport-https \
    && wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && rm packages-microsoft-prod.deb \
    && apt-get update \
    && apt-get install -y dotnet-sdk-8.0 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Gemini CLIのインストール
RUN npm install -g @google/gemini-cli

# Claude Codeのインストール
RUN npm install -g @anthropic-ai/claude-code

# パスの設定
ENV PATH="/root/.cargo/bin:/usr/local/go/bin:/root/.ghcup/bin:/root/.sdkman/candidates/java/current/bin:/root/.sdkman/candidates/maven/current/bin:/root/.sdkman/candidates/gradle/current/bin:/root/.sdkman/candidates/scala/current/bin:/root/.sdkman/candidates/kotlin/current/bin:/root/.rbenv/shims:/usr/share/dotnet:/usr/share/dotnet/tools:/usr/local/bin:$PATH"

# 作業ディレクトリの設定
WORKDIR /srv
