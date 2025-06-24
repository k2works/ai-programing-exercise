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
    NODE_VER=22 \
    RUBY_VER=3.4.4 \
    BUNDLER_VER=2.6.7

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
            && apt-get clean \
            && rm -rf /var/lib/apt/lists/*

# SDKMANのインストール
RUN apt-get update && apt-get install -y bash && apt-get clean \
    && curl -s "https://get.sdkman.io" | bash \
    && bash -c 'source "$HOME/.sdkman/bin/sdkman-init.sh" \
       && sdk selfupdate \
       && sdk install java "$JAVA_VER" \
       && sdk install maven "$MAVEN_VER" \
       && sdk install gradle "$GRADLE_VER"'

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

# パスの設定
ENV PATH="/root/.sdkman/candidates/java/current/bin:/root/.sdkman/candidates/maven/current/bin:/root/.sdkman/candidates/gradle/current/bin:/root/.rbenv/shims:$PATH"

# 作業ディレクトリの設定
WORKDIR /srv

