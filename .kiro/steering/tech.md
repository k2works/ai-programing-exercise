# Technology Stack & Build System

## Core Technologies
- **Base OS**: Ubuntu 22.04
- **Containerization**: Docker & Docker Compose
- **Documentation**: MkDocs with Material theme
- **Build Automation**: Gulp.js with Node.js
- **Version Control**: Git with automated journal generation

## Programming Languages Supported
- **JVM**: Java 21.0.2, Scala 3.4.0, Kotlin 2.0.0, Clojure 1.12.1
- **Web**: Node.js 22, PHP 8.1
- **Scripting**: Ruby 3.4.4, Python 3.12
- **Systems**: Go 1.22.0, Rust stable, C/C++ (C11/C++20)
- **Functional**: Haskell 9.4.8, Erlang 26.2.1, Elixir 1.16.1, Prolog (SWI-Prolog)
- **Enterprise**: .NET 8.0

## Package Managers & Tools
- **Java**: Maven 3.9.4, Gradle 8.10.2 (via SDKMAN)
- **Node.js**: npm, yarn, nvm
- **Python**: uv (modern Python package manager)
- **Ruby**: rbenv, bundler
- **PHP**: Composer
- **Haskell**: GHCup, Cabal, Stack
- **Rust**: rustup, cargo

## Common Commands

### Documentation
```bash
# Start documentation server
npm run docs:serve
# or
npx gulp mkdocs:serve

# Build documentation
npm run docs:build

# Stop documentation server
npm run docs:stop
```

### Development Environment
```bash
# Start development container
docker-compose up -d app

# Build and run with specific image
docker build -t ai-programming-dev .
docker run -it -v $(pwd):/srv ai-programming-dev bash
```

### Journal Generation
```bash
# Generate journal for all commit dates
npm run journal

# Generate journal for specific date
npx gulp journal:generate:date --date=YYYY-MM-DD
```

### Container Registry
```bash
# Pull from GitHub Container Registry
docker pull ghcr.io/k2works/ai-programing-exercise/core:0.1.0

# Tag and push (automated via GitHub Actions on tag push)
git tag 0.0.x
git push origin 0.0.x
```

## Development Workflow
1. Use Docker containers for consistent development environment
2. Document changes in MkDocs format
3. Commit changes trigger automatic journal generation
4. Use PlantUML/Mermaid for architectural diagrams
5. Deploy documentation via GitHub Pages