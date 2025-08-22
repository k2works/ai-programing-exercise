# Project Structure & Organization

## Root Directory Layout
```
ai-programing-exercise/
├── .devcontainer/          # VS Code dev container configuration
├── .git/                   # Git repository data
├── .github/                # GitHub workflows and templates
├── .kiro/                  # Kiro AI assistant configuration
│   ├── specs/              # Feature specifications
│   └── steering/           # AI assistant steering rules
├── docs/                   # Documentation source files
├── script/                 # Build and automation scripts
├── Dockerfile              # Main development environment
├── docker-compose.yml      # Multi-container orchestration
├── gulpfile.js            # Build task definitions
├── mkdocs.yml             # Documentation configuration
├── package.json           # Node.js dependencies and scripts
└── README.md              # Project overview
```

## Documentation Structure (`docs/`)
```
docs/
├── adr/                   # Architecture Decision Records
├── assets/                # CSS, JS, images for documentation
├── design/                # System design documents
├── development/           # Development guides and procedures
├── journal/               # Auto-generated commit journals
├── operation/             # Operational procedures and runbooks
├── reference/             # API references and technical specs
├── requirements/          # Requirements and user stories
├── Dockerfile             # MkDocs container configuration
└── index.md               # Documentation homepage
```

## Scripts Directory (`script/`)
- `mkdocs.js` - MkDocs server management tasks
- `journal.js` - Git commit journal generation

## Configuration Files
- `mkdocs.yml` - Documentation site configuration with Material theme
- `docker-compose.yml` - Multi-service container orchestration
- `gulpfile.js` - Task runner configuration
- `package.json` - Node.js project metadata and scripts

## Naming Conventions
- **Files**: Use kebab-case for filenames (`my-file.md`)
- **Directories**: Use lowercase with descriptive names
- **Documentation**: Use descriptive Japanese/English mixed naming as appropriate
- **Scripts**: Use camelCase for JavaScript functions and variables

## File Organization Principles
1. **Separation of Concerns**: Keep documentation, code, and configuration separate
2. **Documentation-First**: All features should have corresponding documentation
3. **Automation**: Use scripts for repetitive tasks (journal generation, builds)
4. **Containerization**: All environments defined via Docker
5. **Version Control**: Track all changes with meaningful commit messages

## Key Directories to Know
- `/docs` - All documentation lives here, organized by type
- `/script` - Automation and build scripts
- `/.kiro` - AI assistant configuration and specifications
- Root level - Configuration files and main project files