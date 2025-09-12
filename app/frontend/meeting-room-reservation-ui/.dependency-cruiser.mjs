/** @type {import('dependency-cruiser').IConfiguration} */
export default {
  forbidden: [
    // 循環依存を禁止
    {
      name: 'no-circular',
      severity: 'error',
      comment: 'This dependency is part of a circular relationship. You might want to revise your solution (i.e. use dependency inversion, make sure the modules have a single responsibility) ',
      from: {},
      to: {
        circular: true
      }
    },
    // node_modules への直接依存を禁止（必要に応じてコメントアウト）
    {
      name: 'no-orphans',
      comment: "This is an orphan module - it's likely not used (anymore?). Either use it or remove it. If it's not used (yet) make sure you add the module to your 'not-to-unmangled' list.",
      severity: 'warn',
      from: {
        orphan: true,
        pathNot: [
          '(^|/)\\.[^/]+\\.(js|cjs|mjs|ts|json)$', // dot files
          '\\.d\\.ts$', // TypeScript declaration files
          '(^|/)\\.(tsx?)$', // tsx files in dot folders
        ]
      },
      to: {}
    },
    // フォルダ構造のルール
    {
      name: 'not-to-deprecated',
      comment: 'This module uses a (version of an) npm module that has been deprecated. Either upgrade to a later version of that module, or find an alternative. Deprecated modules are a security risk.',
      severity: 'warn',
      from: {},
      to: {
        dependencyTypes: ['deprecated']
      }
    },
    // 他のレイヤーへの不正なアクセスを禁止
    {
      name: 'no-non-package-json',
      severity: 'error',
      comment: "This module depends on an npm package that isn't in the 'dependencies' or 'devDependencies' of your package.json. That's problematic as the package either (1) won't be available on live (2) will be available on live but not in the package.json (so you don't control its version) (3) will be available on live but you won't know about it being dead weight when you want to remove it.",
      from: {},
      to: {
        dependencyTypes: ['npm-no-pkg', 'npm-unknown']
      }
    },
    {
      name: 'not-to-unresolvable',
      comment: "This module depends on a module that cannot be found ('resolved to disk'). If it's an npm module: add it to your package.json. In all other cases: you likely already know what to do.",
      severity: 'error',
      from: {},
      to: {
        couldNotResolve: true
      }
    }
  ],
  options: {
    // TypeScript + React の設定
    doNotFollow: {
      path: 'node_modules'
    },
    includeOnly: '^src/',
    tsPreCompilationDeps: true,
    preserveSymlinks: false,
    moduleSystems: ['amd', 'cjs', 'es6', 'tsd'],
    tsConfig: {
      fileName: 'tsconfig.json'
    },
    enhancedResolveOptions: {
      exportsFields: ['exports'],
      conditionNames: ['import', 'require', 'node', 'default']
    },
    reporterOptions: {
      dot: {
        collapsePattern: 'node_modules/(@[^/]+/[^/]+|[^/]+)',
        theme: {
          graph: {
            splines: 'ortho'
          }
        }
      },
      archi: {
        collapsePattern: '^src/[^/]+',
        theme: {
          graph: {
            splines: 'ortho'
          }
        }
      }
    }
  }
}