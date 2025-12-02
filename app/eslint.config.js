const { FlatCompat } = require('@eslint/eslintrc');
const js = require('@eslint/js');
const typescriptEslint = require('@typescript-eslint/eslint-plugin');
const typescriptParser = require('@typescript-eslint/parser');
const importPlugin = require('eslint-plugin-import');

const compat = new FlatCompat({
  baseDirectory: __dirname,
  recommendedConfig: js.configs.recommended,
});

module.exports = [
  {
    ignores: ['node_modules/**', 'dist/**', '**/*.js', '!eslint.config.js'],
  },
  ...compat.extends(
    'plugin:import/recommended',
    'plugin:import/typescript',
    'plugin:@typescript-eslint/recommended'
  ),
  {
    files: ['src/**/*.ts'],
    languageOptions: {
      parser: typescriptParser,
      parserOptions: {
        ecmaVersion: 'latest',
        sourceType: 'module',
      },
      globals: {
        node: true,
        es6: true,
      },
    },
    plugins: {
      '@typescript-eslint': typescriptEslint,
      import: importPlugin,
    },
    settings: {
      'import/resolver': {
        typescript: {},
      },
    },
    rules: {
      'import/no-restricted-paths': [
        'error',
        {
          zones: [
            // Domain層が依存してはいけない領域
            {
              from: './src/Application/**/*',
              target: './src/Domain/**/!(*.spec.ts|*.test.ts)',
              message: 'Domain層でApplication層をimportしてはいけません。',
            },
            {
              from: './src/Presentation/**/*',
              target: './src/Domain/**/!(*.spec.ts|*.test.ts)',
              message: 'Domain層でPresentation層をimportしてはいけません。',
            },
            {
              from: './src/Infrastructure/**/*!(test).ts',
              target: './src/Domain/**/!(*.spec.ts|*.test.ts)',
              message: 'Domain層でInfrastructure層をimportしてはいけません。',
            },
            // Application層が依存してはいけない領域
            {
              from: './src/Presentation/**/*',
              target: './src/Application/**/!(*.spec.ts|*.test.ts)',
              message: 'Application層でPresentation層をimportしてはいけません。',
            },
            {
              from: './src/Infrastructure/**/*',
              target: './src/Application/**/!(*.spec.ts|*.test.ts)',
              message:
                'Application層でInfrastructure層をimportしてはいけません。',
            },
          ],
        },
      ],
    },
  },
];
