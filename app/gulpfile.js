import gulp from 'gulp';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

// Lint tasks
gulp.task('lint', async () => {
  await execAsync('npm run lint');
});

gulp.task('lint:fix', async () => {
  await execAsync('npm run lint -- --fix --workspaces');
});

// Format tasks
gulp.task('format', async () => {
  await execAsync('npm run format');
});

gulp.task('format:check', async () => {
  await execAsync('npm run format -- --check --workspaces');
});

// Test tasks
gulp.task('test', async () => {
  await execAsync('npm run test');
});

gulp.task('test:watch', async () => {
  await execAsync('npm run test -- --watch --workspaces');
});

gulp.task('test:coverage', async () => {
  await execAsync('npm run test:coverage --workspaces');
});

// Typecheck task
gulp.task('typecheck', async () => {
  await execAsync('npm run typecheck');
});

// Dependency check task
gulp.task('dep:check', async () => {
  await execAsync('npx depcruise --config .dependency-cruiser.js backend/src frontend/src shared/src');
});

// Pre-commit task
gulp.task('pre-commit', gulp.series('lint', 'format:check', 'typecheck', 'test'));

// Watch task
gulp.task('watch', () => {
  gulp.watch(['backend/src/**/*.ts', 'frontend/src/**/*.{ts,tsx}', 'shared/src/**/*.ts'], gulp.series('lint', 'test'));
});

// Default task
gulp.task('default', gulp.series('lint', 'format', 'typecheck', 'test'));
