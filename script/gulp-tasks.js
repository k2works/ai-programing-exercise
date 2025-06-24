'use strict';

/**
 * Gulp tasks definition file
 */

module.exports = function(gulp) {
  // Define a simple task
  gulp.task('hello', function(done) {
    console.log('Hello from gulp task!');
    done();
  });

  // Add more tasks as needed
  gulp.task('build', function(done) {
    console.log('Building project...');
    // Add build logic here
    done();
  });

  gulp.task('default', gulp.series('hello'));
};