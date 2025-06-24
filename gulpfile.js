'use strict';

/**
 * Gulpfile that loads tasks from the script directory
 */

const gulp = require('gulp');

// Load gulp tasks from script directory
require('./script/gulp-tasks.js')(gulp);

// Export gulp to make it available to the gulp CLI
module.exports = gulp;