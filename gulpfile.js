'use strict';

/**
 * Gulpfile that loads tasks from the script directory
 */

import gulp from 'gulp';
import mkdocsTasks from './script/mkdocs.js';
import journalTasks from './script/journal.js';

// Load gulp tasks from script modules
mkdocsTasks(gulp);
journalTasks(gulp);

// Export gulp to make it available to the gulp CLI
export default gulp;
