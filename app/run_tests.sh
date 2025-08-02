#!/bin/bash
cd /workspaces/ai-programing-exercise/app
swipl -g "consult('main.pl'), run_tests, halt."
