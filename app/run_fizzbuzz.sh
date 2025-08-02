#!/bin/bash
cd /workspaces/ai-programing-exercise/app
swipl -g "consult('main.pl'), fizzbuzz_print(1, 100)." -t halt
