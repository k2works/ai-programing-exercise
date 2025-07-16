#!/usr/bin/env php
<?php

require_once 'vendor/autoload.php';

use Symfony\Component\Process\Process;

class FileWatcher
{
    private array $watchPaths;
    private string $command;
    private array $lastModified = [];

    public function __construct(array $watchPaths, string $command)
    {
        $this->watchPaths = $watchPaths;
        $this->command = $command;
    }

    public function watch(): void
    {
        echo "🔍 Watching for file changes in: " . implode(', ', $this->watchPaths) . "\n";
        echo "📋 Command to run: {$this->command}\n";
        echo "⏹️  Press Ctrl+C to stop\n\n";

        // 初回実行
        $this->runCommand();

        while (true) {
            $changed = false;
            
            foreach ($this->watchPaths as $path) {
                if ($this->hasPathChanged($path)) {
                    $changed = true;
                    break;
                }
            }
            
            if ($changed) {
                echo "\n🔄 File change detected, running tests...\n";
                $this->runCommand();
            }
            
            usleep(500000); // 0.5秒待機
        }
    }

    private function hasPathChanged(string $path): bool
    {
        if (!file_exists($path)) {
            return false;
        }

        if (is_file($path)) {
            $currentModified = filemtime($path);
            $lastModified = $this->lastModified[$path] ?? 0;
            
            if ($currentModified > $lastModified) {
                $this->lastModified[$path] = $currentModified;
                return true;
            }
        } elseif (is_dir($path)) {
            $iterator = new RecursiveIteratorIterator(
                new RecursiveDirectoryIterator($path, RecursiveDirectoryIterator::SKIP_DOTS)
            );
            
            foreach ($iterator as $file) {
                if ($file->getExtension() === 'php') {
                    $currentModified = $file->getMTime();
                    $filePath = $file->getPathname();
                    $lastModified = $this->lastModified[$filePath] ?? 0;
                    
                    if ($currentModified > $lastModified) {
                        $this->lastModified[$filePath] = $currentModified;
                        return true;
                    }
                }
            }
        }
        
        return false;
    }

    private function runCommand(): void
    {
        $process = Process::fromShellCommandline($this->command);
        $process->setTimeout(60);
        $process->run();

        if ($process->isSuccessful()) {
            echo "✅ " . $process->getOutput();
        } else {
            echo "❌ " . $process->getErrorOutput();
        }
    }
}

// 引数の処理
$command = $argv[1] ?? 'composer test';
$watchPaths = array_slice($argv, 2) ?: ['src', 'tests'];

$watcher = new FileWatcher($watchPaths, $command);
$watcher->watch();
