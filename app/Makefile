# デフォルトターゲット
.PHONY: all clean test format lint coverage build help

# ヘルプメッセージ
help:
	@echo "利用可能なコマンド:"
	@echo "  build      - アプリケーションをビルド"
	@echo "  test       - テストを実行"
	@echo "  format     - コードフォーマットを実行"
	@echo "  lint       - 静的コード解析を実行" 
	@echo "  coverage   - コードカバレッジを実行"
	@echo "  clean      - ビルドファイルを削除"
	@echo "  all        - すべてのチェックを実行 (デフォルト)"

# デフォルトターゲット
all: build test

# ビルドディレクトリの作成とビルド実行
build:
	@echo "Building application..."
	mkdir -p build
	cd build && cmake .. && make

# テスト実行
test: build
	@echo "Running tests..."
	cd build && ctest --verbose

# コードフォーマット
format:
	@echo "Running code formatter..."
	find . -name "*.cpp" -o -name "*.h" | xargs clang-format -i -style=file

# Lint (静的解析)
lint:
	@echo "Running static analysis..."
	find . -name "*.cpp" | xargs clang-tidy -config-file=.clang-tidy
	@echo "Running cppcheck..."
	cppcheck --enable=all --std=c++17 --suppress=missingIncludeSystem .

# カバレッジ
coverage: 
	@echo "Running coverage analysis..."
	mkdir -p build_coverage
	cd build_coverage && cmake -DENABLE_COVERAGE=ON .. && make
	cd build_coverage && ctest
	cd build_coverage && gcov -r *.gcno
	@echo "Coverage files generated in build_coverage/"

# クリーンアップ
clean:
	@echo "Cleaning build files..."
	rm -rf build build_coverage
	find . -name "*.gcov" -delete
	find . -name "*.gcno" -delete  
	find . -name "*.gcda" -delete