#!/bin/bash

# 負荷テスト実行スクリプト
# JMeterとJavaの並行処理テストを実行

echo "========================================="
echo "MRS 負荷テスト実行"
echo "========================================="

# カラー出力の定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# プロジェクトルートディレクトリに移動
cd "$(dirname "$0")/.." || exit 1

# 1. アプリケーションが起動しているか確認
echo -e "\n${YELLOW}1. アプリケーション状態確認${NC}"
if curl -s -o /dev/null -w "%{http_code}" http://localhost:8080/actuator/health | grep -q "200"; then
    echo -e "${GREEN}✓ アプリケーションは起動しています${NC}"
else
    echo -e "${RED}✗ アプリケーションが起動していません${NC}"
    echo "アプリケーションを起動してください: mvn spring-boot:run"
    exit 1
fi

# 2. Java並行処理テストの実行
echo -e "\n${YELLOW}2. Java並行処理テスト実行${NC}"
echo "100ユーザーの同時予約テストを実行中..."

mvn test -Dtest=ConcurrentReservationTest -DfailIfNoTests=false

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Java並行処理テスト完了${NC}"
else
    echo -e "${RED}✗ Java並行処理テストが失敗しました${NC}"
fi

# 3. JMeter負荷テストの実行（JMeterがインストールされている場合）
echo -e "\n${YELLOW}3. JMeter負荷テスト実行${NC}"

# JMeterの存在確認
if command -v jmeter &> /dev/null; then
    echo "JMeterを使用した負荷テストを開始します..."
    
    # テスト結果ディレクトリの作成
    RESULT_DIR="target/jmeter-results/$(date +%Y%m%d_%H%M%S)"
    mkdir -p "$RESULT_DIR"
    
    # JMeterテストの実行
    jmeter -n \
        -t src/test/jmeter/MRS_LoadTest.jmx \
        -l "$RESULT_DIR/results.jtl" \
        -e -o "$RESULT_DIR/html-report" \
        -Jjmeter.save.saveservice.output_format=csv \
        -Jjmeter.save.saveservice.print_field_names=true
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ JMeter負荷テスト完了${NC}"
        echo "レポート: $RESULT_DIR/html-report/index.html"
    else
        echo -e "${RED}✗ JMeter負荷テストが失敗しました${NC}"
    fi
else
    echo -e "${YELLOW}JMeterがインストールされていません${NC}"
    echo "JMeterをインストールするには:"
    echo "  brew install jmeter (Mac)"
    echo "  または https://jmeter.apache.org/ からダウンロード"
fi

# 4. 結果サマリー
echo -e "\n${YELLOW}=========================================${NC}"
echo -e "${YELLOW}負荷テスト結果サマリー${NC}"
echo -e "${YELLOW}=========================================${NC}"

# テスト結果ファイルの確認
if [ -f "target/surefire-reports/com.example.mrs.performance.ConcurrentReservationTest.txt" ]; then
    echo -e "\n${GREEN}Java並行処理テスト結果:${NC}"
    grep -E "(Tests run:|成功数:|競合数:|エラー数:|デッドロック)" target/surefire-reports/com.example.mrs.performance.ConcurrentReservationTest.txt | tail -10
fi

if [ -d "$RESULT_DIR" ]; then
    echo -e "\n${GREEN}JMeter結果ファイル:${NC}"
    echo "  - 生データ: $RESULT_DIR/results.jtl"
    echo "  - HTMLレポート: $RESULT_DIR/html-report/index.html"
fi

echo -e "\n${GREEN}負荷テスト完了${NC}"