@echo off
setlocal EnableDelayedExpansion

echo =========================================
echo MRS 負荷テスト実行
echo =========================================

REM プロジェクトルートディレクトリに移動
cd /d "%~dp0\.."

REM 1. アプリケーション状態確認
echo.
echo 1. アプリケーション状態確認
curl -s -o nul -w "%%{http_code}" http://localhost:8080/actuator/health > temp.txt
set /p HTTP_CODE=<temp.txt
del temp.txt

if "%HTTP_CODE%"=="200" (
    echo [OK] アプリケーションは起動しています
) else (
    echo [ERROR] アプリケーションが起動していません
    echo アプリケーションを起動してください: mvn spring-boot:run
    exit /b 1
)

REM 2. Java並行処理テストの実行
echo.
echo 2. Java並行処理テスト実行
echo 100ユーザーの同時予約テストを実行中...

call mvn test -Dtest=ConcurrentReservationTest -DfailIfNoTests=false

if %ERRORLEVEL% == 0 (
    echo [OK] Java並行処理テスト完了
) else (
    echo [ERROR] Java並行処理テストが失敗しました
)

REM 3. JMeter負荷テストの実行
echo.
echo 3. JMeter負荷テスト実行

REM JMeterの存在確認
where jmeter >nul 2>&1
if %ERRORLEVEL% == 0 (
    echo JMeterを使用した負荷テストを開始します...
    
    REM テスト結果ディレクトリの作成
    for /f "tokens=1-3 delims=/ " %%a in ('date /t') do set DATE=%%c%%a%%b
    for /f "tokens=1-2 delims=: " %%a in ('time /t') do set TIME=%%a%%b
    set TIME=%TIME: =%
    set RESULT_DIR=target\jmeter-results\%DATE%_%TIME%
    
    if not exist "%RESULT_DIR%" mkdir "%RESULT_DIR%"
    
    REM JMeterテストの実行
    jmeter -n ^
        -t src\test\jmeter\MRS_LoadTest.jmx ^
        -l "%RESULT_DIR%\results.jtl" ^
        -e -o "%RESULT_DIR%\html-report" ^
        -Jjmeter.save.saveservice.output_format=csv ^
        -Jjmeter.save.saveservice.print_field_names=true
    
    if %ERRORLEVEL% == 0 (
        echo [OK] JMeter負荷テスト完了
        echo レポート: %RESULT_DIR%\html-report\index.html
    ) else (
        echo [ERROR] JMeter負荷テストが失敗しました
    )
) else (
    echo [WARNING] JMeterがインストールされていません
    echo JMeterをインストールするには:
    echo   https://jmeter.apache.org/ からダウンロード
    echo   環境変数PATHにJMeterのbinディレクトリを追加
)

REM 4. 結果サマリー
echo.
echo =========================================
echo 負荷テスト結果サマリー
echo =========================================

REM テスト結果ファイルの確認
if exist "target\surefire-reports\com.example.mrs.performance.ConcurrentReservationTest.txt" (
    echo.
    echo Java並行処理テスト結果:
    findstr /C:"Tests run:" /C:"成功数:" /C:"競合数:" /C:"エラー数:" /C:"デッドロック" "target\surefire-reports\com.example.mrs.performance.ConcurrentReservationTest.txt"
)

if exist "%RESULT_DIR%" (
    echo.
    echo JMeter結果ファイル:
    echo   - 生データ: %RESULT_DIR%\results.jtl
    echo   - HTMLレポート: %RESULT_DIR%\html-report\index.html
)

echo.
echo 負荷テスト完了

endlocal