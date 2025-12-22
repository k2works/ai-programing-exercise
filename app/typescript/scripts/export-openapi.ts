/**
 * OpenAPI仕様をYAML形式でエクスポートするスクリプト
 *
 * 使用方法:
 * 1. APIサーバーを起動: npm run dev:api
 * 2. 別ターミナルでこのスクリプトを実行: tsx scripts/export-openapi.ts
 */

import * as fs from 'fs';
import * as yaml from 'js-yaml';
import * as path from 'path';

const API_URL = 'http://localhost:3000/docs/json';
const OUTPUT_DIR = path.join(process.cwd(), 'api-docs');
const OUTPUT_FILE = path.join(OUTPUT_DIR, 'openapi.yaml');

async function exportOpenAPI() {
  try {
    console.log('OpenAPI仕様を取得中...');
    console.log(`URL: ${API_URL}`);

    const response = await fetch(API_URL);

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const openApiSpec = await response.json();

    // 出力ディレクトリが存在しない場合は作成
    if (!fs.existsSync(OUTPUT_DIR)) {
      fs.mkdirSync(OUTPUT_DIR, { recursive: true });
      console.log(`ディレクトリを作成: ${OUTPUT_DIR}`);
    }

    // JSON形式で保存
    const jsonFile = path.join(OUTPUT_DIR, 'openapi.json');
    fs.writeFileSync(jsonFile, JSON.stringify(openApiSpec, null, 2));
    console.log(`JSON形式で保存: ${jsonFile}`);

    // YAML形式に変換して保存
    const yamlContent = yaml.dump(openApiSpec, {
      indent: 2,
      lineWidth: 120,
      noRefs: true,
    });
    fs.writeFileSync(OUTPUT_FILE, yamlContent);
    console.log(`YAML形式で保存: ${OUTPUT_FILE}`);

    console.log('\n✅ OpenAPI仕様のエクスポートが完了しました');
    console.log(`\n📁 出力ファイル:`);
    console.log(`   - ${jsonFile}`);
    console.log(`   - ${OUTPUT_FILE}`);
  } catch (error) {
    console.error('❌ OpenAPI仕様のエクスポートに失敗しました:', error);
    if (error instanceof Error && error.message.includes('ECONNREFUSED')) {
      console.error(
        '\n⚠️ APIサーバーが起動していない可能性があります。'
      );
      console.error('   以下のコマンドでサーバーを起動してください:');
      console.error('   npm run dev:api');
    }
    process.exit(1);
  }
}

exportOpenAPI();
