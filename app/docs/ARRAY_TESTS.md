# 配列や繰り返し処理を理解するためのテスト - 実装詳細

Rustで実装した配列と繰り返し処理のテストは、元のRuby入門記事で扱われた概念をRustの標準ライブラリを使って再現しています。

## 実装したテスト一覧

### 1. 基本的な繰り返し処理
```rust
fn test_iteration()
```
- Rubyの`each`メソッドに相当
- `for`ループを使って各要素を2乗する処理

### 2. フィルタリング操作
```rust
fn test_filter_select_elements()   // select相当
fn test_filter_reject_elements()   // reject相当
```
- Rubyの`select`と`reject`メソッドに相当
- `filter()`メソッドを使って条件に合う/合わない要素を抽出

### 3. 変換操作
```rust
fn test_map_transform_elements()   // map/collect相当
```
- Rubyの`map`メソッドに相当
- `map()`メソッドで各要素を変換

### 4. 検索操作
```rust
fn test_find_first_element()       // find/detect相当
fn test_grep_pattern_matching()    // grep相当
```
- Rubyの`find`、`detect`、`grep`メソッドに相当
- `find()`とパターンマッチングで条件に合う要素を検索

### 5. ソート操作
```rust
fn test_sort_with_custom_comparator()
```
- Rubyの`sort`メソッドに相当
- `sort()`と`sort_by()`でカスタム比較による並び替え

### 6. 条件付き取得
```rust
fn test_take_while_condition()     // take_while相当
fn test_skip_while_condition()     // drop_while相当
```
- Rubyの`take_while`と`drop_while`メソッドに相当
- `take_while()`と`skip_while()`で条件付きの要素取得

### 7. 畳み込み演算
```rust
fn test_fold_accumulation()        // inject相当
fn test_reduce_accumulation()      // reduce相当
```
- Rubyの`inject`と`reduce`メソッドに相当
- `fold()`と`reduce()`で累積演算

### 8. Rust特有の機能
```rust
fn test_collect_with_transformation()  // collectを使った変換
fn test_enumerate_with_index()         // インデックス付き繰り返し
fn test_chain_operations()             // メソッドチェーン
fn test_any_all_predicates()           // any/all述語
```

## RubyとRustの対応関係

| Ruby | Rust | 説明 |
|------|------|------|
| `select` | `filter()` | 条件に合う要素を抽出 |
| `reject` | `filter(!condition)` | 条件に合わない要素を抽出 |
| `map` | `map()` | 各要素を変換 |
| `collect` | `collect()` | イテレータを配列に変換 |
| `find`/`detect` | `find()` | 条件に合う最初の要素を検索 |
| `grep` | `filter(pattern)` | パターンに合う要素を抽出 |
| `take_while` | `take_while()` | 条件が真の間は要素を取得 |
| `drop_while` | `skip_while()` | 条件が真の間は要素をスキップ |
| `inject` | `fold()` | 初期値ありの畳み込み |
| `reduce` | `reduce()` | 初期値なしの畳み込み |

## Rustならではの特徴

1. **ゼロコスト抽象化**: イテレータの操作は最適化され、手書きループと同等の性能
2. **遅延評価**: チェーンされた操作は`collect()`が呼ばれるまで実行されない
3. **型安全性**: コンパイル時に型チェックが行われ、実行時エラーを防ぐ
4. **所有権システム**: メモリ安全性が保証される

## テスト実行結果

すべてのテスト（27個）が成功し、Rubyの配列操作がRustで正しく再現できていることが確認されています。

```
test result: ok. 27 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```
