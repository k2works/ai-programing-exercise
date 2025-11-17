// シードデータの検証テスト
// Testcontainersを使用してシードデータの整合性を検証
// 実際のマイグレーションスキーマに合わせて実装
//
// このテストは `test-support` フィーチャーが有効な場合のみコンパイルされます
// 実行方法: cargo test --features test-support --test seed_data_test
#![cfg(feature = "test-support")]

use sales_management_db::test_support::with_test_pool;

#[tokio::test]
async fn test_seed_data_departments() {
    with_test_pool(|pool| async move {
        // シードデータを投入
        insert_seed_data(&pool).await;

        // 部門数の確認
        let count: i64 = sqlx::query_scalar(r#"SELECT COUNT(*) FROM "部門マスタ""#)
            .fetch_one(&pool)
            .await
            .expect("Failed to count departments");

        assert_eq!(count, 21, "部門マスタは21件であるべき");

        // 階層レベルの確認
        let level1_count: i64 =
            sqlx::query_scalar(r#"SELECT COUNT(*) FROM "部門マスタ" WHERE "組織階層" = 1"#)
                .fetch_one(&pool)
                .await
                .expect("Failed to count level 1 departments");

        assert_eq!(level1_count, 1, "レベル1（本社）は1件であるべき");

        // パスが正しく設定されているか確認
        let root_path: String =
            sqlx::query_scalar(r#"SELECT "部門パス" FROM "部門マスタ" WHERE "部門コード" = '000000'"#)
                .fetch_one(&pool)
                .await
                .expect("Failed to get root path");

        assert_eq!(root_path, "/000000", "本社のパスは /000000 であるべき");
    })
    .await;
}

#[tokio::test]
async fn test_seed_data_employees() {
    with_test_pool(|pool| async move {
        insert_seed_data(&pool).await;

        // 社員数の確認
        let count: i64 = sqlx::query_scalar(r#"SELECT COUNT(*) FROM "社員マスタ""#)
            .fetch_one(&pool)
            .await
            .expect("Failed to count employees");

        assert_eq!(count, 45, "社員マスタは45件であるべき");

        // 経営層の確認（職種コード="01"）
        let management_count: i64 =
            sqlx::query_scalar(r#"SELECT COUNT(*) FROM "社員マスタ" WHERE "職種コード" = '01'"#)
                .fetch_one(&pool)
                .await
                .expect("Failed to count management");

        assert_eq!(management_count, 2, "経営層は2名であるべき");
    })
    .await;
}

#[tokio::test]
async fn test_seed_data_companies() {
    with_test_pool(|pool| async move {
        insert_seed_data(&pool).await;

        // 取引先グループ数の確認
        let group_count: i64 = sqlx::query_scalar(r#"SELECT COUNT(*) FROM "取引先グループマスタ""#)
            .fetch_one(&pool)
            .await
            .expect("Failed to count company groups");

        assert_eq!(group_count, 7, "取引先グループは7件であるべき");

        // 取引先数の確認
        let company_count: i64 = sqlx::query_scalar(r#"SELECT COUNT(*) FROM "取引先マスタ""#)
            .fetch_one(&pool)
            .await
            .expect("Failed to count companies");

        assert_eq!(company_count, 14, "取引先は14件であるべき");

        // 顧客数の確認
        let customer_count: i64 = sqlx::query_scalar(r#"SELECT COUNT(*) FROM "顧客マスタ""#)
            .fetch_one(&pool)
            .await
            .expect("Failed to count customers");

        assert_eq!(customer_count, 10, "顧客は10件であるべき");

        // 仕入先数の確認
        let supplier_count: i64 = sqlx::query_scalar(r#"SELECT COUNT(*) FROM "仕入先マスタ""#)
            .fetch_one(&pool)
            .await
            .expect("Failed to count suppliers");

        assert_eq!(supplier_count, 4, "仕入先は4件であるべき");
    })
    .await;
}

#[tokio::test]
async fn test_seed_data_products() {
    with_test_pool(|pool| async move {
        insert_seed_data(&pool).await;

        // 商品分類数の確認
        let category_count: i64 = sqlx::query_scalar(r#"SELECT COUNT(*) FROM "商品分類マスタ""#)
            .fetch_one(&pool)
            .await
            .expect("Failed to count product categories");

        assert_eq!(category_count, 4, "商品分類は4件であるべき");

        // 商品数の確認
        let product_count: i64 = sqlx::query_scalar(r#"SELECT COUNT(*) FROM "商品マスタ""#)
            .fetch_one(&pool)
            .await
            .expect("Failed to count products");

        assert_eq!(product_count, 20, "商品マスタは20件であるべき");

        // 商品分類ごとの件数確認
        let beef_count: i64 = sqlx::query_scalar(
            r#"SELECT COUNT(*) FROM "商品マスタ" WHERE "商品分類コード" = 'CAT001'"#,
        )
        .fetch_one(&pool)
        .await
        .expect("Failed to count beef products");

        assert_eq!(beef_count, 5, "牛肉製品は5件であるべき");

        let pork_count: i64 = sqlx::query_scalar(
            r#"SELECT COUNT(*) FROM "商品マスタ" WHERE "商品分類コード" = 'CAT002'"#,
        )
        .fetch_one(&pool)
        .await
        .expect("Failed to count pork products");

        assert_eq!(pork_count, 5, "豚肉製品は5件であるべき");

        let chicken_count: i64 = sqlx::query_scalar(
            r#"SELECT COUNT(*) FROM "商品マスタ" WHERE "商品分類コード" = 'CAT003'"#,
        )
        .fetch_one(&pool)
        .await
        .expect("Failed to count chicken products");

        assert_eq!(chicken_count, 5, "鶏肉製品は5件であるべき");

        let processed_count: i64 = sqlx::query_scalar(
            r#"SELECT COUNT(*) FROM "商品マスタ" WHERE "商品分類コード" = 'CAT004'"#,
        )
        .fetch_one(&pool)
        .await
        .expect("Failed to count processed products");

        assert_eq!(processed_count, 5, "加工品は5件であるべき");
    })
    .await;
}

#[tokio::test]
async fn test_seed_data_warehouses() {
    with_test_pool(|pool| async move {
        insert_seed_data(&pool).await;

        // 倉庫数の確認
        let warehouse_count: i64 = sqlx::query_scalar(r#"SELECT COUNT(*) FROM "倉庫マスタ""#)
            .fetch_one(&pool)
            .await
            .expect("Failed to count warehouses");

        assert_eq!(warehouse_count, 2, "倉庫は2件であるべき");
    })
    .await;
}

#[tokio::test]
async fn test_seed_data_foreign_keys() {
    with_test_pool(|pool| async move {
        insert_seed_data(&pool).await;

        // 社員マスタの部門参照が正しいことを確認
        // 注：社員マスタは部門コードのみを参照し、開始日は独自に持つため、
        // 部門コードが部門マスタに存在することを確認
        let invalid_dept_refs = sqlx::query(
            r#"SELECT e."社員コード", e."部門コード"
               FROM "社員マスタ" e
               WHERE NOT EXISTS (
                   SELECT 1 FROM "部門マスタ" d
                   WHERE e."部門コード" = d."部門コード"
               )"#,
        )
        .fetch_all(&pool)
        .await
        .expect("Failed to check employee department references");

        assert_eq!(invalid_dept_refs.len(), 0, "すべての社員が存在する部門コードを参照しているべき");

        // 商品マスタの仕入先参照が正しいことを確認
        let invalid_supplier_refs = sqlx::query(
            r#"SELECT p."商品コード", p."仕入先コード"
               FROM "商品マスタ" p
               LEFT JOIN "仕入先マスタ" s
                 ON p."仕入先コード" = s."仕入先コード"
                AND p."仕入先枝番" = s."仕入先枝番"
               WHERE s."仕入先コード" IS NULL"#,
        )
        .fetch_all(&pool)
        .await
        .expect("Failed to check product supplier references");

        assert_eq!(invalid_supplier_refs.len(), 0, "すべての商品が有効な仕入先を参照しているべき");

        // 取引先マスタのグループ参照が正しいことを確認
        let invalid_group_refs = sqlx::query(
            r#"SELECT c."取引先コード", c."取引先グループコード"
               FROM "取引先マスタ" c
               LEFT JOIN "取引先グループマスタ" g
                 ON c."取引先グループコード" = g."取引先グループコード"
               WHERE g."取引先グループコード" IS NULL"#,
        )
        .fetch_all(&pool)
        .await
        .expect("Failed to check company group references");

        assert_eq!(invalid_group_refs.len(), 0, "すべての取引先が有効なグループを参照しているべき");
    })
    .await;
}

#[tokio::test]
async fn test_seed_data_business_rules() {
    with_test_pool(|pool| async move {
        insert_seed_data(&pool).await;

        // 販売単価が売上原価以上であることを確認
        let invalid_pricing: i64 = sqlx::query_scalar(
            r#"SELECT COUNT(*) FROM "商品マスタ"
               WHERE "販売単価" < "売上原価""#,
        )
        .fetch_one(&pool)
        .await
        .expect("Failed to check pricing");

        assert_eq!(invalid_pricing, 0, "販売単価は売上原価以上であるべき");

        // すべての商品に仕入先が設定されていることを確認
        let products_without_supplier: i64 = sqlx::query_scalar(
            r#"SELECT COUNT(*) FROM "商品マスタ"
               WHERE "仕入先コード" IS NULL"#,
        )
        .fetch_one(&pool)
        .await
        .expect("Failed to check products without supplier");

        assert_eq!(products_without_supplier, 0, "すべての商品に仕入先が設定されているべき");

        // 顧客の締日と支払日が有効範囲であることを確認
        let invalid_payment_terms: i64 = sqlx::query_scalar(
            r#"SELECT COUNT(*) FROM "顧客マスタ"
               WHERE "顧客締日１" < 1 OR "顧客締日１" > 31
                  OR "顧客支払日１" < 1 OR "顧客支払日１" > 31"#,
        )
        .fetch_one(&pool)
        .await
        .expect("Failed to check payment terms");

        assert_eq!(invalid_payment_terms, 0, "締日と支払日は1-31の範囲であるべき");
    })
    .await;
}

// ヘルパー関数: シードデータを投入
async fn insert_seed_data(pool: &sqlx::PgPool) {
    use chrono::NaiveDate;

    let start_date = NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap();
    let now = chrono::Utc::now().naive_utc();

    // 部門マスタの投入（21件）
    insert_departments(pool, start_date, now).await;

    // 取引先グループマスタの投入（7件）
    insert_company_groups(pool, now).await;

    // 取引先マスタの投入（14件）
    insert_companies(pool, now).await;

    // 仕入先マスタの投入（4件）
    insert_suppliers(pool, now).await;

    // 社員マスタの投入（45件）
    insert_employees(pool, start_date, now).await;

    // 顧客マスタの投入（10件）
    insert_customers(pool, now).await;

    // 商品分類マスタの投入（4件）
    insert_product_categories(pool, now).await;

    // 商品マスタの投入（20件）
    insert_products(pool, now).await;

    // 倉庫マスタの投入（2件）
    insert_warehouses(pool, now).await;
}

async fn insert_departments(
    pool: &sqlx::PgPool,
    start_date: chrono::NaiveDateTime,
    now: chrono::NaiveDateTime,
) {
    let departments = vec![
        ("000000", "本社", "/000000", 1, 0, 1),
        ("100000", "食肉製造・販売事業", "/000000/100000", 2, 0, 1),
        ("110000", "食肉加工部門", "/000000/100000/110000", 3, 0, 1),
        ("111000", "牛肉・豚肉・鶏肉課", "/000000/100000/110000/111000", 4, 1, 1),
        ("112000", "食肉加工品課", "/000000/100000/110000/112000", 4, 1, 1),
        ("120000", "小売販売部門", "/000000/100000/120000", 3, 0, 1),
        ("121000", "直営小売店課", "/000000/100000/120000/121000", 4, 1, 1),
        ("122000", "百貨店・スーパー向け販売課", "/000000/100000/120000/122000", 4, 1, 1),
        ("130000", "新規取引先開拓部門", "/000000/100000/130000", 3, 0, 1),
        ("131000", "ホテル・旅館向け課", "/000000/100000/130000/131000", 4, 1, 1),
        ("132000", "飲食店向け課", "/000000/100000/130000/132000", 4, 1, 1),
        ("200000", "食肉加工品事業", "/000000/200000", 2, 0, 1),
        ("210000", "自社ブランド部門", "/000000/200000/210000", 3, 0, 1),
        ("211000", "贈答用製品製造課", "/000000/200000/210000/211000", 4, 1, 1),
        ("212000", "道の駅・土産物製品販売課", "/000000/200000/210000/212000", 4, 1, 1),
        ("220000", "相手先ブランド製造(OEM)部門", "/000000/200000/220000", 3, 0, 1),
        ("221000", "客先要望対応課", "/000000/200000/220000/221000", 4, 1, 1),
        ("300000", "コンサルティング事業", "/000000/300000", 2, 0, 1),
        ("310000", "顧客対応部門", "/000000/300000/310000", 3, 0, 1),
        ("311000", "メニュー提案課", "/000000/300000/310000/311000", 4, 1, 1),
        ("312000", "半加工商品提供課", "/000000/300000/310000/312000", 4, 1, 1),
    ];

    for (code, name, path, level, bottom, slip_entry) in departments {
        sqlx::query(
            r#"INSERT INTO "部門マスタ" (
                "部門コード", "開始日", "部門名", "組織階層", "部門パス",
                "最下層区分", "伝票入力可否", "作成日時", "更新日時"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)"#
        )
        .bind(code)
        .bind(start_date)
        .bind(name)
        .bind(level)
        .bind(path)
        .bind(bottom)
        .bind(slip_entry)
        .bind(now)
        .bind(now)
        .execute(pool)
        .await
        .unwrap();
    }
}

async fn insert_company_groups(pool: &sqlx::PgPool, now: chrono::NaiveDateTime) {
    let groups = vec![
        ("GRP1", "百貨店グループ"),
        ("GRP2", "スーパーグループ"),
        ("GRP3", "ホテル・旅館グループ"),
        ("GRP4", "飲食店グループ"),
        ("GRP5", "観光施設グループ"),
        ("GRP6", "食肉卸グループ"),
        ("GRP7", "畜産業者グループ"),
    ];

    for (code, name) in groups {
        sqlx::query(
            r#"INSERT INTO "取引先グループマスタ" (
                "取引先グループコード", "取引先グループ名", "作成日時", "更新日時"
            ) VALUES ($1, $2, $3, $4)"#
        )
        .bind(code)
        .bind(name)
        .bind(now)
        .bind(now)
        .execute(pool)
        .await
        .unwrap();
    }
}

async fn insert_companies(pool: &sqlx::PgPool, now: chrono::NaiveDateTime) {
    let companies = vec![
        ("COMP0001", "GRP1", "X県有名百貨店"),
        ("COMP0002", "GRP1", "地域百貨店"),
        ("COMP0003", "GRP2", "地域スーパーチェーン"),
        ("COMP0004", "GRP2", "広域スーパーチェーン"),
        ("COMP0005", "GRP3", "シティホテルX"),
        ("COMP0006", "GRP3", "温泉旅館Y"),
        ("COMP0007", "GRP4", "焼肉レストランZ"),
        ("COMP0008", "GRP4", "イタリアンレストランA"),
        ("COMP0009", "GRP5", "道の駅B"),
        ("COMP0010", "GRP5", "観光センターC"),
        ("COMP0011", "GRP6", "地域食肉卸A社"),
        ("COMP0012", "GRP6", "地域食肉卸B社"),
        ("COMP0013", "GRP7", "地域畜産農家"),
        ("COMP0014", "GRP7", "県内畜産組合"),
    ];

    for (code, group_code, name) in companies {
        sqlx::query(
            r#"INSERT INTO "取引先マスタ" (
                "取引先コード", "取引先グループコード", "取引先名", "作成日時", "更新日時"
            ) VALUES ($1, $2, $3, $4, $5)"#
        )
        .bind(code)
        .bind(group_code)
        .bind(name)
        .bind(now)
        .bind(now)
        .execute(pool)
        .await
        .unwrap();
    }
}

async fn insert_suppliers(pool: &sqlx::PgPool, now: chrono::NaiveDateTime) {
    let suppliers = vec![
        ("COMP0011", "地域食肉卸A社", "チイキショクニクオロシエイシャ"),
        ("COMP0012", "地域食肉卸B社", "チイキショクニクオロシビイシャ"),
        ("COMP0013", "地域畜産農家", "チイキチクサンノウカ"),
        ("COMP0014", "県内畜産組合", "ケンナイチクサンクミアイ"),
    ];

    for (comp_code, name, kana) in suppliers {
        sqlx::query(
            r#"INSERT INTO "仕入先マスタ" (
                "仕入先コード", "仕入先枝番", "仕入先名", "仕入先名カナ",
                "仕入先締日", "仕入先支払月", "仕入先支払日",
                "作成日時", "更新日時"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)"#
        )
        .bind(comp_code)
        .bind(1)
        .bind(name)
        .bind(kana)
        .bind(31)
        .bind(1)
        .bind(31)
        .bind(now)
        .bind(now)
        .execute(pool)
        .await
        .unwrap();
    }
}

async fn insert_employees(
    pool: &sqlx::PgPool,
    start_date: chrono::NaiveDateTime,
    now: chrono::NaiveDateTime,
) {
    let employees = vec![
        (
            "EMP00001",
            "山田太郎",
            "ヤマダタロウ",
            "pass0001",
            "03-1234-5678",
            "03-1234-5679",
            "000000",
            "01",
            "01",
        ),
        (
            "EMP00002",
            "佐藤花子",
            "サトウハナコ",
            "pass0002",
            "03-1234-5680",
            "03-1234-5681",
            "000000",
            "01",
            "02",
        ),
        (
            "EMP00003",
            "鈴木一郎",
            "スズキイチロウ",
            "pass0003",
            "03-1234-5682",
            "03-1234-5683",
            "110000",
            "02",
            "03",
        ),
        (
            "EMP00004",
            "田中美咲",
            "タナカミサキ",
            "pass0004",
            "03-1234-5684",
            "03-1234-5685",
            "111000",
            "02",
            "04",
        ),
        (
            "EMP00005",
            "伊藤健太",
            "イトウケンタ",
            "pass0005",
            "03-1234-5686",
            "03-1234-5687",
            "111000",
            "02",
            "04",
        ),
        (
            "EMP00006",
            "渡辺由美",
            "ワタナベユミ",
            "pass0006",
            "03-1234-5688",
            "03-1234-5689",
            "112000",
            "02",
            "04",
        ),
        (
            "EMP00007",
            "中村誠",
            "ナカムラマコト",
            "pass0007",
            "03-1234-5690",
            "03-1234-5691",
            "112000",
            "02",
            "04",
        ),
        (
            "EMP00008",
            "小林愛",
            "コバヤシアイ",
            "pass0008",
            "03-1234-5692",
            "03-1234-5693",
            "120000",
            "02",
            "03",
        ),
        (
            "EMP00009",
            "加藤大輔",
            "カトウダイスケ",
            "pass0009",
            "03-1234-5694",
            "03-1234-5695",
            "121000",
            "02",
            "04",
        ),
        (
            "EMP00010",
            "吉田麻衣",
            "ヨシダマイ",
            "pass0010",
            "03-1234-5696",
            "03-1234-5697",
            "122000",
            "02",
            "04",
        ),
        (
            "EMP00011",
            "山本太一",
            "ヤマモトタイチ",
            "pass0011",
            "03-1234-5698",
            "03-1234-5699",
            "111000",
            "02",
            "05",
        ),
        (
            "EMP00012",
            "松本優子",
            "マツモトユウコ",
            "pass0012",
            "03-1234-5700",
            "03-1234-5701",
            "111000",
            "02",
            "05",
        ),
        (
            "EMP00013",
            "井上健一",
            "イノウエケンイチ",
            "pass0013",
            "03-1234-5702",
            "03-1234-5703",
            "112000",
            "02",
            "05",
        ),
        (
            "EMP00014",
            "木村咲",
            "キムラサキ",
            "pass0014",
            "03-1234-5704",
            "03-1234-5705",
            "121000",
            "02",
            "05",
        ),
        (
            "EMP00015",
            "林隆",
            "ハヤシタカシ",
            "pass0015",
            "03-1234-5706",
            "03-1234-5707",
            "121000",
            "02",
            "05",
        ),
        (
            "EMP00016",
            "斎藤真理",
            "サイトウマリ",
            "pass0016",
            "03-1234-5708",
            "03-1234-5709",
            "122000",
            "02",
            "05",
        ),
        (
            "EMP00017",
            "清水翔太",
            "シミズショウタ",
            "pass0017",
            "03-1234-5710",
            "03-1234-5711",
            "130000",
            "02",
            "05",
        ),
        (
            "EMP00018",
            "山口和也",
            "ヤマグチカズヤ",
            "pass0018",
            "03-1234-5712",
            "03-1234-5713",
            "200000",
            "02",
            "03",
        ),
        (
            "EMP00019",
            "森本あかり",
            "モリモトアカリ",
            "pass0019",
            "03-1234-5714",
            "03-1234-5715",
            "210000",
            "02",
            "04",
        ),
        (
            "EMP00020",
            "阿部修平",
            "アベシュウヘイ",
            "pass0020",
            "03-1234-5716",
            "03-1234-5717",
            "211000",
            "02",
            "04",
        ),
        (
            "EMP00021",
            "池田菜々子",
            "イケダナナコ",
            "pass0021",
            "03-1234-5718",
            "03-1234-5719",
            "212000",
            "02",
            "04",
        ),
        (
            "EMP00022",
            "橋本拓也",
            "ハシモトタクヤ",
            "pass0022",
            "03-1234-5720",
            "03-1234-5721",
            "220000",
            "02",
            "04",
        ),
        (
            "EMP00023",
            "石川舞",
            "イシカワマイ",
            "pass0023",
            "03-1234-5722",
            "03-1234-5723",
            "221000",
            "02",
            "04",
        ),
        (
            "EMP00024",
            "前田勇気",
            "マエダユウキ",
            "pass0024",
            "03-1234-5724",
            "03-1234-5725",
            "210000",
            "02",
            "05",
        ),
        (
            "EMP00025",
            "藤田彩香",
            "フジタアヤカ",
            "pass0025",
            "03-1234-5726",
            "03-1234-5727",
            "211000",
            "02",
            "05",
        ),
        (
            "EMP00026",
            "岡田拓海",
            "オカダタクミ",
            "pass0026",
            "03-1234-5728",
            "03-1234-5729",
            "211000",
            "02",
            "05",
        ),
        (
            "EMP00027",
            "後藤美穂",
            "ゴトウミホ",
            "pass0027",
            "03-1234-5730",
            "03-1234-5731",
            "212000",
            "02",
            "05",
        ),
        (
            "EMP00028",
            "長谷川浩",
            "ハセガワヒロシ",
            "pass0028",
            "03-1234-5732",
            "03-1234-5733",
            "212000",
            "02",
            "05",
        ),
        (
            "EMP00029",
            "村上沙織",
            "ムラカミサオリ",
            "pass0029",
            "03-1234-5734",
            "03-1234-5735",
            "220000",
            "02",
            "05",
        ),
        (
            "EMP00030",
            "近藤剛",
            "コンドウツヨシ",
            "pass0030",
            "03-1234-5736",
            "03-1234-5737",
            "221000",
            "02",
            "05",
        ),
        (
            "EMP00031",
            "坂本恵美",
            "サカモトエミ",
            "pass0031",
            "03-1234-5738",
            "03-1234-5739",
            "221000",
            "02",
            "05",
        ),
        (
            "EMP00032",
            "遠藤隆志",
            "エンドウタカシ",
            "pass0032",
            "03-1234-5740",
            "03-1234-5741",
            "300000",
            "02",
            "03",
        ),
        (
            "EMP00033",
            "青木千尋",
            "アオキチヒロ",
            "pass0033",
            "03-1234-5742",
            "03-1234-5743",
            "310000",
            "02",
            "04",
        ),
        (
            "EMP00034",
            "藤井大輝",
            "フジイダイキ",
            "pass0034",
            "03-1234-5744",
            "03-1234-5745",
            "311000",
            "02",
            "04",
        ),
        (
            "EMP00035",
            "西村結衣",
            "ニシムラユイ",
            "pass0035",
            "03-1234-5746",
            "03-1234-5747",
            "311000",
            "02",
            "04",
        ),
        (
            "EMP00036",
            "福田翔",
            "フクダショウ",
            "pass0036",
            "03-1234-5748",
            "03-1234-5749",
            "312000",
            "02",
            "04",
        ),
        (
            "EMP00037",
            "太田桜",
            "オオタサクラ",
            "pass0037",
            "03-1234-5750",
            "03-1234-5751",
            "312000",
            "02",
            "04",
        ),
        (
            "EMP00038",
            "三浦健",
            "ミウラケン",
            "pass0038",
            "03-1234-5752",
            "03-1234-5753",
            "310000",
            "02",
            "05",
        ),
        (
            "EMP00039",
            "岩崎美智子",
            "イワサキミチコ",
            "pass0039",
            "03-1234-5754",
            "03-1234-5755",
            "311000",
            "02",
            "05",
        ),
        (
            "EMP00040",
            "原田誠一",
            "ハラダセイイチ",
            "pass0040",
            "03-1234-5756",
            "03-1234-5757",
            "311000",
            "02",
            "05",
        ),
        (
            "EMP00041",
            "竹内美咲",
            "タケウチミサキ",
            "pass0041",
            "03-1234-5758",
            "03-1234-5759",
            "312000",
            "02",
            "05",
        ),
        (
            "EMP00042",
            "増田浩二",
            "マスダコウジ",
            "pass0042",
            "03-1234-5760",
            "03-1234-5761",
            "312000",
            "02",
            "05",
        ),
        (
            "EMP00043",
            "小川あゆみ",
            "オガワアユミ",
            "pass0043",
            "03-1234-5762",
            "03-1234-5763",
            "312000",
            "02",
            "05",
        ),
        (
            "EMP00044",
            "高橋真一",
            "タカハシシンイチ",
            "pass0044",
            "03-1234-5764",
            "03-1234-5765",
            "000000",
            "03",
            "03",
        ),
        (
            "EMP00045",
            "中島優香",
            "ナカジマユカ",
            "pass0045",
            "03-1234-5766",
            "03-1234-5767",
            "000000",
            "03",
            "04",
        ),
    ];

    for (code, name, kana, password, tel, fax, dept, job, auth) in employees {
        sqlx::query(
            r#"INSERT INTO "社員マスタ" (
                "社員コード", "社員名", "社員名カナ", "パスワード",
                "電話番号", "FAX番号", "部門コード", "開始日",
                "職種コード", "承認権限コード", "作成日時", "更新日時"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)"#
        )
        .bind(code)
        .bind(name)
        .bind(kana)
        .bind(password)
        .bind(tel)
        .bind(fax)
        .bind(dept)
        .bind(start_date)
        .bind(job)
        .bind(auth)
        .bind(now)
        .bind(now)
        .execute(pool)
        .await
        .unwrap();
    }
}

async fn insert_customers(pool: &sqlx::PgPool, now: chrono::NaiveDateTime) {
    for i in 1..=10 {
        let comp_code = format!("COMP{:04}", i);
        let name = format!("顧客{}", i);
        let kana = format!("コキャク{}", i);

        sqlx::query(
            r#"INSERT INTO "顧客マスタ" (
                "顧客コード", "顧客枝番", "顧客区分",
                "請求先コード", "請求先枝番",
                "回収先コード", "回収先枝番",
                "顧客名", "顧客名カナ",
                "自社担当者コード",
                "顧客締日１", "顧客支払月１", "顧客支払日１",
                "作成日時", "更新日時"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)"#
        )
        .bind(&comp_code)
        .bind(1)
        .bind(0)
        .bind(&comp_code)
        .bind(1)
        .bind(&comp_code)
        .bind(1)
        .bind(&name)
        .bind(&kana)
        .bind("EMP00001")
        .bind(31)
        .bind(1)
        .bind(31)
        .bind(now)
        .bind(now)
        .execute(pool)
        .await
        .unwrap();
    }
}

async fn insert_product_categories(pool: &sqlx::PgPool, now: chrono::NaiveDateTime) {
    let categories = vec![
        ("CAT001", "牛肉", "/CAT001", 1, 1),
        ("CAT002", "豚肉", "/CAT002", 1, 1),
        ("CAT003", "鶏肉", "/CAT003", 1, 1),
        ("CAT004", "加工品", "/CAT004", 1, 1),
    ];

    for (code, name, path, level, bottom) in categories {
        sqlx::query(
            r#"INSERT INTO "商品分類マスタ" (
                "商品分類コード", "商品分類名", "商品分類階層",
                "商品分類パス", "最下層区分", "作成日時", "更新日時"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7)"#
        )
        .bind(code)
        .bind(name)
        .bind(level)
        .bind(path)
        .bind(bottom)
        .bind(now)
        .bind(now)
        .execute(pool)
        .await
        .unwrap();
    }
}

async fn insert_products(pool: &sqlx::PgPool, now: chrono::NaiveDateTime) {
    let products = vec![
        (
            "PROD00001",
            "CAT001",
            "黒毛和牛サーロインステーキ 200g",
            "サーロイン",
            "クロゲワギュウサーロイン",
            5000,
            3700,
            3500,
            "COMP0011",
            1,
        ),
        (
            "PROD00002",
            "CAT001",
            "黒毛和牛ロース 200g",
            "ロース",
            "クロゲワギュウロース",
            4500,
            3350,
            3200,
            "COMP0011",
            1,
        ),
        (
            "PROD00003",
            "CAT001",
            "黒毛和牛カルビ 200g",
            "カルビ",
            "クロゲワギュウカルビ",
            4000,
            2950,
            2800,
            "COMP0011",
            1,
        ),
        (
            "PROD00004",
            "CAT001",
            "黒毛和牛ヒレ 150g",
            "ヒレ",
            "クロゲワギュウヒレ",
            6000,
            4400,
            4200,
            "COMP0011",
            1,
        ),
        (
            "PROD00005",
            "CAT001",
            "国産牛切り落とし 300g",
            "切り落とし",
            "コクサンギュウキリオトシ",
            1500,
            1050,
            1000,
            "COMP0012",
            1,
        ),
        (
            "PROD00006",
            "CAT002",
            "豚ロース 200g",
            "豚ロース",
            "ブタロース",
            800,
            580,
            550,
            "COMP0012",
            1,
        ),
        ("PROD00007", "CAT002", "豚バラ 200g", "豚バラ", "ブタバラ", 700, 505, 480, "COMP0012", 1),
        ("PROD00008", "CAT002", "豚ヒレ 150g", "豚ヒレ", "ブタヒレ", 900, 660, 630, "COMP0012", 1),
        ("PROD00009", "CAT002", "豚コマ 300g", "豚コマ", "ブタコマ", 600, 420, 400, "COMP0012", 1),
        (
            "PROD00010",
            "CAT002",
            "豚肩ロース 200g",
            "豚肩ロース",
            "ブタカタロース",
            750,
            545,
            520,
            "COMP0012",
            1,
        ),
        (
            "PROD00011",
            "CAT003",
            "鶏もも肉 200g",
            "鶏もも",
            "トリモモ",
            500,
            357,
            340,
            "COMP0013",
            1,
        ),
        (
            "PROD00012",
            "CAT003",
            "鶏むね肉 200g",
            "鶏むね",
            "トリムネ",
            400,
            283,
            270,
            "COMP0013",
            1,
        ),
        ("PROD00013", "CAT003", "手羽先 200g", "手羽先", "テバサキ", 450, 325, 310, "COMP0013", 1),
        ("PROD00014", "CAT003", "手羽元 200g", "手羽元", "テバモト", 430, 315, 300, "COMP0013", 1),
        (
            "PROD00015",
            "CAT003",
            "鶏ささみ 150g",
            "鶏ささみ",
            "トリササミ",
            550,
            399,
            380,
            "COMP0013",
            1,
        ),
        (
            "PROD00016",
            "CAT004",
            "ローストビーフ 100g",
            "ローストビーフ",
            "ローストビーフ",
            1200,
            840,
            800,
            "COMP0014",
            1,
        ),
        ("PROD00017", "CAT004", "ハム 100g", "ハム", "ハム", 600, 420, 400, "COMP0014", 1),
        (
            "PROD00018",
            "CAT004",
            "ソーセージ 5本入",
            "ソーセージ",
            "ソーセージ",
            500,
            357,
            340,
            "COMP0014",
            1,
        ),
        (
            "PROD00019",
            "CAT004",
            "ベーコン 100g",
            "ベーコン",
            "ベーコン",
            550,
            388,
            370,
            "COMP0014",
            1,
        ),
        (
            "PROD00020",
            "CAT004",
            "コロッケ 4個入",
            "コロッケ",
            "コロッケ",
            400,
            262,
            250,
            "COMP0014",
            1,
        ),
    ];

    for (code, cat_code, fullname, abbr, kana, price, purchase, cost, sup_code, sup_seq) in products
    {
        sqlx::query(
            r#"INSERT INTO "商品マスタ" (
                "商品コード", "商品分類コード", "商品正式名", "商品略称", "商品名カナ",
                "販売単価", "仕入単価", "売上原価", "税区分",
                "在庫管理対象区分", "仕入先コード", "仕入先枝番",
                "作成日時", "更新日時"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)"#
        )
        .bind(code)
        .bind(cat_code)
        .bind(fullname)
        .bind(abbr)
        .bind(kana)
        .bind(price)
        .bind(purchase)
        .bind(cost)
        .bind(1)
        .bind(1)
        .bind(sup_code)
        .bind(sup_seq)
        .bind(now)
        .bind(now)
        .execute(pool)
        .await
        .unwrap();
    }
}

async fn insert_warehouses(pool: &sqlx::PgPool, now: chrono::NaiveDateTime) {
    let warehouses = vec![
        (
            "WH1",
            "本社倉庫",
            "本社",
            "100-0001",
            "東京都",
            "千代田区千代田",
            "1-1-1",
            "03-1234-5678",
            "03-1234-5679",
        ),
        (
            "WH2",
            "工場倉庫",
            "工場",
            "100-0002",
            "東京都",
            "千代田区丸の内",
            "2-2-2",
            "03-1234-5680",
            "03-1234-5681",
        ),
    ];

    for (code, name, abbr, zip, pref, addr1, addr2, tel, fax) in warehouses {
        sqlx::query(
            r#"INSERT INTO "倉庫マスタ" (
                "倉庫コード", "倉庫名", "倉庫名略称",
                "郵便番号", "都道府県", "住所１", "住所２",
                "電話番号", "ＦＡＸ番号", "作成日時", "更新日時"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)"#
        )
        .bind(code)
        .bind(name)
        .bind(abbr)
        .bind(zip)
        .bind(pref)
        .bind(addr1)
        .bind(addr2)
        .bind(tel)
        .bind(fax)
        .bind(now)
        .bind(now)
        .execute(pool)
        .await
        .unwrap();
    }
}
