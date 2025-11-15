package main

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"github.com/jmoiron/sqlx"
)

// seedCompanyGroups 取引先グループマスタのシードデータを投入
func seedCompanyGroups(ctx context.Context, tx *sqlx.Tx) (int, error) {
	now := time.Now()
	query := `
		INSERT INTO "取引先グループマスタ" (
			"取引先グループコード", "取引先グループ名",
			"作成日時", "作成者名", "更新日時", "更新者名"
		) VALUES ($1, $2, $3, $4, $5, $6)
	`

	groups := []struct {
		コード string
		名称  string
	}{
		{"G001", "百貨店グループ"},
		{"G002", "スーパーグループ"},
		{"G003", "ホテル・旅館グループ"},
		{"G004", "飲食店グループ"},
		{"G005", "観光施設グループ"},
		{"G006", "食肉卸グループ"},
		{"G007", "畜産業者グループ"},
	}

	count := 0
	for _, grp := range groups {
		_, err := tx.ExecContext(ctx, query, grp.コード, grp.名称, now, "seed", now, "seed")
		if err != nil {
			return 0, fmt.Errorf("failed to insert company group %s: %w", grp.コード, err)
		}
		count++
	}

	return count, nil
}

// seedCompanies 取引先マスタのシードデータを投入
func seedCompanies(ctx context.Context, tx *sqlx.Tx) (int, error) {
	now := time.Now()
	query := `
		INSERT INTO "取引先マスタ" (
			"取引先コード", "取引先名", "取引先名カナ", "取引先グループコード",
			"作成日時", "作成者名", "更新日時", "更新者名"
		) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
	`

	companies := []struct {
		コード  string
		名称   string
		カナ   *string
		グループ string
	}{
		// 得意先
		{"COMP0001", "地域百貨店", strPtr("チイキヒャッカテン"), "G001"},
		{"COMP0002", "X県有名百貨店", strPtr("ケンユウメイヒャッカテン"), "G001"},
		{"COMP0003", "地域スーパーチェーン", strPtr("チイキスーパー"), "G002"},
		{"COMP0004", "広域スーパーチェーン", strPtr("コウイキスーパー"), "G002"},
		{"COMP0005", "シティホテル", strPtr("シティホテル"), "G003"},
		{"COMP0006", "温泉旅館", strPtr("オンセンリョカン"), "G003"},
		{"COMP0007", "焼肉レストラン", strPtr("ヤキニクレストラン"), "G004"},
		{"COMP0008", "イタリアンレストラン", strPtr("イタリアンレストラン"), "G004"},
		{"COMP0009", "道の駅", strPtr("ミチノエキ"), "G005"},
		{"COMP0010", "観光センター", strPtr("カンコウセンター"), "G005"},

		// 仕入先
		{"COMP0011", "地域食肉卸A社", strPtr("チイキショクニクオロシA"), "G006"},
		{"COMP0012", "地域食肉卸B社", strPtr("チイキショクニクオロシB"), "G006"},
		{"COMP0013", "地域畜産農家", strPtr("チイキチクサンノウカ"), "G007"},
		{"COMP0014", "県内畜産組合", strPtr("ケンナイチクサンクミアイ"), "G007"},
	}

	count := 0
	for _, comp := range companies {
		var kana sql.NullString
		if comp.カナ != nil {
			kana = sql.NullString{String: *comp.カナ, Valid: true}
		}

		_, err := tx.ExecContext(ctx, query,
			comp.コード, comp.名称, kana, comp.グループ,
			now, "seed", now, "seed",
		)
		if err != nil {
			return 0, fmt.Errorf("failed to insert company %s: %w", comp.コード, err)
		}
		count++
	}

	return count, nil
}

// seedCustomers 顧客マスタのシードデータを投入
func seedCustomers(ctx context.Context, tx *sqlx.Tx) (int, error) {
	now := time.Now()
	query := `
		INSERT INTO "顧客マスタ" (
			"顧客コード", "顧客枝番", "請求先コード", "請求先枝番", "回収先コード", "回収先枝番",
			"顧客名", "作成日時", "作成者名", "更新日時", "更新者名"
		) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
	`

	// 得意先10社を顧客として登録
	customers := []string{
		"COMP0001", "COMP0002", "COMP0003", "COMP0004", "COMP0005",
		"COMP0006", "COMP0007", "COMP0008", "COMP0009", "COMP0010",
	}

	count := 0
	for _, custCode := range customers {
		// 顧客名を取得
		var custName string
		err := tx.QueryRowContext(ctx, `SELECT "取引先名" FROM "取引先マスタ" WHERE "取引先コード" = $1`, custCode).Scan(&custName)
		if err != nil {
			return 0, fmt.Errorf("failed to get company name for %s: %w", custCode, err)
		}

		// 請求先・回収先は自分自身とする
		_, err = tx.ExecContext(ctx, query,
			custCode, 0, custCode, 0, custCode, 0,
			custName, now, "seed", now, "seed")
		if err != nil {
			return 0, fmt.Errorf("failed to insert customer %s: %w", custCode, err)
		}
		count++
	}

	return count, nil
}

// seedSuppliers 仕入先マスタのシードデータを投入
func seedSuppliers(ctx context.Context, tx *sqlx.Tx) (int, error) {
	now := time.Now()
	query := `
		INSERT INTO "仕入先マスタ" (
			"仕入先コード", "仕入先枝番", "仕入先名",
			"作成日時", "作成者名", "更新日時", "更新者名"
		) VALUES ($1, $2, $3, $4, $5, $6, $7)
	`

	// 仕入先4社を登録
	suppliers := []string{"COMP0011", "COMP0012", "COMP0013", "COMP0014"}

	count := 0
	for _, suppCode := range suppliers {
		// 仕入先名を取得
		var suppName string
		err := tx.QueryRowContext(ctx, `SELECT "取引先名" FROM "取引先マスタ" WHERE "取引先コード" = $1`, suppCode).Scan(&suppName)
		if err != nil {
			return 0, fmt.Errorf("failed to get company name for %s: %w", suppCode, err)
		}

		_, err = tx.ExecContext(ctx, query, suppCode, 0, suppName, now, "seed", now, "seed")
		if err != nil {
			return 0, fmt.Errorf("failed to insert supplier %s: %w", suppCode, err)
		}
		count++
	}

	return count, nil
}

// seedProductCategories 商品分類マスタのシードデータを投入
func seedProductCategories(ctx context.Context, tx *sqlx.Tx) (int, error) {
	now := time.Now()
	query := `
		INSERT INTO "商品分類マスタ" (
			"商品分類コード", "商品分類名", "商品分類階層", "商品分類パス", "最下層区分",
			"作成日時", "作成者名", "更新日時", "更新者名"
		) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
	`

	categories := []struct {
		コード string
		名称  string
		階層  int
		パス  string
		最下層 int
	}{
		{"CAT001", "牛肉", 0, "/CAT001", 1},
		{"CAT002", "豚肉", 0, "/CAT002", 1},
		{"CAT003", "鶏肉", 0, "/CAT003", 1},
		{"CAT004", "加工品", 0, "/CAT004", 1},
		{"CAT005", "その他", 0, "/CAT005", 1},
	}

	count := 0
	for _, cat := range categories {
		_, err := tx.ExecContext(ctx, query,
			cat.コード, cat.名称, cat.階層, cat.パス, cat.最下層,
			now, "seed", now, "seed",
		)
		if err != nil {
			return 0, fmt.Errorf("failed to insert product category %s: %w", cat.コード, err)
		}
		count++
	}

	return count, nil
}

// seedProducts 商品マスタのシードデータを投入
func seedProducts(ctx context.Context, tx *sqlx.Tx) (int, error) {
	now := time.Now()
	query := `
		INSERT INTO "商品マスタ" (
			"商品コード", "商品正式名", "商品略称", "商品名カナ", "販売単価", "仕入単価",
			"商品分類コード", "作成日時", "作成者名", "更新日時", "更新者名"
		) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
	`

	products := []struct {
		コード   string
		正式名   string
		略称    string
		カナ    *string
		販売単価  int
		仕入単価  int
		分類コード *string
	}{
		// 牛肉製品
		{"PROD00001", "黒毛和牛サーロインステーキ 200g", "サーロイン", strPtr("クロゲワギュウサーロイン"), 5000, 3500, strPtr("CAT001")},
		{"PROD00002", "黒毛和牛ロース 100g", "ロース", strPtr("クロゲワギュウロース"), 3000, 2100, strPtr("CAT001")},
		{"PROD00003", "黒毛和牛カルビ 100g", "カルビ", strPtr("クロゲワギュウカルビ"), 2500, 1750, strPtr("CAT001")},
		{"PROD00004", "黒毛和牛ヒレ 100g", "ヒレ", strPtr("クロゲワギュウヒレ"), 4000, 2800, strPtr("CAT001")},
		{"PROD00005", "国産牛切り落とし 200g", "切り落とし", strPtr("コクサンギュウキリオトシ"), 1000, 700, strPtr("CAT001")},

		// 豚肉製品
		{"PROD00006", "国産豚ロース 100g", "豚ロース", strPtr("コクサンブタロース"), 500, 350, strPtr("CAT002")},
		{"PROD00007", "国産豚バラ 100g", "豚バラ", strPtr("コクサンブタバラ"), 400, 280, strPtr("CAT002")},
		{"PROD00008", "国産豚ヒレ 100g", "豚ヒレ", strPtr("コクサンブタヒレ"), 600, 420, strPtr("CAT002")},
		{"PROD00009", "国産豚コマ 200g", "豚コマ", strPtr("コクサンブタコマ"), 350, 245, strPtr("CAT002")},
		{"PROD00010", "国産豚肩ロース 100g", "豚肩ロース", strPtr("コクサンブタカタロース"), 450, 315, strPtr("CAT002")},

		// 鶏肉製品
		{"PROD00011", "国産鶏もも肉 100g", "鶏もも", strPtr("コクサントリモモ"), 250, 175, strPtr("CAT003")},
		{"PROD00012", "国産鶏むね肉 100g", "鶏むね", strPtr("コクサントリムネ"), 150, 105, strPtr("CAT003")},
		{"PROD00013", "国産手羽先 100g", "手羽先", strPtr("コクサンテバサキ"), 200, 140, strPtr("CAT003")},
		{"PROD00014", "国産手羽元 100g", "手羽元", strPtr("コクサンテバモト"), 180, 126, strPtr("CAT003")},
		{"PROD00015", "国産鶏ささみ 100g", "鶏ささみ", strPtr("コクサントリササミ"), 300, 210, strPtr("CAT003")},

		// 加工品
		{"PROD00016", "自家製ローストビーフ 100g", "ローストビーフ", strPtr("ローストビーフ"), 1500, 1050, strPtr("CAT004")},
		{"PROD00017", "自家製ハム 100g", "ハム", strPtr("ハム"), 800, 560, strPtr("CAT004")},
		{"PROD00018", "自家製ソーセージ 100g", "ソーセージ", strPtr("ソーセージ"), 700, 490, strPtr("CAT004")},
		{"PROD00019", "自家製ベーコン 100g", "ベーコン", strPtr("ベーコン"), 900, 630, strPtr("CAT004")},
		{"PROD00020", "揚げたてコロッケ 1個", "コロッケ", strPtr("コロッケ"), 100, 70, strPtr("CAT004")},
	}

	count := 0
	for _, prod := range products {
		var kana, cat sql.NullString
		if prod.カナ != nil {
			kana = sql.NullString{String: *prod.カナ, Valid: true}
		}
		if prod.分類コード != nil {
			cat = sql.NullString{String: *prod.分類コード, Valid: true}
		}

		_, err := tx.ExecContext(ctx, query,
			prod.コード, prod.正式名, prod.略称, kana, prod.販売単価, prod.仕入単価,
			cat, now, "seed", now, "seed",
		)
		if err != nil {
			return 0, fmt.Errorf("failed to insert product %s: %w", prod.コード, err)
		}
		count++
	}

	return count, nil
}

// seedWarehouses 倉庫マスタのシードデータを投入
func seedWarehouses(ctx context.Context, tx *sqlx.Tx) (int, error) {
	now := time.Now()
	query := `
		INSERT INTO "倉庫マスタ" (
			"倉庫コード", "倉庫名", "郵便番号", "住所１", "電話番号",
			"作成日時", "作成者名", "更新日時", "更新者名"
		) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
	`

	warehouses := []struct {
		コード  string
		名称   string
		郵便番号 *string
		住所   *string
		電話番号 *string
	}{
		{"001", "本社倉庫", strPtr("100-0001"), strPtr("東京都千代田区千代田1-1-1"), strPtr("03-1234-5678")},
		{"002", "工場倉庫", strPtr("200-0001"), strPtr("神奈川県横浜市中区山下町1-1-1"), strPtr("045-1234-5678")},
	}

	count := 0
	for _, wh := range warehouses {
		var zip, addr, tel sql.NullString
		if wh.郵便番号 != nil {
			zip = sql.NullString{String: *wh.郵便番号, Valid: true}
		}
		if wh.住所 != nil {
			addr = sql.NullString{String: *wh.住所, Valid: true}
		}
		if wh.電話番号 != nil {
			tel = sql.NullString{String: *wh.電話番号, Valid: true}
		}

		_, err := tx.ExecContext(ctx, query,
			wh.コード, wh.名称, zip, addr, tel,
			now, "seed", now, "seed",
		)
		if err != nil {
			return 0, fmt.Errorf("failed to insert warehouse %s: %w", wh.コード, err)
		}
		count++
	}

	return count, nil
}
