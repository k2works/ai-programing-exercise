-- ユーザーテーブル
COMMENT ON TABLE "users" IS 'ユーザー';
COMMENT ON COLUMN "users"."id" IS 'ユーザーID';
COMMENT ON COLUMN "users"."first_name" IS '名';
COMMENT ON COLUMN "users"."last_name" IS '姓';
COMMENT ON COLUMN "users"."password" IS 'パスワード';
COMMENT ON COLUMN "users"."role_name" IS 'ロール名';
COMMENT ON COLUMN "users"."user_type" IS 'ユーザー区分';
COMMENT ON COLUMN "users"."status" IS '登録区分';
COMMENT ON COLUMN "users"."created_by" IS '作成者';
COMMENT ON COLUMN "users"."created_at" IS '作成日時';
COMMENT ON COLUMN "users"."updated_at" IS '更新日時';

-- 商品テーブル
COMMENT ON TABLE "products" IS '商品';
COMMENT ON COLUMN "products"."product_number" IS '商品番号';
COMMENT ON COLUMN "products"."product_code" IS '商品コード';
COMMENT ON COLUMN "products"."product_name" IS '商品名称';
COMMENT ON COLUMN "products"."product_abbreviation" IS '商品略称';
COMMENT ON COLUMN "products"."product_category" IS '商品区分';
COMMENT ON COLUMN "products"."sales_price" IS '販売単価';
COMMENT ON COLUMN "products"."purchase_price" IS '仕入単価';
COMMENT ON COLUMN "products"."tax_category" IS '税区分';
COMMENT ON COLUMN "products"."sales_status" IS '販売状態';
COMMENT ON COLUMN "products"."created_by" IS '作成者';
COMMENT ON COLUMN "products"."created_at" IS '作成日時';
COMMENT ON COLUMN "products"."updated_at" IS '更新日時';

-- 商品構成テーブル
COMMENT ON TABLE "product_compositions" IS '商品構成';
COMMENT ON COLUMN "product_compositions"."product_number" IS '商品番号';
COMMENT ON COLUMN "product_compositions"."item_number" IS '単品番号';
COMMENT ON COLUMN "product_compositions"."required_quantity" IS '必要数量';

-- 得意先テーブル
COMMENT ON TABLE "customers" IS '得意先';
COMMENT ON COLUMN "customers"."customer_number" IS '得意先番号';
COMMENT ON COLUMN "customers"."customer_code" IS '得意先コード';
COMMENT ON COLUMN "customers"."customer_name" IS '得意先名称';
COMMENT ON COLUMN "customers"."contact_phone" IS '連絡先電話番号';
COMMENT ON COLUMN "customers"."contact_email" IS '連絡先メールアドレス';
COMMENT ON COLUMN "customers"."credit_card_info" IS 'クレジットカード情報';
COMMENT ON COLUMN "customers"."created_by" IS '作成者';
COMMENT ON COLUMN "customers"."created_at" IS '作成日時';
COMMENT ON COLUMN "customers"."updated_at" IS '更新日時';

-- 注文テーブル
COMMENT ON TABLE "orders" IS '注文';
COMMENT ON COLUMN "orders"."order_number" IS '注文番号';
COMMENT ON COLUMN "orders"."order_date" IS '注文日';
COMMENT ON COLUMN "orders"."customer_number" IS '得意先番号';
COMMENT ON COLUMN "orders"."product_number" IS '商品番号';
COMMENT ON COLUMN "orders"."quantity" IS '数量';
COMMENT ON COLUMN "orders"."desired_delivery_date" IS '希望配送日';
COMMENT ON COLUMN "orders"."delivery_address" IS '配送先住所';
COMMENT ON COLUMN "orders"."delivery_phone" IS '配送先電話番号';
COMMENT ON COLUMN "orders"."delivery_message" IS '配送メッセージ';
COMMENT ON COLUMN "orders"."status" IS '注文状態';
COMMENT ON COLUMN "orders"."created_by" IS '作成者';
COMMENT ON COLUMN "orders"."created_at" IS '作成日時';
COMMENT ON COLUMN "orders"."updated_at" IS '更新日時';

-- 受注テーブル
COMMENT ON TABLE "received_orders" IS '受注';
COMMENT ON COLUMN "received_orders"."received_order_number" IS '受注番号';
COMMENT ON COLUMN "received_orders"."order_number" IS '注文番号';
COMMENT ON COLUMN "received_orders"."received_date" IS '受注日';
COMMENT ON COLUMN "received_orders"."scheduled_shipment_date" IS '出荷予定日';
COMMENT ON COLUMN "received_orders"."allocation_status" IS '引当状態';
COMMENT ON COLUMN "received_orders"."total_amount" IS '受注金額合計';
COMMENT ON COLUMN "received_orders"."total_tax" IS '消費税合計';
COMMENT ON COLUMN "received_orders"."created_by" IS '作成者';
COMMENT ON COLUMN "received_orders"."created_at" IS '作成日時';
COMMENT ON COLUMN "received_orders"."updated_at" IS '更新日時';

-- 出荷テーブル
COMMENT ON TABLE "shipments" IS '出荷';
COMMENT ON COLUMN "shipments"."shipment_number" IS '出荷番号';
COMMENT ON COLUMN "shipments"."received_order_number" IS '受注番号';
COMMENT ON COLUMN "shipments"."shipment_date" IS '出荷日';
COMMENT ON COLUMN "shipments"."carrier" IS '配送業者';
COMMENT ON COLUMN "shipments"."tracking_number" IS '追跡番号';
COMMENT ON COLUMN "shipments"."created_by" IS '作成者';
COMMENT ON COLUMN "shipments"."created_at" IS '作成日時';
COMMENT ON COLUMN "shipments"."updated_at" IS '更新日時';

-- 売上テーブル
COMMENT ON TABLE "sales" IS '売上';
COMMENT ON COLUMN "sales"."sales_number" IS '売上番号';
COMMENT ON COLUMN "sales"."shipment_number" IS '出荷番号';
COMMENT ON COLUMN "sales"."sales_date" IS '売上日';
COMMENT ON COLUMN "sales"."total_amount" IS '売上金額合計';
COMMENT ON COLUMN "sales"."total_tax" IS '消費税合計';
COMMENT ON COLUMN "sales"."created_by" IS '作成者';
COMMENT ON COLUMN "sales"."created_at" IS '作成日時';
COMMENT ON COLUMN "sales"."updated_at" IS '更新日時';

-- 返品テーブル
COMMENT ON TABLE "returns" IS '返品';
COMMENT ON COLUMN "returns"."return_number" IS '返品番号';
COMMENT ON COLUMN "returns"."order_number" IS '注文番号';
COMMENT ON COLUMN "returns"."return_date" IS '返品日';
COMMENT ON COLUMN "returns"."reason" IS '返品理由';
COMMENT ON COLUMN "returns"."refund_amount" IS '返金金額';
COMMENT ON COLUMN "returns"."status" IS '処理状態';
COMMENT ON COLUMN "returns"."created_by" IS '作成者';
COMMENT ON COLUMN "returns"."created_at" IS '作成日時';
COMMENT ON COLUMN "returns"."updated_at" IS '更新日時';

-- 単品テーブル
COMMENT ON TABLE "items" IS '単品';
COMMENT ON COLUMN "items"."item_number" IS '単品番号';
COMMENT ON COLUMN "items"."item_code" IS '単品コード';
COMMENT ON COLUMN "items"."item_name" IS '単品名称';
COMMENT ON COLUMN "items"."supplier_number" IS '仕入先番号';
COMMENT ON COLUMN "items"."quality_days" IS '品質維持可能日数';
COMMENT ON COLUMN "items"."lead_time" IS 'リードタイム';
COMMENT ON COLUMN "items"."purchase_unit_quantity" IS '購入単位数量';
COMMENT ON COLUMN "items"."purchase_price" IS '仕入単価';
COMMENT ON COLUMN "items"."created_by" IS '作成者';
COMMENT ON COLUMN "items"."created_at" IS '作成日時';
COMMENT ON COLUMN "items"."updated_at" IS '更新日時';

-- 仕入先テーブル
COMMENT ON TABLE "suppliers" IS '仕入先';
COMMENT ON COLUMN "suppliers"."supplier_number" IS '仕入先番号';
COMMENT ON COLUMN "suppliers"."supplier_code" IS '仕入先コード';
COMMENT ON COLUMN "suppliers"."supplier_name" IS '仕入先名称';
COMMENT ON COLUMN "suppliers"."contact_phone" IS '連絡先電話番号';
COMMENT ON COLUMN "suppliers"."contact_email" IS '連絡先メールアドレス';
COMMENT ON COLUMN "suppliers"."status" IS '取引状態';
COMMENT ON COLUMN "suppliers"."created_by" IS '作成者';
COMMENT ON COLUMN "suppliers"."created_at" IS '作成日時';
COMMENT ON COLUMN "suppliers"."updated_at" IS '更新日時';

-- 在庫テーブル
COMMENT ON TABLE "inventories" IS '在庫';
COMMENT ON COLUMN "inventories"."inventory_id" IS '在庫ID';
COMMENT ON COLUMN "inventories"."item_number" IS '単品番号';
COMMENT ON COLUMN "inventories"."lot_number" IS 'ロット番号';
COMMENT ON COLUMN "inventories"."arrival_date" IS '入荷日';
COMMENT ON COLUMN "inventories"."expiration_date" IS '有効期限';
COMMENT ON COLUMN "inventories"."quantity" IS '数量';
COMMENT ON COLUMN "inventories"."allocated_quantity" IS '引当済数量';
COMMENT ON COLUMN "inventories"."available_quantity" IS '利用可能数量';
COMMENT ON COLUMN "inventories"."status" IS '在庫状態';
COMMENT ON COLUMN "inventories"."created_by" IS '作成者';
COMMENT ON COLUMN "inventories"."created_at" IS '作成日時';
COMMENT ON COLUMN "inventories"."updated_at" IS '更新日時';

-- 発注テーブル
COMMENT ON TABLE "placement_orders" IS '発注';
COMMENT ON COLUMN "placement_orders"."placement_order_number" IS '発注番号';
COMMENT ON COLUMN "placement_orders"."order_date" IS '発注日';
COMMENT ON COLUMN "placement_orders"."supplier_number" IS '仕入先番号';
COMMENT ON COLUMN "placement_orders"."desired_delivery_date" IS '希望納品日';
COMMENT ON COLUMN "placement_orders"."total_amount" IS '発注金額合計';
COMMENT ON COLUMN "placement_orders"."total_tax" IS '消費税合計';
COMMENT ON COLUMN "placement_orders"."status" IS '発注状態';
COMMENT ON COLUMN "placement_orders"."created_by" IS '作成者';
COMMENT ON COLUMN "placement_orders"."created_at" IS '作成日時';
COMMENT ON COLUMN "placement_orders"."updated_at" IS '更新日時';

-- 発注明細テーブル
COMMENT ON TABLE "placement_order_lines" IS '発注明細';
COMMENT ON COLUMN "placement_order_lines"."placement_order_line_number" IS '発注明細番号';
COMMENT ON COLUMN "placement_order_lines"."placement_order_number" IS '発注番号';
COMMENT ON COLUMN "placement_order_lines"."item_number" IS '単品番号';
COMMENT ON COLUMN "placement_order_lines"."order_quantity" IS '発注数量';
COMMENT ON COLUMN "placement_order_lines"."arrived_quantity" IS '入荷済数量';
COMMENT ON COLUMN "placement_order_lines"."purchase_price" IS '仕入単価';
COMMENT ON COLUMN "placement_order_lines"."created_by" IS '作成者';
COMMENT ON COLUMN "placement_order_lines"."created_at" IS '作成日時';
COMMENT ON COLUMN "placement_order_lines"."updated_at" IS '更新日時';

-- 入荷テーブル
COMMENT ON TABLE "arrivals" IS '入荷';
COMMENT ON COLUMN "arrivals"."arrival_number" IS '入荷番号';
COMMENT ON COLUMN "arrivals"."placement_order_number" IS '発注番号';
COMMENT ON COLUMN "arrivals"."arrival_date" IS '入荷日';
COMMENT ON COLUMN "arrivals"."inspection_status" IS '検収状態';
COMMENT ON COLUMN "arrivals"."created_by" IS '作成者';
COMMENT ON COLUMN "arrivals"."created_at" IS '作成日時';
COMMENT ON COLUMN "arrivals"."updated_at" IS '更新日時';

-- 入荷明細テーブル
COMMENT ON TABLE "arrival_lines" IS '入荷明細';
COMMENT ON COLUMN "arrival_lines"."arrival_line_number" IS '入荷明細番号';
COMMENT ON COLUMN "arrival_lines"."arrival_number" IS '入荷番号';
COMMENT ON COLUMN "arrival_lines"."placement_order_line_number" IS '発注明細番号';
COMMENT ON COLUMN "arrival_lines"."item_number" IS '単品番号';
COMMENT ON COLUMN "arrival_lines"."arrived_quantity" IS '入荷数量';
COMMENT ON COLUMN "arrival_lines"."inspection_result" IS '検収結果';
COMMENT ON COLUMN "arrival_lines"."created_by" IS '作成者';
COMMENT ON COLUMN "arrival_lines"."created_at" IS '作成日時';
COMMENT ON COLUMN "arrival_lines"."updated_at" IS '更新日時';

-- 仕入テーブル
COMMENT ON TABLE "purchases" IS '仕入';
COMMENT ON COLUMN "purchases"."purchase_number" IS '仕入番号';
COMMENT ON COLUMN "purchases"."arrival_number" IS '入荷番号';
COMMENT ON COLUMN "purchases"."purchase_date" IS '仕入日';
COMMENT ON COLUMN "purchases"."total_amount" IS '仕入金額合計';
COMMENT ON COLUMN "purchases"."total_tax" IS '消費税合計';
COMMENT ON COLUMN "purchases"."created_by" IS '作成者';
COMMENT ON COLUMN "purchases"."created_at" IS '作成日時';
COMMENT ON COLUMN "purchases"."updated_at" IS '更新日時';
