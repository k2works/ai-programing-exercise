# frozen_string_literal: true

require 'rails_helper'

RSpec.describe '調達フロー統合テスト', type: :model do
  let(:warehouse) { create(:warehouse) }
  let(:supplier) { create(:party, :organization) }
  let(:product) { create(:product) }

  describe '発注から仕入、在庫計上まで' do
    it '正常に処理できる' do
      # 1. 発注作成
      po = create(:purchase_order, party: supplier, warehouse: warehouse)
      po_item = create(:purchase_order_item, purchase_order: po, product: product, quantity: 100)

      expect(po.draft?).to be true
      expect(po.calculate_total).to eq(100_000)

      # 2. 発注確定
      po.submit!
      expect(po.submitted?).to be true

      # 3. 仕入作成（purchase_items を先に作成してから purchase を作成）
      purchase = Purchase.new(purchase_number: 'PUR20250106001', purchase_date: Date.current,
                              purchase_order: po, party: supplier)
      purchase.purchase_items.build(product: product, warehouse: warehouse,
                                    lot_number: 'LOT20250106001', quantity: 100, unit_price: 1000)
      purchase.save!

      # 4. 在庫が計上されていることを確認
      stock = Stock.find_by(
        warehouse_id: warehouse.id,
        product_id: product.id,
        lot_number: 'LOT20250106001',
        stock_type: 'normal',
        quality_type: 'good'
      )

      expect(stock).to be_present
      expect(stock.actual_quantity).to eq(100)
      expect(stock.valid_quantity).to eq(100)

      # 5. 発注明細の入荷数量が更新されている
      expect(po_item.reload.received_quantity).to eq(100)
      expect(po.reload.all_received?).to be true

      # 6. 発注を入荷完了にする
      po.receive!
      expect(po.received?).to be true
    end

    it '在庫引当ができる' do
      # 在庫作成
      stock = create(:stock,
                     warehouse: warehouse,
                     product: product,
                     actual_quantity: 100,
                     valid_quantity: 100)

      # 引当
      stock.allocate(30)
      expect(stock.actual_quantity).to eq(100)
      expect(stock.valid_quantity).to eq(70)
      expect(stock.allocated_quantity).to eq(30)

      # 引当解除
      stock.deallocate(10)
      expect(stock.valid_quantity).to eq(80)
      expect(stock.allocated_quantity).to eq(20)
    end
  end
end
