# frozen_string_literal: true

# db/seeds.rb
puts '🌱 Seeding database...'

# クリーンアップ（依存関係の順に削除）
puts '  Cleaning up existing data...'
# 支払・入金関連（最も外側の依存）
BillPayment.delete_all
Payment.delete_all

# 明細データ
BillItem.delete_all
InvoiceItem.delete_all
PurchaseItem.delete_all
OrderItem.delete_all
PurchaseOrderItem.delete_all

# ヘッダーデータ
Bill.delete_all
Invoice.delete_all
Purchase.delete_all
Order.delete_all
PurchaseOrder.delete_all

# 在庫・倉庫
Stock.delete_all
Warehouse.delete_all

# 与信限度額（Partyへの外部キー制約があるため先に削除）
CreditLimit.delete_all

# 取引先関連
PartyRole.delete_all
Organization.delete_all
Person.delete_all
Party.delete_all

# 商品関連
Product.delete_all
ProductCategory.delete_all

# 社員・部門
Employee.delete_all
Department.delete_all

# 採番シーケンス
NumberSequence.delete_all

# 部門
puts '  Creating departments...'
sales_dept = Department.create!(
  code: 'D001',
  name: '営業部'
)

purchasing_dept = Department.create!(
  code: 'D002',
  name: '購買部'
)

# 社員
puts '  Creating employees...'
sales_emp = Employee.create!(
  code: 'E001',
  first_name: '太郎',
  last_name: '山田',
  department: sales_dept
)

purchasing_emp = Employee.create!(
  code: 'E002',
  first_name: '花子',
  last_name: '佐藤',
  department: purchasing_dept
)

# 商品分類
puts '  Creating product categories...'
electronics = ProductCategory.create!(
  code: 'PC001',
  name: '電子機器'
)

# 商品
puts '  Creating products...'
products = []
5.times do |i|
  products << Product.create!(
    code: "P#{(i + 1).to_s.rjust(4, '0')}",
    name: "商品#{i + 1}",
    unit_price: (i + 1) * 1000,
    product_category: electronics
  )
end

# 倉庫
puts '  Creating warehouses...'
main_warehouse = Warehouse.create!(
  code: 'W001',
  name: '本社倉庫',
  warehouse_type: 1,
  address: '東京都千代田区',
  phone: '03-1234-5678'
)

# 取引先（顧客）
puts '  Creating customers...'
customers = []
3.times do |i|
  party = Party.create!(party_type: 'Organization')
  Organization.create!(
    party: party,
    name: "株式会社顧客#{i + 1}",
    tax_id: "123456789#{i}"
  )
  PartyRole.create!(
    party: party,
    role_type: 'Customer',
    started_at: 1.year.ago
  )
  CreditLimit.create!(
    party: party,
    limit_amount: (i + 1) * 1_000_000
  )
  customers << party
end

# 取引先（仕入先）
puts '  Creating suppliers...'
suppliers = []
2.times do |i|
  party = Party.create!(party_type: 'Organization')
  Organization.create!(
    party: party,
    name: "株式会社仕入先#{i + 1}",
    tax_id: "987654321#{i}"
  )
  PartyRole.create!(
    party: party,
    role_type: 'Supplier',
    started_at: 1.year.ago
  )
  suppliers << party
end

# 受注
puts '  Creating sales orders...'
orders = []
customers.each_with_index do |customer, i|
  order = Order.create!(
    order_type: 'Sales',
    order_date: (i + 1).days.ago,
    party: customer,
    status: 'confirmed'
  )

  # 注文明細
  2.times do |j|
    OrderItem.create!(
      order: order,
      product: products[j],
      quantity: (j + 1) * 10,
      unit_price: products[j].unit_price
    )
  end

  orders << order
end

# 発注
puts '  Creating purchase orders...'
purchase_orders = []
suppliers.each_with_index do |supplier, i|
  po = PurchaseOrder.create!(
    order_date: (i + 1).days.ago,
    party: supplier,
    warehouse: main_warehouse,
    status: 'submitted'
  )

  # 発注明細
  2.times do |j|
    PurchaseOrderItem.create!(
      purchase_order: po,
      product: products[j],
      quantity: (j + 1) * 50,
      unit_price: products[j].unit_price * 0.7 # 仕入単価は売価の70%
    )
  end

  purchase_orders << po
end

# 仕入と在庫
puts '  Creating purchases and stocks...'
purchase_orders.each_with_index do |po, i|
  purchase = Purchase.create!(
    purchase_date: po.order_date + 1.day,
    purchase_order: po,
    party: po.party
  )

  po.purchase_order_items.each_with_index do |po_item, j|
    PurchaseItem.create!(
      purchase: purchase,
      product: po_item.product,
      lot_number: "LOT#{Date.current.strftime('%Y%m%d')}#{(i * 10 + j).to_s.rjust(3, '0')}",
      warehouse: main_warehouse,
      quantity: po_item.quantity,
      unit_price: po_item.unit_price
    )
  end
end

# 請求書
puts '  Creating invoices...'
orders.group_by(&:party).each do |customer, customer_orders|
  invoice = Invoice.create!(
    invoice_date: Date.current,
    closing_date: Date.current.end_of_month,
    due_date: Date.current.end_of_month + 1.month,
    party: customer
  )

  customer_orders.each do |order|
    InvoiceItem.create!(
      invoice: invoice,
      order: order,
      amount: order.calculate_total
    )
  end
end

# 採番シーケンス
# 注意: Order、PurchaseOrder、Purchase、Invoiceの作成時に自動的に生成されます
puts '  Number sequences already created automatically'

puts '✅ Seeding completed!'
puts "  Departments: #{Department.count}"
puts "  Employees: #{Employee.count}"
puts "  Products: #{Product.count}"
puts "  Customers: #{customers.count}"
puts "  Suppliers: #{suppliers.count}"
puts "  Orders: #{orders.count}"
puts "  Purchase Orders: #{purchase_orders.count}"
puts "  Stocks: #{Stock.count}"
puts "  Invoices: #{Invoice.count}"
