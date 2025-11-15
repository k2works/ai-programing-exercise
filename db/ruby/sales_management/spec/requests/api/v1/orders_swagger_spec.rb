# frozen_string_literal: true

require 'swagger_helper'

RSpec.describe 'Api::V1::Orders', type: :request do
  path '/api/v1/orders' do
    get 'List orders' do
      tags 'Orders'
      produces 'application/json'
      parameter name: :page, in: :query, type: :integer, required: false, description: 'Page number'
      parameter name: :per_page, in: :query, type: :integer, required: false, description: 'Items per page'

      response '200', 'orders found' do
        schema type: :object,
               properties: {
                 orders: {
                   type: :array,
                   items: { '$ref' => '#/components/schemas/Order' }
                 },
                 meta: { '$ref' => '#/components/schemas/PaginationMeta' }
               },
               required: %w[orders meta]

        let(:customer) { create(:party, :organization) }
        let!(:orders) { create_list(:order, 5, :sales, party: customer) }

        run_test! do |response|
          data = JSON.parse(response.body)
          expect(data['orders'].length).to eq(5)
          expect(data['meta']).to have_key('current_page')
        end
      end
    end

    post 'Create an order' do
      tags 'Orders'
      consumes 'application/json'
      produces 'application/json'
      parameter name: :order, in: :body, schema: {
        type: :object,
        properties: {
          order: { '$ref' => '#/components/schemas/OrderInput' }
        },
        required: ['order']
      }

      response '201', 'order created' do
        schema '$ref' => '#/components/schemas/Order'

        let(:customer) { create(:party, :organization) }
        let(:product) { create(:product) }
        let(:order) do
          {
            order: {
              order_type: 'Sales',
              party_id: customer.id,
              order_items_attributes: [
                {
                  product_id: product.id,
                  quantity: 10,
                  unit_price: 1000
                }
              ]
            }
          }
        end

        run_test! do |response|
          data = JSON.parse(response.body)
          expect(data['order_number']).to be_present
          expect(data['items'].length).to eq(1)
        end
      end

      response '422', 'invalid request' do
        schema '$ref' => '#/components/schemas/Error'

        let(:order) { { order: { order_type: 'Invalid' } } }

        run_test!
      end
    end
  end

  path '/api/v1/orders/{id}' do
    parameter name: :id, in: :path, type: :integer, description: 'Order ID'

    get 'Show an order' do
      tags 'Orders'
      produces 'application/json'

      response '200', 'order found' do
        schema '$ref' => '#/components/schemas/Order'

        let(:customer) { create(:party, :organization) }
        let(:order_record) { create(:order, :sales, party: customer) }
        let(:id) { order_record.id }

        run_test! do |response|
          data = JSON.parse(response.body)
          expect(data['id']).to eq(order_record.id)
        end
      end

      response '404', 'order not found' do
        let(:id) { 99_999 }
        run_test!
      end
    end

    patch 'Update an order' do
      tags 'Orders'
      consumes 'application/json'
      produces 'application/json'
      parameter name: :order, in: :body, schema: {
        type: :object,
        properties: {
          order: { '$ref' => '#/components/schemas/OrderInput' }
        }
      }

      response '200', 'order updated' do
        schema '$ref' => '#/components/schemas/Order'

        let(:customer) { create(:party, :organization) }
        let(:existing_order) { create(:order, :sales, party: customer) }
        let(:id) { existing_order.id }
        let(:order) { { order: { status: 'confirmed' } } }

        run_test!
      end
    end

    delete 'Delete an order' do
      tags 'Orders'

      response '204', 'order deleted' do
        let(:customer) { create(:party, :organization) }
        let(:order_record) { create(:order, :sales, party: customer) }
        let(:id) { order_record.id }

        run_test!
      end
    end
  end

  path '/api/v1/orders/{id}/confirm' do
    parameter name: :id, in: :path, type: :integer, description: 'Order ID'

    post 'Confirm an order' do
      tags 'Orders'
      produces 'application/json'
      description 'Change order status to confirmed'

      response '200', 'order confirmed' do
        schema '$ref' => '#/components/schemas/Order'

        let(:customer) { create(:party, :organization) }
        let(:order_record) { create(:order, :sales, party: customer) }
        let(:id) { order_record.id }

        run_test! do |response|
          data = JSON.parse(response.body)
          expect(data['status']).to eq('confirmed')
        end
      end
    end
  end
end
