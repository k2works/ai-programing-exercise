# frozen_string_literal: true

module Api
  module V1
    class OrdersController < ApplicationController
      before_action :set_order, only: %i[show update destroy confirm]

      # GET /api/v1/orders
      def index
        @orders = Order.includes(:party, :order_items)
                       .page(params[:page])
                       .per(params[:per_page] || 20)

        render json: {
          orders: @orders.map { |order| order_json(order) },
          meta: {
            current_page: @orders.current_page,
            total_pages: @orders.total_pages,
            total_count: @orders.total_count
          }
        }
      end

      # GET /api/v1/orders/:id
      def show
        render json: order_json(@order)
      end

      # POST /api/v1/orders
      def create
        @order = Order.new(order_params)

        if @order.save
          render json: order_json(@order), status: :created
        else
          render json: { errors: @order.errors.full_messages }, status: :unprocessable_entity
        end
      end

      # PATCH/PUT /api/v1/orders/:id
      def update
        if @order.update(order_params)
          render json: order_json(@order)
        else
          render json: { errors: @order.errors.full_messages }, status: :unprocessable_entity
        end
      end

      # DELETE /api/v1/orders/:id
      def destroy
        @order.destroy
        head :no_content
      end

      # POST /api/v1/orders/:id/confirm
      def confirm
        if @order.confirm!
          render json: order_json(@order)
        else
          render json: { errors: @order.errors.full_messages }, status: :unprocessable_entity
        end
      end

      private

      def set_order
        @order = Order.find(params[:id])
      end

      def order_params
        params.require(:order).permit(
          :order_type,
          :order_date,
          :party_id,
          :status,
          order_items_attributes: %i[id product_id quantity unit_price _destroy]
        )
      end

      def order_json(order)
        {
          id: order.id,
          order_number: order.order_number,
          order_type: order.order_type,
          order_date: order.order_date,
          status: order.status,
          party: {
            id: order.party.id,
            name: order.party.organization&.name || order.party.person&.full_name
          },
          items: order.order_items.map do |item|
            {
              id: item.id,
              product_id: item.product_id,
              product_name: item.product.name,
              quantity: item.quantity,
              unit_price: item.unit_price.to_f,
              subtotal: item.subtotal.to_f
            }
          end,
          total_amount: order.total_amount.to_f,
          created_at: order.created_at,
          updated_at: order.updated_at
        }
      end
    end
  end
end
