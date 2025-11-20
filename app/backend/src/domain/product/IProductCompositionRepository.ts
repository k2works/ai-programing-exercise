export interface IProductCompositionRepository {
  saveCompositions(
    productId: number,
    items: Array<{ itemId: number; requiredQty: number }>
  ): Promise<void>;
  
  findByProductId(productId: number): Promise<Array<{ itemId: number; requiredQty: number }>>;
}
