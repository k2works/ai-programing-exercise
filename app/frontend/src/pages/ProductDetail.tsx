import { useState } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import {
  Container,
  Typography,
  Box,
  Button,
  Paper,
  CircularProgress,
  TextField,
} from '@mui/material';
import { useGetApiProductsId } from '../api/generated/products/products';

export function ProductDetail() {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const { data: product, isLoading } = useGetApiProductsId(id!);
  const [quantity, setQuantity] = useState(1);

  const handleAddToCart = () => {
    if (!product) return;

    const cart = JSON.parse(localStorage.getItem('cart') || '[]');
    const existingItem = cart.find((item: any) => item.productId === product.id);

    if (existingItem) {
      existingItem.quantity += quantity;
    } else {
      cart.push({
        productId: product.id,
        name: product.name,
        price: product.salesPrice,
        quantity,
      });
    }

    localStorage.setItem('cart', JSON.stringify(cart));
    alert('カートに追加しました');
    navigate('/products');
  };

  if (isLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  if (!product) {
    return (
      <Container maxWidth="md" sx={{ mt: 4 }}>
        <Typography>商品が見つかりません</Typography>
        <Button onClick={() => navigate('/products')} sx={{ mt: 2 }}>
          商品一覧に戻る
        </Button>
      </Container>
    );
  }

  return (
    <Container maxWidth="md" sx={{ mt: 4, mb: 4 }}>
      <Paper sx={{ p: 4 }}>
        <Typography variant="h4" component="h1" gutterBottom>
          {product.name}
        </Typography>

        <Box sx={{ mt: 3 }}>
          <Typography color="text.secondary" gutterBottom>
            商品コード: {product.code}
          </Typography>

          <Typography variant="h5" color="primary" sx={{ mt: 2, mb: 3 }}>
            ¥{product.salesPrice.toLocaleString()}
          </Typography>

          <Box sx={{ display: 'flex', alignItems: 'center', gap: 2, mt: 3 }}>
            <TextField
              type="number"
              label="数量"
              value={quantity}
              onChange={(e) => setQuantity(Math.max(1, parseInt(e.target.value) || 1))}
              inputProps={{ min: 1 }}
              sx={{ width: 120 }}
            />
            <Button variant="contained" size="large" onClick={handleAddToCart}>
              カートに追加
            </Button>
          </Box>

          <Button onClick={() => navigate('/products')} sx={{ mt: 3 }}>
            商品一覧に戻る
          </Button>
        </Box>
      </Paper>
    </Container>
  );
}
