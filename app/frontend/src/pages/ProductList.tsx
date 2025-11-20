import {
  Container,
  Typography,
  Grid,
  Card,
  CardContent,
  CardActions,
  Button,
  Box,
  CircularProgress,
} from '@mui/material';
import { useNavigate } from 'react-router-dom';
import { useGetApiProducts } from '../api/generated/products/products';

interface CartItem {
  productId: number;
  name: string;
  price: number;
  quantity: number;
}

export function ProductList() {
  const { data: products, isLoading } = useGetApiProducts();
  const navigate = useNavigate();

  const handleViewDetails = (productId: number) => {
    navigate(`/products/${productId}`);
  };

  const handleAddToCart = (product: any) => {
    const cart: CartItem[] = JSON.parse(localStorage.getItem('cart') || '[]');
    const existingItem = cart.find((item) => item.productId === product.id);

    if (existingItem) {
      existingItem.quantity += 1;
    } else {
      cart.push({
        productId: product.id,
        name: product.name,
        price: product.salesPrice,
        quantity: 1,
      });
    }

    localStorage.setItem('cart', JSON.stringify(cart));
    alert('カートに追加しました');
  };

  if (isLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  return (
    <Container maxWidth="lg" sx={{ mt: 4, mb: 4 }}>
      <Typography variant="h4" component="h1" gutterBottom>
        商品一覧
      </Typography>

      <Grid container spacing={3}>
        {products?.map((product: any) => (
          <Grid item xs={12} sm={6} md={4} key={product.id}>
            <Card>
              <CardContent>
                <Typography variant="h6" component="h2">
                  {product.name}
                </Typography>
                <Typography color="text.secondary" gutterBottom>
                  商品コード: {product.code}
                </Typography>
                <Typography variant="h5" color="primary">
                  ¥{product.salesPrice.toLocaleString()}
                </Typography>
              </CardContent>
              <CardActions>
                <Button size="small" onClick={() => handleViewDetails(product.id)}>
                  詳細
                </Button>
                <Button
                  size="small"
                  variant="contained"
                  onClick={() => handleAddToCart(product)}
                >
                  カートに追加
                </Button>
              </CardActions>
            </Card>
          </Grid>
        ))}
      </Grid>

      {products?.length === 0 && (
        <Box textAlign="center" mt={4}>
          <Typography color="text.secondary">
            販売中の商品がありません
          </Typography>
        </Box>
      )}
    </Container>
  );
}
