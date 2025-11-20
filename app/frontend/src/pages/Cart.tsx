import { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  Container,
  Typography,
  Box,
  Button,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  IconButton,
  TextField,
} from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';

interface CartItem {
  productId: number;
  name: string;
  price: number;
  quantity: number;
}

export function Cart() {
  const [cart, setCart] = useState<CartItem[]>([]);
  const navigate = useNavigate();

  useEffect(() => {
    loadCart();
  }, []);

  const loadCart = () => {
    const cartData = JSON.parse(localStorage.getItem('cart') || '[]');
    setCart(cartData);
  };

  const updateQuantity = (productId: number, newQuantity: number) => {
    const updatedCart = cart.map((item) =>
      item.productId === productId
        ? { ...item, quantity: Math.max(1, newQuantity) }
        : item
    );
    setCart(updatedCart);
    localStorage.setItem('cart', JSON.stringify(updatedCart));
  };

  const removeItem = (productId: number) => {
    const updatedCart = cart.filter((item) => item.productId !== productId);
    setCart(updatedCart);
    localStorage.setItem('cart', JSON.stringify(updatedCart));
  };

  const clearCart = () => {
    setCart([]);
    localStorage.removeItem('cart');
  };

  const getTotalPrice = () => {
    return cart.reduce((total, item) => total + item.price * item.quantity, 0);
  };

  const handleCheckout = () => {
    navigate('/checkout');
  };

  return (
    <Container maxWidth="lg" sx={{ mt: 4, mb: 4 }}>
      <Typography variant="h4" component="h1" gutterBottom>
        カート
      </Typography>

      {cart.length === 0 ? (
        <Box textAlign="center" mt={4}>
          <Typography color="text.secondary" gutterBottom>
            カートは空です
          </Typography>
          <Button variant="contained" onClick={() => navigate('/products')} sx={{ mt: 2 }}>
            商品を見る
          </Button>
        </Box>
      ) : (
        <>
          <TableContainer component={Paper}>
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell>商品名</TableCell>
                  <TableCell align="right">単価</TableCell>
                  <TableCell align="center">数量</TableCell>
                  <TableCell align="right">小計</TableCell>
                  <TableCell align="center">操作</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {cart.map((item) => (
                  <TableRow key={item.productId}>
                    <TableCell>{item.name}</TableCell>
                    <TableCell align="right">¥{item.price.toLocaleString()}</TableCell>
                    <TableCell align="center">
                      <TextField
                        type="number"
                        value={item.quantity}
                        onChange={(e) =>
                          updateQuantity(item.productId, parseInt(e.target.value) || 1)
                        }
                        inputProps={{ min: 1 }}
                        sx={{ width: 80 }}
                        size="small"
                      />
                    </TableCell>
                    <TableCell align="right">
                      ¥{(item.price * item.quantity).toLocaleString()}
                    </TableCell>
                    <TableCell align="center">
                      <IconButton
                        color="error"
                        onClick={() => removeItem(item.productId)}
                      >
                        <DeleteIcon />
                      </IconButton>
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </TableContainer>

          <Box sx={{ mt: 3, display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
            <Button variant="outlined" color="error" onClick={clearCart}>
              カートをクリア
            </Button>
            <Box sx={{ textAlign: 'right' }}>
              <Typography variant="h5" gutterBottom>
                合計: ¥{getTotalPrice().toLocaleString()}
              </Typography>
              <Button variant="contained" size="large" onClick={handleCheckout}>
                注文に進む
              </Button>
            </Box>
          </Box>
        </>
      )}
    </Container>
  );
}
