import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  Container,
  Typography,
  Box,
  Button,
  Paper,
  TextField,
  Grid,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
} from '@mui/material';
import { usePostApiOrders } from '../api/generated/orders/orders';

interface CartItem {
  productId: number;
  name: string;
  price: number;
  quantity: number;
}

export function Checkout() {
  const navigate = useNavigate();
  const [cart] = useState<CartItem[]>(JSON.parse(localStorage.getItem('cart') || '[]'));
  const [deliveryAddress, setDeliveryAddress] = useState('');
  const [deliveryPhone, setDeliveryPhone] = useState('');
  const [deliveryMessage, setDeliveryMessage] = useState('');
  const [desiredDeliveryDate, setDesiredDeliveryDate] = useState('');
  const { mutateAsync: createOrder, isPending } = usePostApiOrders();

  const getTotalPrice = () => {
    return cart.reduce((total, item) => total + item.price * item.quantity, 0);
  };

  const getMinDeliveryDate = () => {
    const date = new Date();
    date.setDate(date.getDate() + 2);
    return date.toISOString().split('T')[0];
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    try {
      for (let i = 0; i < cart.length; i++) {
        const item = cart[i];
        const orderId = Math.floor(Math.random() * 1000000) + 1;
        await createOrder({
          data: {
            id: orderId,
            orderDate: new Date().toISOString().split('T')[0],
            customerId: 1,
            productId: item.productId,
            quantity: item.quantity,
            desiredDeliveryDate,
            deliveryAddress,
            deliveryPhone,
            deliveryMessage: deliveryMessage || undefined,
          },
        });
      }

      localStorage.removeItem('cart');
      alert('注文が完了しました');
      navigate('/orders');
    } catch (error: any) {
      alert(error.response?.data?.message || '注文の作成に失敗しました');
    }
  };

  if (cart.length === 0) {
    return (
      <Container maxWidth="md" sx={{ mt: 4 }}>
        <Typography>カートが空です</Typography>
        <Button onClick={() => navigate('/products')} sx={{ mt: 2 }}>
          商品を見る
        </Button>
      </Container>
    );
  }

  return (
    <Container maxWidth="lg" sx={{ mt: 4, mb: 4 }}>
      <Typography variant="h4" component="h1" gutterBottom>
        注文確認
      </Typography>

      <Grid container spacing={3}>
        <Grid item xs={12} md={8}>
          <Paper sx={{ p: 3, mb: 3 }}>
            <Typography variant="h6" gutterBottom>
              配送情報
            </Typography>
            <Box component="form" onSubmit={handleSubmit}>
              <TextField
                fullWidth
                label="配送先住所"
                value={deliveryAddress}
                onChange={(e) => setDeliveryAddress(e.target.value)}
                required
                margin="normal"
              />
              <TextField
                fullWidth
                label="電話番号"
                value={deliveryPhone}
                onChange={(e) => setDeliveryPhone(e.target.value)}
                required
                margin="normal"
              />
              <TextField
                fullWidth
                label="希望配送日"
                type="date"
                value={desiredDeliveryDate}
                onChange={(e) => setDesiredDeliveryDate(e.target.value)}
                required
                margin="normal"
                InputLabelProps={{ shrink: true }}
                inputProps={{ min: getMinDeliveryDate() }}
                helperText="注文日の2日後以降を指定してください"
              />
              <TextField
                fullWidth
                label="配送メッセージ（任意）"
                value={deliveryMessage}
                onChange={(e) => setDeliveryMessage(e.target.value)}
                margin="normal"
                multiline
                rows={3}
              />
            </Box>
          </Paper>
        </Grid>

        <Grid item xs={12} md={4}>
          <Paper sx={{ p: 3 }}>
            <Typography variant="h6" gutterBottom>
              注文内容
            </Typography>
            <TableContainer>
              <Table size="small">
                <TableHead>
                  <TableRow>
                    <TableCell>商品</TableCell>
                    <TableCell align="right">数量</TableCell>
                    <TableCell align="right">小計</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {cart.map((item) => (
                    <TableRow key={item.productId}>
                      <TableCell>{item.name}</TableCell>
                      <TableCell align="right">{item.quantity}</TableCell>
                      <TableCell align="right">
                        ¥{(item.price * item.quantity).toLocaleString()}
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </TableContainer>
            <Box sx={{ mt: 2, pt: 2, borderTop: 1, borderColor: 'divider' }}>
              <Typography variant="h6">
                合計: ¥{getTotalPrice().toLocaleString()}
              </Typography>
            </Box>
            <Button
              fullWidth
              variant="contained"
              size="large"
              onClick={handleSubmit}
              disabled={isPending || !deliveryAddress || !deliveryPhone || !desiredDeliveryDate}
              sx={{ mt: 2 }}
            >
              {isPending ? '処理中...' : '注文を確定'}
            </Button>
            <Button
              fullWidth
              variant="outlined"
              onClick={() => navigate('/cart')}
              sx={{ mt: 1 }}
            >
              カートに戻る
            </Button>
          </Paper>
        </Grid>
      </Grid>
    </Container>
  );
}
