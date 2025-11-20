import { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  Container,
  Typography,
  Box,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Button,
  Chip,
  CircularProgress,
} from '@mui/material';
import axios from 'axios';

interface Order {
  id: number;
  orderDate: string;
  productId: number;
  quantity: number;
  desiredDeliveryDate: string;
  deliveryAddress: string;
  status: string;
}

export function OrderList() {
  const [orders, setOrders] = useState<Order[]>([]);
  const [loading, setLoading] = useState(true);
  const navigate = useNavigate();

  useEffect(() => {
    fetchOrders();
  }, []);

  const fetchOrders = async () => {
    try {
      const token = localStorage.getItem('token');
      // TODO: Get actual customer ID from user context
      const customerId = 1;
      const response = await axios.get(
        `http://localhost:3000/api/customers/${customerId}/orders`,
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );
      setOrders(response.data);
    } catch (error) {
      console.error('Failed to fetch orders:', error);
    } finally {
      setLoading(false);
    }
  };

  const getStatusLabel = (status: string) => {
    const statusMap: { [key: string]: { label: string; color: any } } = {
      pending: { label: '保留中', color: 'warning' },
      shipped: { label: '出荷済み', color: 'info' },
      delivered: { label: '配送完了', color: 'success' },
      cancelled: { label: 'キャンセル', color: 'error' },
    };
    return statusMap[status] || { label: status, color: 'default' };
  };

  const handleViewDetails = (orderId: number) => {
    navigate(`/orders/${orderId}`);
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  return (
    <Container maxWidth="lg" sx={{ mt: 4, mb: 4 }}>
      <Typography variant="h4" component="h1" gutterBottom>
        注文履歴
      </Typography>

      {orders.length === 0 ? (
        <Box textAlign="center" mt={4}>
          <Typography color="text.secondary" gutterBottom>
            注文履歴がありません
          </Typography>
          <Button variant="contained" onClick={() => navigate('/products')} sx={{ mt: 2 }}>
            商品を見る
          </Button>
        </Box>
      ) : (
        <TableContainer component={Paper}>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>注文番号</TableCell>
                <TableCell>注文日</TableCell>
                <TableCell>配送希望日</TableCell>
                <TableCell>数量</TableCell>
                <TableCell>配送先</TableCell>
                <TableCell>ステータス</TableCell>
                <TableCell align="center">操作</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {orders.map((order) => {
                const statusInfo = getStatusLabel(order.status);
                return (
                  <TableRow key={order.id}>
                    <TableCell>{order.id}</TableCell>
                    <TableCell>{new Date(order.orderDate).toLocaleDateString('ja-JP')}</TableCell>
                    <TableCell>
                      {new Date(order.desiredDeliveryDate).toLocaleDateString('ja-JP')}
                    </TableCell>
                    <TableCell>{order.quantity}</TableCell>
                    <TableCell>{order.deliveryAddress}</TableCell>
                    <TableCell>
                      <Chip label={statusInfo.label} color={statusInfo.color} size="small" />
                    </TableCell>
                    <TableCell align="center">
                      <Button size="small" onClick={() => handleViewDetails(order.id)}>
                        詳細
                      </Button>
                    </TableCell>
                  </TableRow>
                );
              })}
            </TableBody>
          </Table>
        </TableContainer>
      )}
    </Container>
  );
}
