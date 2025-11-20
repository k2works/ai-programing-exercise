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
import { useGetApiCustomersCustomerIdOrders } from '../api/generated/orders/orders';

export function OrderList() {
  const customerId = '1'; // TODO: Get from user context
  const { data: orders, isLoading } = useGetApiCustomersCustomerIdOrders(customerId);
  const navigate = useNavigate();

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
        注文履歴
      </Typography>

      {orders?.length === 0 ? (
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
              {orders?.map((order: any) => {
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
