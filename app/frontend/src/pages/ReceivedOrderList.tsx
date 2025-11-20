import { useState } from 'react';
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
  CircularProgress,
  Chip,
} from '@mui/material';
import { useGetApiStaffOrdersPending } from '../api/generated/staff-orders/staff-orders';

export function ReceivedOrderList() {
  const navigate = useNavigate();
  const { data: orders, isLoading, refetch } = useGetApiStaffOrdersPending();

  if (isLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  return (
    <Container maxWidth="lg" sx={{ mt: 4, mb: 4 }}>
      <Box display="flex" justifyContent="space-between" alignItems="center" mb={3}>
        <Typography variant="h4" component="h1">
          受注管理
        </Typography>
      </Box>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>注文ID</TableCell>
              <TableCell>注文日</TableCell>
              <TableCell>得意先ID</TableCell>
              <TableCell>商品ID</TableCell>
              <TableCell>数量</TableCell>
              <TableCell>希望配送日</TableCell>
              <TableCell>ステータス</TableCell>
              <TableCell align="center">操作</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {orders?.map((order: any) => (
              <TableRow key={order.id}>
                <TableCell>{order.id}</TableCell>
                <TableCell>{new Date(order.orderDate).toLocaleDateString()}</TableCell>
                <TableCell>{order.customerId}</TableCell>
                <TableCell>{order.productId}</TableCell>
                <TableCell>{order.quantity}</TableCell>
                <TableCell>
                  {new Date(order.desiredDeliveryDate).toLocaleDateString()}
                </TableCell>
                <TableCell>
                  <Chip label={order.status} color="warning" size="small" />
                </TableCell>
                <TableCell align="center">
                  <Button
                    size="small"
                    variant="contained"
                    onClick={() => navigate(`/staff/orders/${order.id}`)}
                  >
                    詳細
                  </Button>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>

      {orders?.length === 0 && (
        <Box textAlign="center" mt={4}>
          <Typography color="text.secondary">未処理の注文はありません</Typography>
        </Box>
      )}
    </Container>
  );
}
