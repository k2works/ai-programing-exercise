import { useParams, useNavigate } from 'react-router-dom';
import {
  Container,
  Typography,
  Box,
  Paper,
  Grid,
  Button,
  CircularProgress,
  Divider,
  Chip,
} from '@mui/material';
import {
  useGetApiStaffOrdersId,
  usePostApiStaffOrdersIdConfirm,
} from '../api/generated/staff-orders/staff-orders';

export function ReceivedOrderDetail() {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const { data, isLoading, refetch } = useGetApiStaffOrdersId(id!);
  const { mutateAsync: confirmOrder } = usePostApiStaffOrdersIdConfirm();

  const handleConfirm = async () => {
    if (!confirm('この注文を確認しますか？在庫が引き当てられます。')) return;

    try {
      await confirmOrder({ id: id! });
      alert('受注を確認しました');
      refetch();
    } catch (error: any) {
      alert(error.response?.data?.error || '受注確認に失敗しました');
    }
  };

  if (isLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  const order = data?.order;
  const receivedOrder = data?.receivedOrder;

  return (
    <Container maxWidth="md" sx={{ mt: 4, mb: 4 }}>
      <Box display="flex" justifyContent="space-between" alignItems="center" mb={3}>
        <Typography variant="h4" component="h1">
          受注詳細
        </Typography>
        <Button variant="outlined" onClick={() => navigate('/staff/orders')}>
          一覧に戻る
        </Button>
      </Box>

      <Paper sx={{ p: 3, mb: 3 }}>
        <Typography variant="h6" gutterBottom>
          注文情報
        </Typography>
        <Divider sx={{ mb: 2 }} />
        <Grid container spacing={2}>
          <Grid item xs={6}>
            <Typography variant="body2" color="text.secondary">
              注文ID
            </Typography>
            <Typography variant="body1">{order?.id}</Typography>
          </Grid>
          <Grid item xs={6}>
            <Typography variant="body2" color="text.secondary">
              注文日
            </Typography>
            <Typography variant="body1">
              {order?.orderDate && new Date(order.orderDate).toLocaleDateString()}
            </Typography>
          </Grid>
          <Grid item xs={6}>
            <Typography variant="body2" color="text.secondary">
              得意先ID
            </Typography>
            <Typography variant="body1">{order?.customerId}</Typography>
          </Grid>
          <Grid item xs={6}>
            <Typography variant="body2" color="text.secondary">
              商品ID
            </Typography>
            <Typography variant="body1">{order?.productId}</Typography>
          </Grid>
          <Grid item xs={6}>
            <Typography variant="body2" color="text.secondary">
              数量
            </Typography>
            <Typography variant="body1">{order?.quantity}</Typography>
          </Grid>
          <Grid item xs={6}>
            <Typography variant="body2" color="text.secondary">
              希望配送日
            </Typography>
            <Typography variant="body1">
              {order?.desiredDeliveryDate &&
                new Date(order.desiredDeliveryDate).toLocaleDateString()}
            </Typography>
          </Grid>
          <Grid item xs={12}>
            <Typography variant="body2" color="text.secondary">
              配送先住所
            </Typography>
            <Typography variant="body1">{order?.deliveryAddress}</Typography>
          </Grid>
          <Grid item xs={6}>
            <Typography variant="body2" color="text.secondary">
              配送先電話番号
            </Typography>
            <Typography variant="body1">{order?.deliveryPhone}</Typography>
          </Grid>
          <Grid item xs={6}>
            <Typography variant="body2" color="text.secondary">
              ステータス
            </Typography>
            <Chip label={order?.status} color="warning" size="small" />
          </Grid>
          {order?.deliveryMessage && (
            <Grid item xs={12}>
              <Typography variant="body2" color="text.secondary">
                配送メッセージ
              </Typography>
              <Typography variant="body1">{order.deliveryMessage}</Typography>
            </Grid>
          )}
        </Grid>
      </Paper>

      {receivedOrder && (
        <Paper sx={{ p: 3, mb: 3 }}>
          <Typography variant="h6" gutterBottom>
            受注情報
          </Typography>
          <Divider sx={{ mb: 2 }} />
          <Grid container spacing={2}>
            <Grid item xs={6}>
              <Typography variant="body2" color="text.secondary">
                受注ID
              </Typography>
              <Typography variant="body1">{receivedOrder.id}</Typography>
            </Grid>
            <Grid item xs={6}>
              <Typography variant="body2" color="text.secondary">
                受注日
              </Typography>
              <Typography variant="body1">
                {receivedOrder.receivedDate &&
                  new Date(receivedOrder.receivedDate).toLocaleDateString()}
              </Typography>
            </Grid>
            <Grid item xs={6}>
              <Typography variant="body2" color="text.secondary">
                予定出荷日
              </Typography>
              <Typography variant="body1">
                {receivedOrder.scheduledShipmentDate &&
                  new Date(receivedOrder.scheduledShipmentDate).toLocaleDateString()}
              </Typography>
            </Grid>
            <Grid item xs={6}>
              <Typography variant="body2" color="text.secondary">
                引当状態
              </Typography>
              <Chip
                label={receivedOrder.allocationStatus === 'allocated' ? '引当済' : '未引当'}
                color={receivedOrder.allocationStatus === 'allocated' ? 'success' : 'default'}
                size="small"
              />
            </Grid>
            <Grid item xs={6}>
              <Typography variant="body2" color="text.secondary">
                合計金額
              </Typography>
              <Typography variant="body1">
                ¥{(receivedOrder.totalAmount || 0).toLocaleString()}
              </Typography>
            </Grid>
            <Grid item xs={6}>
              <Typography variant="body2" color="text.secondary">
                消費税
              </Typography>
              <Typography variant="body1">
                ¥{(receivedOrder.totalTax || 0).toLocaleString()}
              </Typography>
            </Grid>
          </Grid>
        </Paper>
      )}

      {!receivedOrder && order?.status === 'pending' && (
        <Box textAlign="center">
          <Button variant="contained" color="primary" size="large" onClick={handleConfirm}>
            受注確認
          </Button>
        </Box>
      )}
    </Container>
  );
}
