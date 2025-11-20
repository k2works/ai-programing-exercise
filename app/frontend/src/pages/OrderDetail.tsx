import { useState } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import {
  Container,
  Typography,
  Box,
  Button,
  Paper,
  Grid,
  Chip,
  CircularProgress,
  TextField,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
} from '@mui/material';
import {
  useGetApiOrdersId,
  usePatchApiOrdersIdDeliveryDate,
  usePatchApiOrdersIdCancel,
} from '../api/generated/orders/orders';

export function OrderDetail() {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const { data: order, isLoading, refetch } = useGetApiOrdersId(id!);
  const { mutateAsync: changeDeliveryDate } = usePatchApiOrdersIdDeliveryDate();
  const { mutateAsync: cancelOrder } = usePatchApiOrdersIdCancel();
  const [changeDateDialogOpen, setChangeDateDialogOpen] = useState(false);
  const [newDeliveryDate, setNewDeliveryDate] = useState('');

  const handleChangeDeliveryDate = async () => {
    try {
      await changeDeliveryDate({
        id: id!,
        data: { newDate: newDeliveryDate },
      });
      alert('配送日を変更しました');
      setChangeDateDialogOpen(false);
      refetch();
    } catch (error: any) {
      alert(error.response?.data?.message || '配送日の変更に失敗しました');
    }
  };

  const handleCancelOrder = async () => {
    if (!confirm('注文をキャンセルしますか？')) return;

    try {
      await cancelOrder({ id: id! });
      alert('注文をキャンセルしました');
      refetch();
    } catch (error: any) {
      alert(error.response?.data?.message || '注文のキャンセルに失敗しました');
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

  const canModify = (status: string) => {
    return status === 'pending';
  };

  if (isLoading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  if (!order) {
    return (
      <Container maxWidth="md" sx={{ mt: 4 }}>
        <Typography>注文が見つかりません</Typography>
        <Button onClick={() => navigate('/orders')} sx={{ mt: 2 }}>
          注文履歴に戻る
        </Button>
      </Container>
    );
  }

  const statusInfo = getStatusLabel(order.status);

  return (
    <Container maxWidth="md" sx={{ mt: 4, mb: 4 }}>
      <Typography variant="h4" component="h1" gutterBottom>
        注文詳細
      </Typography>

      <Paper sx={{ p: 3 }}>
        <Grid container spacing={2}>
          <Grid item xs={12}>
            <Box display="flex" justifyContent="space-between" alignItems="center">
              <Typography variant="h6">注文番号: {order.id}</Typography>
              <Chip label={statusInfo.label} color={statusInfo.color} />
            </Box>
          </Grid>

          <Grid item xs={12} sm={6}>
            <Typography color="text.secondary">注文日</Typography>
            <Typography>{new Date(order.orderDate).toLocaleDateString('ja-JP')}</Typography>
          </Grid>

          <Grid item xs={12} sm={6}>
            <Typography color="text.secondary">配送希望日</Typography>
            <Typography>
              {new Date(order.desiredDeliveryDate).toLocaleDateString('ja-JP')}
            </Typography>
          </Grid>

          <Grid item xs={12} sm={6}>
            <Typography color="text.secondary">商品ID</Typography>
            <Typography>{order.productId}</Typography>
          </Grid>

          <Grid item xs={12} sm={6}>
            <Typography color="text.secondary">数量</Typography>
            <Typography>{order.quantity}</Typography>
          </Grid>

          <Grid item xs={12}>
            <Typography color="text.secondary">配送先住所</Typography>
            <Typography>{order.deliveryAddress}</Typography>
          </Grid>

          <Grid item xs={12}>
            <Typography color="text.secondary">電話番号</Typography>
            <Typography>{order.deliveryPhone}</Typography>
          </Grid>

          {order.deliveryMessage && (
            <Grid item xs={12}>
              <Typography color="text.secondary">配送メッセージ</Typography>
              <Typography>{order.deliveryMessage}</Typography>
            </Grid>
          )}
        </Grid>

        <Box sx={{ mt: 3, display: 'flex', gap: 2 }}>
          {canModify(order.status) && (
            <>
              <Button
                variant="outlined"
                onClick={() => {
                  setNewDeliveryDate(
                    new Date(order.desiredDeliveryDate).toISOString().split('T')[0]
                  );
                  setChangeDateDialogOpen(true);
                }}
              >
                配送日変更
              </Button>
              <Button variant="outlined" color="error" onClick={handleCancelOrder}>
                注文キャンセル
              </Button>
            </>
          )}
          <Button onClick={() => navigate('/orders')}>注文履歴に戻る</Button>
        </Box>
      </Paper>

      <Dialog open={changeDateDialogOpen} onClose={() => setChangeDateDialogOpen(false)}>
        <DialogTitle>配送日変更</DialogTitle>
        <DialogContent>
          <TextField
            fullWidth
            label="新しい配送希望日"
            type="date"
            value={newDeliveryDate}
            onChange={(e) => setNewDeliveryDate(e.target.value)}
            margin="normal"
            InputLabelProps={{ shrink: true }}
          />
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setChangeDateDialogOpen(false)}>キャンセル</Button>
          <Button onClick={handleChangeDeliveryDate} variant="contained">
            変更
          </Button>
        </DialogActions>
      </Dialog>
    </Container>
  );
}
