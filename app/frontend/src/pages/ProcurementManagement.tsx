import { useState } from 'react';
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
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  Grid,
  IconButton,
  Chip,
} from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import AddIcon from '@mui/icons-material/Add';
import {
  usePostApiPlacementOrders,
  useDeleteApiPlacementOrdersId,
} from '../api/generated/procurement/procurement';

export function ProcurementManagement() {
  const [dialogOpen, setDialogOpen] = useState(false);
  const { mutateAsync: createOrder } = usePostApiPlacementOrders();
  const { mutateAsync: cancelOrder } = useDeleteApiPlacementOrdersId();

  // Mock orders data
  const [orders, setOrders] = useState([
    {
      id: 1,
      orderDate: '2025-01-01',
      supplierId: 1,
      desiredDeliveryDate: '2025-01-10',
      status: 'pending',
      totalAmount: 100000,
    },
  ]);

  const [formData, setFormData] = useState({
    id: 0,
    orderDate: new Date().toISOString().split('T')[0],
    supplierId: 1,
    desiredDeliveryDate: '',
  });

  const [lines, setLines] = useState<Array<{ itemId: number; orderQty: number }>>([]);
  const [newLine, setNewLine] = useState({ itemId: 0, orderQty: 10 });

  const handleCreate = () => {
    setFormData({
      id: 0,
      orderDate: new Date().toISOString().split('T')[0],
      supplierId: 1,
      desiredDeliveryDate: '',
    });
    setLines([]);
    setDialogOpen(true);
  };

  const handleAddLine = () => {
    if (newLine.itemId > 0 && newLine.orderQty > 0) {
      setLines([...lines, newLine]);
      setNewLine({ itemId: 0, orderQty: 10 });
    }
  };

  const handleRemoveLine = (index: number) => {
    setLines(lines.filter((_, i) => i !== index));
  };

  const handleSubmit = async () => {
    try {
      await createOrder({
        data: {
          id: formData.id,
          orderDate: formData.orderDate,
          supplierId: formData.supplierId,
          desiredDeliveryDate: formData.desiredDeliveryDate,
          lines,
        },
      });
      alert('発注を作成しました');
      setDialogOpen(false);
      // Refresh orders list
      setOrders([
        ...orders,
        { ...formData, status: 'pending', totalAmount: lines.reduce((sum, l) => sum + l.orderQty * 100, 0) },
      ]);
    } catch (error: any) {
      alert(error.response?.data?.error || '発注作成に失敗しました');
    }
  };

  const handleCancel = async (id: number) => {
    if (!confirm('この発注をキャンセルしますか？')) return;

    try {
      await cancelOrder({ id: String(id) });
      alert('発注をキャンセルしました');
      setOrders(orders.map((o) => (o.id === id ? { ...o, status: 'cancelled' } : o)));
    } catch (error: any) {
      alert(error.response?.data?.error || 'キャンセルに失敗しました');
    }
  };

  return (
    <Container maxWidth="lg" sx={{ mt: 4, mb: 4 }}>
      <Box display="flex" justifyContent="space-between" alignItems="center" mb={3}>
        <Typography variant="h4" component="h1">
          発注管理
        </Typography>
        <Button variant="contained" onClick={handleCreate}>
          発注作成
        </Button>
      </Box>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>発注ID</TableCell>
              <TableCell>発注日</TableCell>
              <TableCell>仕入先ID</TableCell>
              <TableCell>希望配送日</TableCell>
              <TableCell>合計金額</TableCell>
              <TableCell>ステータス</TableCell>
              <TableCell align="center">操作</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {orders.map((order) => (
              <TableRow key={order.id}>
                <TableCell>{order.id}</TableCell>
                <TableCell>{new Date(order.orderDate).toLocaleDateString()}</TableCell>
                <TableCell>{order.supplierId}</TableCell>
                <TableCell>{new Date(order.desiredDeliveryDate).toLocaleDateString()}</TableCell>
                <TableCell>¥{order.totalAmount.toLocaleString()}</TableCell>
                <TableCell>
                  <Chip
                    label={order.status === 'pending' ? '未処理' : 'キャンセル'}
                    color={order.status === 'pending' ? 'warning' : 'default'}
                    size="small"
                  />
                </TableCell>
                <TableCell align="center">
                  {order.status === 'pending' && (
                    <Button size="small" color="error" onClick={() => handleCancel(order.id)}>
                      キャンセル
                    </Button>
                  )}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} maxWidth="md" fullWidth>
        <DialogTitle>発注作成</DialogTitle>
        <DialogContent>
          <Grid container spacing={2} sx={{ mt: 1 }}>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="発注ID"
                type="number"
                value={formData.id}
                onChange={(e) => setFormData({ ...formData, id: Number(e.target.value) })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="仕入先ID"
                type="number"
                value={formData.supplierId}
                onChange={(e) => setFormData({ ...formData, supplierId: Number(e.target.value) })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="発注日"
                type="date"
                value={formData.orderDate}
                onChange={(e) => setFormData({ ...formData, orderDate: e.target.value })}
                required
                InputLabelProps={{ shrink: true }}
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="希望配送日"
                type="date"
                value={formData.desiredDeliveryDate}
                onChange={(e) => setFormData({ ...formData, desiredDeliveryDate: e.target.value })}
                required
                InputLabelProps={{ shrink: true }}
              />
            </Grid>
          </Grid>

          <Box sx={{ mt: 3 }}>
            <Typography variant="subtitle1" gutterBottom>
              発注明細
            </Typography>
            {lines.map((line, index) => (
              <Box key={index} display="flex" alignItems="center" gap={2} mb={1}>
                <Typography>単品ID: {line.itemId}</Typography>
                <Typography>数量: {line.orderQty}</Typography>
                <IconButton size="small" onClick={() => handleRemoveLine(index)}>
                  <DeleteIcon />
                </IconButton>
              </Box>
            ))}

            <Box display="flex" alignItems="center" gap={2} mt={2}>
              <TextField
                label="単品ID"
                type="number"
                value={newLine.itemId}
                onChange={(e) => setNewLine({ ...newLine, itemId: Number(e.target.value) })}
                size="small"
              />
              <TextField
                label="数量"
                type="number"
                value={newLine.orderQty}
                onChange={(e) => setNewLine({ ...newLine, orderQty: Number(e.target.value) })}
                size="small"
              />
              <IconButton color="primary" onClick={handleAddLine}>
                <AddIcon />
              </IconButton>
            </Box>
          </Box>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDialogOpen(false)}>キャンセル</Button>
          <Button onClick={handleSubmit} variant="contained" disabled={lines.length === 0}>
            作成
          </Button>
        </DialogActions>
      </Dialog>
    </Container>
  );
}
