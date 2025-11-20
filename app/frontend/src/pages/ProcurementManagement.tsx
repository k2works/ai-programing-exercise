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
  Radio,
  RadioGroup,
  FormControlLabel,
  FormLabel,
} from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import AddIcon from '@mui/icons-material/Add';
import {
  usePostApiPlacementOrders,
  useDeleteApiPlacementOrdersId,
  usePostApiArrivals,
  usePostApiArrivalsIdInspect,
} from '../api/generated/procurement/procurement';

export function ProcurementManagement() {
  const [dialogOpen, setDialogOpen] = useState(false);
  const [arrivalDialogOpen, setArrivalDialogOpen] = useState(false);
  const [inspectDialogOpen, setInspectDialogOpen] = useState(false);
  const { mutateAsync: createOrder } = usePostApiPlacementOrders();
  const { mutateAsync: cancelOrder } = useDeleteApiPlacementOrdersId();
  const { mutateAsync: createArrival } = usePostApiArrivals();
  const { mutateAsync: inspectArrival } = usePostApiArrivalsIdInspect();

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

  // Mock arrivals data
  const [arrivals, setArrivals] = useState<any[]>([]);

  const [formData, setFormData] = useState({
    id: 0,
    orderDate: new Date().toISOString().split('T')[0],
    supplierId: 1,
    desiredDeliveryDate: '',
  });

  const [arrivalFormData, setArrivalFormData] = useState({
    id: 0,
    placementOrderId: 0,
    arrivalDate: new Date().toISOString().split('T')[0],
  });

  const [lines, setLines] = useState<Array<{ itemId: number; orderQty: number }>>([]);
  const [newLine, setNewLine] = useState({ itemId: 0, orderQty: 10 });

  const [arrivalLines, setArrivalLines] = useState<
    Array<{ placementOrderLineId: number; itemId: number; arrivedQty: number }>
  >([]);
  const [newArrivalLine, setNewArrivalLine] = useState({
    placementOrderLineId: 0,
    itemId: 0,
    arrivedQty: 0,
  });

  const [inspections, setInspections] = useState<
    Array<{ lineId: number; result: 'accepted' | 'rejected' }>
  >([]);
  const [selectedArrival, setSelectedArrival] = useState<any>(null);

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

  const handleOpenArrival = (orderId: number) => {
    setArrivalFormData({
      id: 0,
      placementOrderId: orderId,
      arrivalDate: new Date().toISOString().split('T')[0],
    });
    setArrivalLines([]);
    setArrivalDialogOpen(true);
  };

  const handleAddArrivalLine = () => {
    if (newArrivalLine.itemId > 0 && newArrivalLine.arrivedQty > 0) {
      setArrivalLines([...arrivalLines, newArrivalLine]);
      setNewArrivalLine({ placementOrderLineId: 0, itemId: 0, arrivedQty: 0 });
    }
  };

  const handleRemoveArrivalLine = (index: number) => {
    setArrivalLines(arrivalLines.filter((_, i) => i !== index));
  };

  const handleSubmitArrival = async () => {
    try {
      await createArrival({
        data: {
          id: arrivalFormData.id,
          placementOrderId: arrivalFormData.placementOrderId,
          arrivalDate: arrivalFormData.arrivalDate,
          lines: arrivalLines,
        },
      });
      alert('入荷を記録しました');
      setArrivalDialogOpen(false);
      setArrivals([...arrivals, { ...arrivalFormData, lines: arrivalLines, status: 'uninspected' }]);
    } catch (error: any) {
      alert(error.response?.data?.error || '入荷記録に失敗しました');
    }
  };

  const handleOpenInspect = (arrival: any) => {
    setSelectedArrival(arrival);
    setInspections(arrival.lines.map((l: any, i: number) => ({ lineId: i + 1, result: 'accepted' })));
    setInspectDialogOpen(true);
  };

  const handleSubmitInspect = async () => {
    try {
      await inspectArrival({
        id: String(selectedArrival.id),
        data: { lineInspections: inspections },
      });
      alert('検収を完了しました');
      setInspectDialogOpen(false);
      setArrivals(
        arrivals.map((a) =>
          a.id === selectedArrival.id ? { ...a, status: 'inspected' } : a
        )
      );
    } catch (error: any) {
      alert(error.response?.data?.error || '検収に失敗しました');
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
                    <>
                      <Button size="small" onClick={() => handleOpenArrival(order.id)}>
                        入荷
                      </Button>
                      <Button size="small" color="error" onClick={() => handleCancel(order.id)}>
                        キャンセル
                      </Button>
                    </>
                  )}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>

      <Typography variant="h5" sx={{ mt: 4, mb: 2 }}>
        入荷一覧
      </Typography>
      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>入荷ID</TableCell>
              <TableCell>発注ID</TableCell>
              <TableCell>入荷日</TableCell>
              <TableCell>ステータス</TableCell>
              <TableCell align="center">操作</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {arrivals.map((arrival) => (
              <TableRow key={arrival.id}>
                <TableCell>{arrival.id}</TableCell>
                <TableCell>{arrival.placementOrderId}</TableCell>
                <TableCell>{new Date(arrival.arrivalDate).toLocaleDateString()}</TableCell>
                <TableCell>
                  <Chip
                    label={arrival.status === 'uninspected' ? '未検収' : '検収済'}
                    color={arrival.status === 'uninspected' ? 'warning' : 'success'}
                    size="small"
                  />
                </TableCell>
                <TableCell align="center">
                  {arrival.status === 'uninspected' && (
                    <Button size="small" onClick={() => handleOpenInspect(arrival)}>
                      検収
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

      <Dialog
        open={arrivalDialogOpen}
        onClose={() => setArrivalDialogOpen(false)}
        maxWidth="md"
        fullWidth
      >
        <DialogTitle>入荷記録</DialogTitle>
        <DialogContent>
          <Grid container spacing={2} sx={{ mt: 1 }}>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="入荷ID"
                type="number"
                value={arrivalFormData.id}
                onChange={(e) =>
                  setArrivalFormData({ ...arrivalFormData, id: Number(e.target.value) })
                }
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="入荷日"
                type="date"
                value={arrivalFormData.arrivalDate}
                onChange={(e) =>
                  setArrivalFormData({ ...arrivalFormData, arrivalDate: e.target.value })
                }
                required
                InputLabelProps={{ shrink: true }}
              />
            </Grid>
          </Grid>

          <Box sx={{ mt: 3 }}>
            <Typography variant="subtitle1" gutterBottom>
              入荷明細
            </Typography>
            {arrivalLines.map((line, index) => (
              <Box key={index} display="flex" alignItems="center" gap={2} mb={1}>
                <Typography>単品ID: {line.itemId}</Typography>
                <Typography>数量: {line.arrivedQty}</Typography>
                <IconButton size="small" onClick={() => handleRemoveArrivalLine(index)}>
                  <DeleteIcon />
                </IconButton>
              </Box>
            ))}

            <Box display="flex" alignItems="center" gap={2} mt={2}>
              <TextField
                label="発注明細ID"
                type="number"
                value={newArrivalLine.placementOrderLineId}
                onChange={(e) =>
                  setNewArrivalLine({
                    ...newArrivalLine,
                    placementOrderLineId: Number(e.target.value),
                  })
                }
                size="small"
              />
              <TextField
                label="単品ID"
                type="number"
                value={newArrivalLine.itemId}
                onChange={(e) =>
                  setNewArrivalLine({ ...newArrivalLine, itemId: Number(e.target.value) })
                }
                size="small"
              />
              <TextField
                label="数量"
                type="number"
                value={newArrivalLine.arrivedQty}
                onChange={(e) =>
                  setNewArrivalLine({ ...newArrivalLine, arrivedQty: Number(e.target.value) })
                }
                size="small"
              />
              <IconButton color="primary" onClick={handleAddArrivalLine}>
                <AddIcon />
              </IconButton>
            </Box>
          </Box>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setArrivalDialogOpen(false)}>キャンセル</Button>
          <Button onClick={handleSubmitArrival} variant="contained" disabled={arrivalLines.length === 0}>
            記録
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={inspectDialogOpen}
        onClose={() => setInspectDialogOpen(false)}
        maxWidth="sm"
        fullWidth
      >
        <DialogTitle>検収処理</DialogTitle>
        <DialogContent>
          <Box sx={{ mt: 2 }}>
            {inspections.map((inspection, index) => (
              <Box key={index} mb={2}>
                <FormLabel>明細 {inspection.lineId}</FormLabel>
                <RadioGroup
                  value={inspection.result}
                  onChange={(e) => {
                    const newInspections = [...inspections];
                    newInspections[index].result = e.target.value as 'accepted' | 'rejected';
                    setInspections(newInspections);
                  }}
                  row
                >
                  <FormControlLabel value="accepted" control={<Radio />} label="合格" />
                  <FormControlLabel value="rejected" control={<Radio />} label="不合格" />
                </RadioGroup>
              </Box>
            ))}
          </Box>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setInspectDialogOpen(false)}>キャンセル</Button>
          <Button onClick={handleSubmitInspect} variant="contained">
            検収完了
          </Button>
        </DialogActions>
      </Dialog>
    </Container>
  );
}
