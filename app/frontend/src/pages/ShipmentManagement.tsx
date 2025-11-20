import { useState } from 'react';
import {
  Box,
  Button,
  Card,
  CardContent,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Tab,
  Tabs,
  TextField,
  Typography,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  Alert,
} from '@mui/material';
import {
  useGetApiShipmentsPickingList,
  usePostApiShipmentsIdConfirmPicking,
  usePostApiShipments,
  usePostApiShipmentsReturns,
} from '../api/generated/default/default';

interface TabPanelProps {
  children?: React.ReactNode;
  index: number;
  value: number;
}

function TabPanel(props: TabPanelProps) {
  const { children, value, index, ...other } = props;
  return (
    <div hidden={value !== index} {...other}>
      {value === index && <Box sx={{ p: 3 }}>{children}</Box>}
    </div>
  );
}

export default function ShipmentManagement() {
  const [tabValue, setTabValue] = useState(0);
  const [pickingDialogOpen, setPickingDialogOpen] = useState(false);
  const [shipmentDialogOpen, setShipmentDialogOpen] = useState(false);
  const [returnDialogOpen, setReturnDialogOpen] = useState(false);
  const [receivedOrderId, setReceivedOrderId] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<string | null>(null);

  const { data: pickingList, refetch: refetchPickingList } = useGetApiShipmentsPickingList(
    { receivedOrderId: parseInt(receivedOrderId) || 0 },
    { query: { enabled: false } }
  );

  const confirmPickingMutation = usePostApiShipmentsIdConfirmPicking();
  const createShipmentMutation = usePostApiShipments();
  const processReturnMutation = usePostApiShipmentsReturns();

  const handleTabChange = (_event: React.SyntheticEvent, newValue: number) => {
    setTabValue(newValue);
    setError(null);
    setSuccess(null);
  };

  const handleGeneratePickingList = async () => {
    try {
      setError(null);
      await refetchPickingList();
    } catch (err) {
      setError('ピッキングリストの生成に失敗しました');
    }
  };

  const handleConfirmPicking = async () => {
    try {
      setError(null);
      await confirmPickingMutation.mutateAsync({ id: parseInt(receivedOrderId) });
      setSuccess('ピッキングが確認されました');
      setPickingDialogOpen(false);
      setReceivedOrderId('');
    } catch (err) {
      setError('ピッキング確認に失敗しました');
    }
  };

  const handleCreateShipment = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    const formData = new FormData(event.currentTarget);
    
    try {
      setError(null);
      await createShipmentMutation.mutateAsync({
        data: {
          receivedOrderId: parseInt(formData.get('receivedOrderId') as string),
          shipmentDate: formData.get('shipmentDate') as string,
          carrier: formData.get('carrier') as string || undefined,
          trackingNumber: formData.get('trackingNumber') as string || undefined,
        },
      });
      setSuccess('出荷が登録されました');
      setShipmentDialogOpen(false);
    } catch (err) {
      setError('出荷登録に失敗しました');
    }
  };

  const handleProcessReturn = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    const formData = new FormData(event.currentTarget);
    
    try {
      setError(null);
      await processReturnMutation.mutateAsync({
        data: {
          orderId: parseInt(formData.get('orderId') as string),
          returnDate: formData.get('returnDate') as string,
          reason: formData.get('reason') as string || undefined,
          refundAmount: parseInt(formData.get('refundAmount') as string),
        },
      });
      setSuccess('返品が登録されました');
      setReturnDialogOpen(false);
    } catch (err) {
      setError('返品登録に失敗しました');
    }
  };

  return (
    <Box sx={{ width: '100%' }}>
      <Typography variant="h4" gutterBottom>
        出荷管理
      </Typography>

      {error && (
        <Alert severity="error" sx={{ mb: 2 }} onClose={() => setError(null)}>
          {error}
        </Alert>
      )}

      {success && (
        <Alert severity="success" sx={{ mb: 2 }} onClose={() => setSuccess(null)}>
          {success}
        </Alert>
      )}

      <Box sx={{ borderBottom: 1, borderColor: 'divider' }}>
        <Tabs value={tabValue} onChange={handleTabChange}>
          <Tab label="ピッキング" />
          <Tab label="出荷" />
          <Tab label="返品" />
        </Tabs>
      </Box>

      {/* Picking Tab */}
      <TabPanel value={tabValue} index={0}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              ピッキングリスト生成
            </Typography>
            <Box sx={{ display: 'flex', gap: 2, mb: 2 }}>
              <TextField
                label="受注番号"
                type="number"
                value={receivedOrderId}
                onChange={(e) => setReceivedOrderId(e.target.value)}
                size="small"
              />
              <Button
                variant="contained"
                onClick={() => setPickingDialogOpen(true)}
                disabled={!receivedOrderId}
              >
                ピッキングリスト生成
              </Button>
            </Box>
          </CardContent>
        </Card>
      </TabPanel>

      {/* Shipment Tab */}
      <TabPanel value={tabValue} index={1}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              出荷登録
            </Typography>
            <Button
              variant="contained"
              onClick={() => setShipmentDialogOpen(true)}
            >
              新規出荷
            </Button>
          </CardContent>
        </Card>
      </TabPanel>

      {/* Return Tab */}
      <TabPanel value={tabValue} index={2}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              返品処理
            </Typography>
            <Button
              variant="contained"
              onClick={() => setReturnDialogOpen(true)}
            >
              返品登録
            </Button>
          </CardContent>
        </Card>
      </TabPanel>

      {/* Picking Dialog */}
      <Dialog
        open={pickingDialogOpen}
        onClose={() => setPickingDialogOpen(false)}
        maxWidth="md"
        fullWidth
      >
        <DialogTitle>ピッキングリスト</DialogTitle>
        <DialogContent>
          {!pickingList ? (
            <Box sx={{ py: 2 }}>
              <Button variant="contained" onClick={handleGeneratePickingList}>
                リスト生成
              </Button>
            </Box>
          ) : (
            <TableContainer component={Paper}>
              <Table>
                <TableHead>
                  <TableRow>
                    <TableCell>アイテム名</TableCell>
                    <TableCell align="right">必要数量</TableCell>
                    <TableCell>ロット番号</TableCell>
                  </TableRow>
                </TableHead>
                <TableBody>
                  {pickingList.items?.map((item) => (
                    <TableRow key={item.itemId}>
                      <TableCell>{item.itemName}</TableCell>
                      <TableCell align="right">{item.requiredQuantity}</TableCell>
                      <TableCell>{item.lotNumber}</TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </TableContainer>
          )}
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setPickingDialogOpen(false)}>キャンセル</Button>
          {pickingList && (
            <Button onClick={handleConfirmPicking} variant="contained">
              ピッキング確認
            </Button>
          )}
        </DialogActions>
      </Dialog>

      {/* Shipment Dialog */}
      <Dialog
        open={shipmentDialogOpen}
        onClose={() => setShipmentDialogOpen(false)}
        maxWidth="sm"
        fullWidth
      >
        <form onSubmit={handleCreateShipment}>
          <DialogTitle>出荷登録</DialogTitle>
          <DialogContent>
            <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2, pt: 1 }}>
              <TextField
                name="receivedOrderId"
                label="受注番号"
                type="number"
                required
                fullWidth
              />
              <TextField
                name="shipmentDate"
                label="出荷日"
                type="date"
                required
                fullWidth
                InputLabelProps={{ shrink: true }}
              />
              <TextField
                name="carrier"
                label="配送業者"
                fullWidth
              />
              <TextField
                name="trackingNumber"
                label="追跡番号"
                fullWidth
              />
            </Box>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => setShipmentDialogOpen(false)}>キャンセル</Button>
            <Button type="submit" variant="contained">
              登録
            </Button>
          </DialogActions>
        </form>
      </Dialog>

      {/* Return Dialog */}
      <Dialog
        open={returnDialogOpen}
        onClose={() => setReturnDialogOpen(false)}
        maxWidth="sm"
        fullWidth
      >
        <form onSubmit={handleProcessReturn}>
          <DialogTitle>返品登録</DialogTitle>
          <DialogContent>
            <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2, pt: 1 }}>
              <TextField
                name="orderId"
                label="注文番号"
                type="number"
                required
                fullWidth
              />
              <TextField
                name="returnDate"
                label="返品日"
                type="date"
                required
                fullWidth
                InputLabelProps={{ shrink: true }}
              />
              <TextField
                name="reason"
                label="返品理由"
                multiline
                rows={3}
                fullWidth
              />
              <TextField
                name="refundAmount"
                label="返金額"
                type="number"
                required
                fullWidth
              />
            </Box>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => setReturnDialogOpen(false)}>キャンセル</Button>
            <Button type="submit" variant="contained">
              登録
            </Button>
          </DialogActions>
        </form>
      </Dialog>
    </Box>
  );
}
