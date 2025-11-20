import { useState } from 'react';
import {
  Container,
  Typography,
  Box,
  Tabs,
  Tab,
  Paper,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Button,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  Grid,
  Chip,
} from '@mui/material';
import { useGetApiItems } from '../api/generated/items/items';
import {
  usePostApiItems,
  usePutApiItemsId,
  usePostApiSuppliers,
  usePatchApiSuppliersIdActivate,
  usePatchApiSuppliersIdDeactivate,
} from '../api/generated/inventory/inventory';

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

export function InventoryManagement() {
  const [tabValue, setTabValue] = useState(0);
  const [dialogOpen, setDialogOpen] = useState(false);
  const [supplierDialogOpen, setSupplierDialogOpen] = useState(false);
  const [editingItem, setEditingItem] = useState<any>(null);
  const { data: items, refetch } = useGetApiItems();
  const { mutateAsync: createItem } = usePostApiItems();
  const { mutateAsync: updateItem } = usePutApiItemsId();
  const { mutateAsync: createSupplier } = usePostApiSuppliers();
  const { mutateAsync: activateSupplier } = usePatchApiSuppliersIdActivate();
  const { mutateAsync: deactivateSupplier } = usePatchApiSuppliersIdDeactivate();

  // Mock suppliers data - in real app, fetch from API
  const [suppliers, setSuppliers] = useState([
    { id: 1, code: 'SUP001', name: 'Supplier A', status: 'active' },
    { id: 2, code: 'SUP002', name: 'Supplier B', status: 'inactive' },
  ]);

  const [formData, setFormData] = useState({
    id: 0,
    code: '',
    name: '',
    supplierId: 1,
    qualityDays: 7,
    leadTime: 3,
    purchaseUnitQty: 10,
    purchasePrice: 100,
  });

  const [supplierFormData, setSupplierFormData] = useState({
    id: 0,
    code: '',
    name: '',
    phone: '',
    email: '',
  });

  const handleCreate = () => {
    setEditingItem(null);
    setFormData({
      id: 0,
      code: '',
      name: '',
      supplierId: 1,
      qualityDays: 7,
      leadTime: 3,
      purchaseUnitQty: 10,
      purchasePrice: 100,
    });
    setDialogOpen(true);
  };

  const handleEdit = (item: any) => {
    setEditingItem(item);
    setFormData({
      id: item.id,
      code: item.code,
      name: item.name,
      supplierId: item.supplierId,
      qualityDays: item.qualityDays,
      leadTime: item.leadTime,
      purchaseUnitQty: item.purchaseUnitQty,
      purchasePrice: item.purchasePrice,
    });
    setDialogOpen(true);
  };

  const handleSubmit = async () => {
    try {
      if (editingItem) {
        await updateItem({
          id: String(formData.id),
          data: {
            qualityDays: formData.qualityDays,
            leadTime: formData.leadTime,
            purchaseUnitQty: formData.purchaseUnitQty,
            purchasePrice: formData.purchasePrice,
          },
        });
        alert('単品を更新しました');
      } else {
        await createItem({
          data: formData,
        });
        alert('単品を登録しました');
      }
      setDialogOpen(false);
      refetch();
    } catch (error: any) {
      alert(error.response?.data?.error || '操作に失敗しました');
    }
  };

  const handleCreateSupplier = () => {
    setSupplierFormData({ id: 0, code: '', name: '', phone: '', email: '' });
    setSupplierDialogOpen(true);
  };

  const handleSubmitSupplier = async () => {
    try {
      await createSupplier({
        data: {
          id: supplierFormData.id,
          code: supplierFormData.code,
          name: supplierFormData.name,
          phone: supplierFormData.phone || null,
          email: supplierFormData.email || null,
        },
      });
      alert('仕入先を登録しました');
      setSupplierDialogOpen(false);
      // Refresh suppliers list
      setSuppliers([
        ...suppliers,
        { ...supplierFormData, status: 'active' },
      ]);
    } catch (error: any) {
      alert(error.response?.data?.error || '操作に失敗しました');
    }
  };

  const handleToggleSupplierStatus = async (id: number, currentStatus: string) => {
    try {
      if (currentStatus === 'active') {
        await deactivateSupplier({ id: String(id) });
        alert('仕入先を無効化しました');
      } else {
        await activateSupplier({ id: String(id) });
        alert('仕入先を有効化しました');
      }
      // Update local state
      setSuppliers(
        suppliers.map((s) =>
          s.id === id ? { ...s, status: currentStatus === 'active' ? 'inactive' : 'active' } : s
        )
      );
    } catch (error: any) {
      alert(error.response?.data?.error || '操作に失敗しました');
    }
  };

  return (
    <Container maxWidth="lg" sx={{ mt: 4, mb: 4 }}>
      <Typography variant="h4" component="h1" gutterBottom>
        在庫管理
      </Typography>

      <Box sx={{ borderBottom: 1, borderColor: 'divider' }}>
        <Tabs value={tabValue} onChange={(_, v) => setTabValue(v)}>
          <Tab label="単品管理" />
          <Tab label="仕入先管理" />
        </Tabs>
      </Box>

      <TabPanel value={tabValue} index={0}>
        <Box display="flex" justifyContent="flex-end" mb={2}>
          <Button variant="contained" onClick={handleCreate}>
            単品登録
          </Button>
        </Box>

        <TableContainer component={Paper}>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>ID</TableCell>
                <TableCell>コード</TableCell>
                <TableCell>名称</TableCell>
                <TableCell>仕入先ID</TableCell>
                <TableCell>品質維持日数</TableCell>
                <TableCell>リードタイム</TableCell>
                <TableCell>購入単位数量</TableCell>
                <TableCell>購入価格</TableCell>
                <TableCell align="center">操作</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {items?.map((item: any) => (
                <TableRow key={item.id}>
                  <TableCell>{item.id}</TableCell>
                  <TableCell>{item.code}</TableCell>
                  <TableCell>{item.name}</TableCell>
                  <TableCell>{item.supplierId}</TableCell>
                  <TableCell>{item.qualityDays}日</TableCell>
                  <TableCell>{item.leadTime}日</TableCell>
                  <TableCell>{item.purchaseUnitQty}</TableCell>
                  <TableCell>¥{item.purchasePrice}</TableCell>
                  <TableCell align="center">
                    <Button size="small" onClick={() => handleEdit(item)}>
                      編集
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      </TabPanel>

      <TabPanel value={tabValue} index={1}>
        <Box display="flex" justifyContent="flex-end" mb={2}>
          <Button variant="contained" onClick={handleCreateSupplier}>
            仕入先登録
          </Button>
        </Box>

        <TableContainer component={Paper}>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>ID</TableCell>
                <TableCell>コード</TableCell>
                <TableCell>名称</TableCell>
                <TableCell>ステータス</TableCell>
                <TableCell align="center">操作</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {suppliers.map((supplier) => (
                <TableRow key={supplier.id}>
                  <TableCell>{supplier.id}</TableCell>
                  <TableCell>{supplier.code}</TableCell>
                  <TableCell>{supplier.name}</TableCell>
                  <TableCell>
                    <Chip
                      label={supplier.status === 'active' ? '有効' : '無効'}
                      color={supplier.status === 'active' ? 'success' : 'default'}
                      size="small"
                    />
                  </TableCell>
                  <TableCell align="center">
                    <Button
                      size="small"
                      color={supplier.status === 'active' ? 'warning' : 'success'}
                      onClick={() => handleToggleSupplierStatus(supplier.id, supplier.status)}
                    >
                      {supplier.status === 'active' ? '無効化' : '有効化'}
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      </TabPanel>

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} maxWidth="sm" fullWidth>
        <DialogTitle>{editingItem ? '単品編集' : '単品登録'}</DialogTitle>
        <DialogContent>
          <Grid container spacing={2} sx={{ mt: 1 }}>
            {!editingItem && (
              <>
                <Grid item xs={12}>
                  <TextField
                    fullWidth
                    label="ID"
                    type="number"
                    value={formData.id}
                    onChange={(e) => setFormData({ ...formData, id: Number(e.target.value) })}
                    required
                  />
                </Grid>
                <Grid item xs={12}>
                  <TextField
                    fullWidth
                    label="コード"
                    value={formData.code}
                    onChange={(e) => setFormData({ ...formData, code: e.target.value })}
                    required
                  />
                </Grid>
                <Grid item xs={12}>
                  <TextField
                    fullWidth
                    label="名称"
                    value={formData.name}
                    onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                    required
                  />
                </Grid>
                <Grid item xs={12}>
                  <TextField
                    fullWidth
                    label="仕入先ID"
                    type="number"
                    value={formData.supplierId}
                    onChange={(e) =>
                      setFormData({ ...formData, supplierId: Number(e.target.value) })
                    }
                    required
                  />
                </Grid>
              </>
            )}
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="品質維持日数"
                type="number"
                value={formData.qualityDays}
                onChange={(e) =>
                  setFormData({ ...formData, qualityDays: Number(e.target.value) })
                }
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="リードタイム（日）"
                type="number"
                value={formData.leadTime}
                onChange={(e) => setFormData({ ...formData, leadTime: Number(e.target.value) })}
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="購入単位数量"
                type="number"
                value={formData.purchaseUnitQty}
                onChange={(e) =>
                  setFormData({ ...formData, purchaseUnitQty: Number(e.target.value) })
                }
                required
              />
            </Grid>
            <Grid item xs={6}>
              <TextField
                fullWidth
                label="購入価格"
                type="number"
                value={formData.purchasePrice}
                onChange={(e) =>
                  setFormData({ ...formData, purchasePrice: Number(e.target.value) })
                }
                required
              />
            </Grid>
          </Grid>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDialogOpen(false)}>キャンセル</Button>
          <Button onClick={handleSubmit} variant="contained">
            {editingItem ? '更新' : '登録'}
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={supplierDialogOpen}
        onClose={() => setSupplierDialogOpen(false)}
        maxWidth="sm"
        fullWidth
      >
        <DialogTitle>仕入先登録</DialogTitle>
        <DialogContent>
          <Grid container spacing={2} sx={{ mt: 1 }}>
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="ID"
                type="number"
                value={supplierFormData.id}
                onChange={(e) =>
                  setSupplierFormData({ ...supplierFormData, id: Number(e.target.value) })
                }
                required
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="コード"
                value={supplierFormData.code}
                onChange={(e) => setSupplierFormData({ ...supplierFormData, code: e.target.value })}
                required
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="名称"
                value={supplierFormData.name}
                onChange={(e) => setSupplierFormData({ ...supplierFormData, name: e.target.value })}
                required
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="電話番号"
                value={supplierFormData.phone}
                onChange={(e) =>
                  setSupplierFormData({ ...supplierFormData, phone: e.target.value })
                }
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="メールアドレス"
                type="email"
                value={supplierFormData.email}
                onChange={(e) =>
                  setSupplierFormData({ ...supplierFormData, email: e.target.value })
                }
              />
            </Grid>
          </Grid>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setSupplierDialogOpen(false)}>キャンセル</Button>
          <Button onClick={handleSubmitSupplier} variant="contained">
            登録
          </Button>
        </DialogActions>
      </Dialog>
    </Container>
  );
}
