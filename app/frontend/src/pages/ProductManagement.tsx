import { useState } from 'react';
import { useNavigate } from 'react-router-dom';
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
  Chip,
  CircularProgress,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
  Grid,
  IconButton,
  List,
  ListItem,
  ListItemText,
  Select,
  MenuItem,
  FormControl,
  InputLabel,
} from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import AddIcon from '@mui/icons-material/Add';
import {
  useGetApiProducts,
  usePostApiProducts,
  usePutApiProductsId,
  usePatchApiProductsIdStopSales,
  usePatchApiProductsIdResumeSales,
  usePatchApiProductsIdEndSales,
  usePostApiProductsIdComposition,
  useGetApiProductsIdComposition,
} from '../api/generated/products/products';
import { useGetApiItems } from '../api/generated/items/items';

export function ProductManagement() {
  const navigate = useNavigate();
  const { data: products, isLoading, refetch } = useGetApiProducts();
  const { data: items } = useGetApiItems();
  const { mutateAsync: createProduct } = usePostApiProducts();
  const { mutateAsync: updateProduct } = usePutApiProductsId();
  const { mutateAsync: stopSales } = usePatchApiProductsIdStopSales();
  const { mutateAsync: resumeSales } = usePatchApiProductsIdResumeSales();
  const { mutateAsync: endSales } = usePatchApiProductsIdEndSales();
  const { mutateAsync: saveComposition } = usePostApiProductsIdComposition();

  const [dialogOpen, setDialogOpen] = useState(false);
  const [compositionDialogOpen, setCompositionDialogOpen] = useState(false);
  const [editingProduct, setEditingProduct] = useState<any>(null);
  const [selectedProductId, setSelectedProductId] = useState<number | null>(null);
  const [formData, setFormData] = useState({
    id: 0,
    code: '',
    name: '',
    salesPrice: 0,
  });
  const [compositionItems, setCompositionItems] = useState<
    Array<{ itemId: number; requiredQty: number }>
  >([]);
  const [newItemId, setNewItemId] = useState<number>(0);
  const [newItemQty, setNewItemQty] = useState<number>(1);

  const handleCreate = () => {
    setEditingProduct(null);
    setFormData({ id: 0, code: '', name: '', salesPrice: 0 });
    setDialogOpen(true);
  };

  const handleEdit = (product: any) => {
    setEditingProduct(product);
    setFormData({
      id: product.id,
      code: product.code,
      name: product.name,
      salesPrice: product.salesPrice,
    });
    setDialogOpen(true);
  };

  const handleSubmit = async () => {
    try {
      if (editingProduct) {
        await updateProduct({
          id: String(formData.id),
          data: {
            name: formData.name,
            salesPrice: formData.salesPrice,
          },
        });
        alert('商品を更新しました');
      } else {
        await createProduct({
          data: {
            id: formData.id,
            code: formData.code,
            name: formData.name,
            salesPrice: formData.salesPrice,
          },
        });
        alert('商品を登録しました');
      }
      setDialogOpen(false);
      refetch();
    } catch (error: any) {
      alert(error.response?.data?.message || '操作に失敗しました');
    }
  };

  const handleStopSales = async (id: number) => {
    if (!confirm('販売を停止しますか？')) return;
    try {
      await stopSales({ id: String(id) });
      alert('販売を停止しました');
      refetch();
    } catch (error: any) {
      alert(error.response?.data?.message || '操作に失敗しました');
    }
  };

  const handleResumeSales = async (id: number) => {
    if (!confirm('販売を再開しますか？')) return;
    try {
      await resumeSales({ id: String(id) });
      alert('販売を再開しました');
      refetch();
    } catch (error: any) {
      alert(error.response?.data?.message || '操作に失敗しました');
    }
  };

  const handleEndSales = async (id: number) => {
    if (!confirm('販売を終了しますか？この操作は取り消せません。')) return;
    try {
      await endSales({ id: String(id) });
      alert('販売を終了しました');
      refetch();
    } catch (error: any) {
      alert(error.response?.data?.message || '操作に失敗しました');
    }
  };

  const handleOpenComposition = async (productId: number) => {
    setSelectedProductId(productId);
    try {
      const response = await fetch(`http://localhost:3000/api/products/${productId}/composition`, {
        headers: {
          Authorization: `Bearer ${localStorage.getItem('token')}`,
        },
      });
      const data = await response.json();
      setCompositionItems(data);
    } catch (error) {
      setCompositionItems([]);
    }
    setCompositionDialogOpen(true);
  };

  const handleAddCompositionItem = () => {
    if (newItemId && newItemQty > 0) {
      setCompositionItems([...compositionItems, { itemId: newItemId, requiredQty: newItemQty }]);
      setNewItemId(0);
      setNewItemQty(1);
    }
  };

  const handleRemoveCompositionItem = (index: number) => {
    setCompositionItems(compositionItems.filter((_, i) => i !== index));
  };

  const handleSaveComposition = async () => {
    if (!selectedProductId) return;
    try {
      await saveComposition({
        id: String(selectedProductId),
        data: { items: compositionItems },
      });
      alert('単品を関連付けました');
      setCompositionDialogOpen(false);
    } catch (error: any) {
      alert(error.response?.data?.message || '操作に失敗しました');
    }
  };

  const getStatusLabel = (status: string) => {
    const statusMap: { [key: string]: { label: string; color: any } } = {
      on_sale: { label: '販売中', color: 'success' },
      stopped: { label: '停止中', color: 'warning' },
      ended: { label: '終了', color: 'error' },
    };
    return statusMap[status] || { label: status, color: 'default' };
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
      <Box display="flex" justifyContent="space-between" alignItems="center" mb={3}>
        <Typography variant="h4" component="h1">
          商品管理
        </Typography>
        <Button variant="contained" onClick={handleCreate}>
          新規登録
        </Button>
      </Box>

      <TableContainer component={Paper}>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell>ID</TableCell>
              <TableCell>商品コード</TableCell>
              <TableCell>商品名</TableCell>
              <TableCell align="right">販売価格</TableCell>
              <TableCell>ステータス</TableCell>
              <TableCell align="center">操作</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {products?.map((product: any) => {
              const statusInfo = getStatusLabel(product.salesStatus);
              return (
                <TableRow key={product.id}>
                  <TableCell>{product.id}</TableCell>
                  <TableCell>{product.code}</TableCell>
                  <TableCell>{product.name}</TableCell>
                  <TableCell align="right">¥{product.salesPrice.toLocaleString()}</TableCell>
                  <TableCell>
                    <Chip label={statusInfo.label} color={statusInfo.color} size="small" />
                  </TableCell>
                  <TableCell align="center">
                    <Box display="flex" gap={1} justifyContent="center">
                      <Button size="small" onClick={() => handleEdit(product)}>
                        編集
                      </Button>
                      <Button size="small" onClick={() => handleOpenComposition(product.id)}>
                        単品
                      </Button>
                      {product.salesStatus === 'on_sale' && (
                        <Button
                          size="small"
                          color="warning"
                          onClick={() => handleStopSales(product.id)}
                        >
                          停止
                        </Button>
                      )}
                      {product.salesStatus === 'stopped' && (
                        <>
                          <Button
                            size="small"
                            color="success"
                            onClick={() => handleResumeSales(product.id)}
                          >
                            再開
                          </Button>
                          <Button
                            size="small"
                            color="error"
                            onClick={() => handleEndSales(product.id)}
                          >
                            終了
                          </Button>
                        </>
                      )}
                    </Box>
                  </TableCell>
                </TableRow>
              );
            })}
          </TableBody>
        </Table>
      </TableContainer>

      <Dialog open={dialogOpen} onClose={() => setDialogOpen(false)} maxWidth="sm" fullWidth>
        <DialogTitle>{editingProduct ? '商品編集' : '商品登録'}</DialogTitle>
        <DialogContent>
          <Grid container spacing={2} sx={{ mt: 1 }}>
            {!editingProduct && (
              <>
                <Grid item xs={12}>
                  <TextField
                    fullWidth
                    label="商品ID"
                    type="number"
                    value={formData.id}
                    onChange={(e) => setFormData({ ...formData, id: Number(e.target.value) })}
                    required
                  />
                </Grid>
                <Grid item xs={12}>
                  <TextField
                    fullWidth
                    label="商品コード"
                    value={formData.code}
                    onChange={(e) => setFormData({ ...formData, code: e.target.value })}
                    required
                    inputProps={{ maxLength: 6 }}
                    helperText="6文字以内"
                  />
                </Grid>
              </>
            )}
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="商品名"
                value={formData.name}
                onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                required
              />
            </Grid>
            <Grid item xs={12}>
              <TextField
                fullWidth
                label="販売価格"
                type="number"
                value={formData.salesPrice}
                onChange={(e) => setFormData({ ...formData, salesPrice: Number(e.target.value) })}
                required
              />
            </Grid>
          </Grid>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setDialogOpen(false)}>キャンセル</Button>
          <Button
            onClick={handleSubmit}
            variant="contained"
            disabled={!formData.name || formData.salesPrice <= 0 || (!editingProduct && !formData.code)}
          >
            {editingProduct ? '更新' : '登録'}
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={compositionDialogOpen}
        onClose={() => setCompositionDialogOpen(false)}
        maxWidth="sm"
        fullWidth
      >
        <DialogTitle>単品関連付け</DialogTitle>
        <DialogContent>
          <Box sx={{ mt: 2 }}>
            <Typography variant="subtitle2" gutterBottom>
              関連付けられた単品
            </Typography>
            <List>
              {compositionItems.map((item, index) => {
                const itemData = items?.find((i: any) => i.id === item.itemId);
                return (
                  <ListItem
                    key={index}
                    secondaryAction={
                      <IconButton edge="end" onClick={() => handleRemoveCompositionItem(index)}>
                        <DeleteIcon />
                      </IconButton>
                    }
                  >
                    <ListItemText
                      primary={itemData?.name || `Item ${item.itemId}`}
                      secondary={`必要数量: ${item.requiredQty}`}
                    />
                  </ListItem>
                );
              })}
            </List>

            <Box sx={{ mt: 3 }}>
              <Typography variant="subtitle2" gutterBottom>
                単品を追加
              </Typography>
              <Grid container spacing={2} alignItems="center">
                <Grid item xs={6}>
                  <FormControl fullWidth>
                    <InputLabel>単品</InputLabel>
                    <Select
                      value={newItemId}
                      onChange={(e) => setNewItemId(Number(e.target.value))}
                      label="単品"
                    >
                      <MenuItem value={0}>選択してください</MenuItem>
                      {items?.map((item: any) => (
                        <MenuItem key={item.id} value={item.id}>
                          {item.name}
                        </MenuItem>
                      ))}
                    </Select>
                  </FormControl>
                </Grid>
                <Grid item xs={4}>
                  <TextField
                    fullWidth
                    label="必要数量"
                    type="number"
                    value={newItemQty}
                    onChange={(e) => setNewItemQty(Number(e.target.value))}
                    inputProps={{ min: 1 }}
                  />
                </Grid>
                <Grid item xs={2}>
                  <IconButton color="primary" onClick={handleAddCompositionItem}>
                    <AddIcon />
                  </IconButton>
                </Grid>
              </Grid>
            </Box>
          </Box>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setCompositionDialogOpen(false)}>キャンセル</Button>
          <Button onClick={handleSaveComposition} variant="contained">
            保存
          </Button>
        </DialogActions>
      </Dialog>
    </Container>
  );
}
