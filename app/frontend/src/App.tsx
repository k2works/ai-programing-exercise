import { Routes, Route, Link } from 'react-router-dom';
import { Container, Typography, Box, Button, AppBar, Toolbar } from '@mui/material';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { AuthProvider, useAuth } from './contexts/AuthContext';
import { LoginPage } from './pages/LoginPage';
import { ProductList } from './pages/ProductList';
import { ProductDetail } from './pages/ProductDetail';
import { Cart } from './pages/Cart';
import { Checkout } from './pages/Checkout';
import { OrderList } from './pages/OrderList';
import { OrderDetail } from './pages/OrderDetail';
import { ProductManagement } from './pages/ProductManagement';
import { ProtectedRoute } from './components/ProtectedRoute';

const queryClient = new QueryClient();

function Home() {
  const { user, logout } = useAuth();
  const isAdmin = user?.roleName === 'admin';

  return (
    <Box sx={{ mt: 4 }}>
      <Typography variant="h3" component="h1" gutterBottom>
        Bouquet
      </Typography>
      <Typography variant="body1" gutterBottom>
        ようこそ、{user?.lastName} {user?.firstName}さん
      </Typography>
      <Typography variant="body2" color="text.secondary">
        ロール: {user?.roleName} | タイプ: {user?.userType}
      </Typography>
      <Box sx={{ mt: 3, display: 'flex', gap: 2 }}>
        <Button variant="contained" component={Link} to="/products">
          商品を見る
        </Button>
        <Button variant="outlined" component={Link} to="/cart">
          カート
        </Button>
        <Button variant="outlined" component={Link} to="/orders">
          注文履歴
        </Button>
        {isAdmin && (
          <Button variant="outlined" color="secondary" component={Link} to="/admin/products">
            商品管理
          </Button>
        )}
      </Box>
      <Button variant="outlined" onClick={logout} sx={{ mt: 2 }}>
        ログアウト
      </Button>
    </Box>
  );
}

function AppContent() {
  const { isAuthenticated, user } = useAuth();
  const isAdmin = user?.roleName === 'admin';

  return (
    <>
      {isAuthenticated && (
        <AppBar position="static">
          <Toolbar>
            <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
              <Link to="/" style={{ color: 'inherit', textDecoration: 'none' }}>
                Bouquet
              </Link>
            </Typography>
            <Button color="inherit" component={Link} to="/products">
              商品
            </Button>
            <Button color="inherit" component={Link} to="/cart">
              カート
            </Button>
            <Button color="inherit" component={Link} to="/orders">
              注文
            </Button>
            {isAdmin && (
              <Button color="inherit" component={Link} to="/admin/products">
                管理
              </Button>
            )}
          </Toolbar>
        </AppBar>
      )}
      <Container maxWidth="lg">
        <Routes>
          <Route path="/login" element={<LoginPage />} />
          <Route
            path="/"
            element={
              <ProtectedRoute>
                <Home />
              </ProtectedRoute>
            }
          />
          <Route
            path="/products"
            element={
              <ProtectedRoute>
                <ProductList />
              </ProtectedRoute>
            }
          />
          <Route
            path="/products/:id"
            element={
              <ProtectedRoute>
                <ProductDetail />
              </ProtectedRoute>
            }
          />
          <Route
            path="/cart"
            element={
              <ProtectedRoute>
                <Cart />
              </ProtectedRoute>
            }
          />
          <Route
            path="/checkout"
            element={
              <ProtectedRoute>
                <Checkout />
              </ProtectedRoute>
            }
          />
          <Route
            path="/orders"
            element={
              <ProtectedRoute>
                <OrderList />
              </ProtectedRoute>
            }
          />
          <Route
            path="/orders/:id"
            element={
              <ProtectedRoute>
                <OrderDetail />
              </ProtectedRoute>
            }
          />
          <Route
            path="/admin/products"
            element={
              <ProtectedRoute>
                <ProductManagement />
              </ProtectedRoute>
            }
          />
        </Routes>
      </Container>
    </>
  );
}

function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <AuthProvider>
        <AppContent />
      </AuthProvider>
    </QueryClientProvider>
  );
}

export default App;
