import { Routes, Route, Link } from 'react-router-dom';
import { Container, Typography, Box, Button, AppBar, Toolbar } from '@mui/material';
import { AuthProvider, useAuth } from './contexts/AuthContext';
import { LoginPage } from './pages/LoginPage';
import { ProductList } from './pages/ProductList';
import { ProductDetail } from './pages/ProductDetail';
import { Cart } from './pages/Cart';
import { ProtectedRoute } from './components/ProtectedRoute';

function Home() {
  const { user, logout } = useAuth();

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
      </Box>
      <Button variant="outlined" onClick={logout} sx={{ mt: 2 }}>
        ログアウト
      </Button>
    </Box>
  );
}

function AppContent() {
  const { isAuthenticated } = useAuth();

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
        </Routes>
      </Container>
    </>
  );
}

function App() {
  return (
    <AuthProvider>
      <AppContent />
    </AuthProvider>
  );
}

export default App;
