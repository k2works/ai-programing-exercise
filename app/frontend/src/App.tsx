import { Routes, Route } from 'react-router-dom';
import { Container, Typography, Box, Button, AppBar, Toolbar } from '@mui/material';
import { AuthProvider, useAuth } from './contexts/AuthContext';
import { LoginPage } from './pages/LoginPage';
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
              Bouquet
            </Typography>
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
