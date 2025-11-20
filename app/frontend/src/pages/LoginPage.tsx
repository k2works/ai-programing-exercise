import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { Box, TextField, Button, Typography, Alert } from '@mui/material';
import { useAuth } from '../contexts/AuthContext';

export function LoginPage() {
  const [userId, setUserId] = useState('admin-001');
  const [password, setPassword] = useState('admin123');
  const [error, setError] = useState('');
  const { login } = useAuth();
  const navigate = useNavigate();

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');

    try {
      await login(userId, password);
      navigate('/');
    } catch (err) {
      setError('ログインに失敗しました。ユーザーIDとパスワードを確認してください。');
    }
  };

  return (
    <Box
      sx={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        minHeight: '100vh',
      }}
    >
      <Box sx={{ maxWidth: 400, width: '100%', p: 3 }}>
        <Typography variant="h4" component="h1" gutterBottom align="center">
          ログイン
        </Typography>

        {error && (
          <Alert severity="error" sx={{ mb: 2 }}>
            {error}
          </Alert>
        )}

        <form onSubmit={handleSubmit}>
          <TextField
            fullWidth
            label="ユーザーID"
            value={userId}
            onChange={(e) => setUserId(e.target.value)}
            margin="normal"
            required
          />
          <TextField
            fullWidth
            label="パスワード"
            type="password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            margin="normal"
            required
          />
          <Button
            type="submit"
            fullWidth
            variant="contained"
            sx={{ mt: 3 }}
          >
            ログイン
          </Button>
        </form>
      </Box>
    </Box>
  );
}
