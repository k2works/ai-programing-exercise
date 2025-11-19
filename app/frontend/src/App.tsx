import { Routes, Route } from 'react-router-dom';
import { Container, Typography, Box } from '@mui/material';

function Home() {
  return (
    <Box sx={{ mt: 4 }}>
      <Typography variant="h3" component="h1" gutterBottom>
        Bouquet
      </Typography>
      <Typography variant="body1">フロントエンド基盤のセットアップが完了しました。</Typography>
    </Box>
  );
}

function App() {
  return (
    <Container maxWidth="lg">
      <Routes>
        <Route path="/" element={<Home />} />
      </Routes>
    </Container>
  );
}

export default App;
