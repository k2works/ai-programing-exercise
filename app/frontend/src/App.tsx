import React, { useState, useEffect } from 'react';
import Login from './components/Login';
import RoomList from './components/RoomList';
import { ApiService } from './services/api';
import './App.css';

interface UserInfo {
  userId: string;
  name: string;
  role: string;
}

function App() {
  const [isLoggedIn, setIsLoggedIn] = useState(false);
  const [userInfo, setUserInfo] = useState<UserInfo | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  // 初期化時にローカルストレージからトークンを確認
  useEffect(() => {
    const token = localStorage.getItem('accessToken');
    if (token) {
      ApiService.setAuthToken(token);
      // 実際のアプリではトークンの有効性を確認するAPI呼び出しを行う
      setIsLoggedIn(true);
      // ユーザー情報をローカルストレージから復元（簡単な実装）
      const savedUserInfo = localStorage.getItem('userInfo');
      if (savedUserInfo) {
        setUserInfo(JSON.parse(savedUserInfo));
      }
    }
    setIsLoading(false);
  }, []);

  const handleLogin = (token: string, userInfo: UserInfo) => {
    setIsLoggedIn(true);
    setUserInfo(userInfo);
    // ユーザー情報をローカルストレージに保存
    localStorage.setItem('userInfo', JSON.stringify(userInfo));
  };

  const handleLogout = () => {
    setIsLoggedIn(false);
    setUserInfo(null);
    localStorage.removeItem('userInfo');
    ApiService.removeAuthToken();
  };

  if (isLoading) {
    return (
      <div style={{ 
        display: 'flex', 
        justifyContent: 'center', 
        alignItems: 'center', 
        height: '100vh',
        fontSize: '1.2rem',
        color: '#666'
      }}>
        読み込み中...
      </div>
    );
  }

  return (
    <div className="App">
      {isLoggedIn && userInfo ? (
        <RoomList userInfo={userInfo} onLogout={handleLogout} />
      ) : (
        <Login onLogin={handleLogin} />
      )}
    </div>
  );
}

export default App;
