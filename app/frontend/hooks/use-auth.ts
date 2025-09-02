'use client';

import { useState, useEffect } from 'react';

interface User {
  userId: string;
  name: string;
  role: string;
}

export function useAuth() {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    // トークンからユーザー情報を取得（簡易実装）
    const token = localStorage.getItem('auth_token');
    if (token) {
      try {
        // JWTトークンをデコード（実際の実装では適切なライブラリを使用）
        const payload = JSON.parse(atob(token.split('.')[1]));
        setUser({
          userId: payload.sub || 'user',
          name: payload.name || 'ユーザー',
          role: payload.role || 'USER'
        });
      } catch (error) {
        console.error('トークンの解析に失敗しました:', error);
        // フォールバック：デフォルトユーザー情報を設定
        setUser({
          userId: 'user',
          name: 'ユーザー',
          role: 'USER'
        });
      }
    }
    setLoading(false);
  }, []);

  const isAdmin = user?.role === 'ADMIN';
  const canCancelReservation = (reservationUserId: string) => {
    return isAdmin || user?.userId === reservationUserId;
  };

  return {
    user,
    loading,
    isAdmin,
    canCancelReservation
  };
}