import { apiClient, setAuthToken, clearAuthToken } from '@/lib/api-client';
import { LoginRequest, LoginResponse } from '@/types/api';

export const authService = {
  async login(credentials: LoginRequest): Promise<LoginResponse> {
    const response = await apiClient.post('/auth/login', credentials);
    const data = response.data;
    
    // トークンを保存
    setAuthToken(data.token);
    localStorage.setItem('auth_token', data.token);
    localStorage.setItem('user_id', data.userId);
    localStorage.setItem('user_name', data.userName);
    
    return data;
  },

  logout() {
    clearAuthToken();
    localStorage.removeItem('auth_token');
    localStorage.removeItem('user_id');
    localStorage.removeItem('user_name');
  },

  getStoredToken(): string | null {
    return localStorage.getItem('auth_token');
  },

  getStoredUser() {
    return {
      userId: localStorage.getItem('user_id'),
      userName: localStorage.getItem('user_name'),
    };
  },

  isAuthenticated(): boolean {
    return !!this.getStoredToken();
  }
};