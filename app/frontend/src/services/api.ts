import axios from 'axios';

// API ベース URL（環境に応じて設定）
const API_BASE_URL = process.env.REACT_APP_API_URL || 'http://localhost:5150/api';

// Axios インスタンスを作成
const api = axios.create({
  baseURL: API_BASE_URL,
  timeout: 10000,
  // HTTPS証明書エラーを回避するため、HTTPを使用
  validateStatus: function (status) {
    return status < 500; // 500未満のステータスコードを正常とみなす
  },
});

// レスポンスインターセプター（認証エラー処理）
api.interceptors.response.use(
  (response) => response,
  (error) => {
    // ログインエンドポイントの場合は自動ログアウトしない
    const isLoginEndpoint = error.config?.url?.includes('/auth/login');
    
    if (error.response?.status === 401 && !isLoginEndpoint) {
      // 認証エラーの場合はローカルストレージをクリア
      console.warn('認証エラーでログアウトします:', error.config?.url);
      localStorage.removeItem('accessToken');
      localStorage.removeItem('refreshToken');
      window.location.href = '/login';
    }
    return Promise.reject(error);
  }
);

// 型定義
export interface LoginRequest {
  userId: string;
  password: string;
}

export interface LoginResponse {
  accessToken: string;
  refreshToken: string;
  expiresAt: string;
  userInfo: {
    userId: string;
    name: string;
    role: string;
  };
}

export interface Room {
  roomId: string;
  roomName: string;
  capacity: number;
  isActive: boolean;
}

export interface ReservableRoom {
  reservableRoomId: string;
  roomId: string;
  roomName: string;
  isAvailable: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface Reservation {
  reservationId: string;
  roomId: string;
  userId: string;
  title: string;
  startTime: string;
  endTime: string;
  participants: string[];
  status: string;
  rowVersion: number;
  createdAt: string;
  updatedAt: string;
}

export interface CreateReservationRequest {
  roomId: string;
  title: string;
  startTime: string;
  endTime: string;
  participants?: string[];
}

export interface UpdateReservationRequest {
  title: string;
  startTime: string;
  endTime: string;
  participants?: string[];
}

// API サービス関数
export class ApiService {
  // 認証関連
  static async login(credentials: LoginRequest): Promise<LoginResponse> {
    const response = await api.post('/auth/login', credentials);
    const data = response.data;
    
    // ログイン成功時にトークンをデフォルトヘッダーに設定
    if (data.accessToken) {
      this.setAuthToken(data.accessToken);
    }
    
    return data;
  }

  static async logout(): Promise<void> {
    const token = localStorage.getItem('accessToken');
    if (token) {
      await api.post('/auth/logout', {}, {
        headers: { Authorization: `Bearer ${token}` }
      });
    }
    localStorage.removeItem('accessToken');
    localStorage.removeItem('refreshToken');
    // axios のデフォルトヘッダーからも削除
    this.removeAuthToken();
  }

  // 会議室関連
  static async getRooms(): Promise<Room[]> {
    const response = await api.get('/rooms');
    return response.data;
  }

  static async getRoomById(id: string): Promise<Room> {
    const response = await api.get(`/rooms/${id}`);
    return response.data;
  }

  static async getAvailableRooms(date?: string): Promise<ReservableRoom[]> {
    const params = date ? { date } : {};
    const response = await api.get('/rooms/available', { params });
    return response.data;
  }

  // 予約関連
  static async createReservation(request: CreateReservationRequest): Promise<Reservation> {
    const response = await api.post('/reservations', request);
    return response.data;
  }

  static async getReservations(roomId?: string, startDate?: string, endDate?: string): Promise<Reservation[]> {
    const params: any = {};
    if (roomId) params.roomId = roomId;
    if (startDate) params.startDate = startDate;
    if (endDate) params.endDate = endDate;
    
    const response = await api.get('/reservations', { params });
    return response.data;
  }

  static async getMyReservations(): Promise<Reservation[]> {
    const response = await api.get('/reservations/my');
    return response.data;
  }

  static async getReservationById(id: string): Promise<Reservation> {
    const response = await api.get(`/reservations/${id}`);
    return response.data;
  }

  static async updateReservation(
    id: string, 
    request: UpdateReservationRequest, 
    expectedRowVersion: number
  ): Promise<Reservation> {
    const response = await api.put(`/reservations/${id}`, request, {
      headers: {
        'If-Match': expectedRowVersion.toString()
      }
    });
    return response.data;
  }

  // トークン設定
  static setAuthToken(token: string): void {
    api.defaults.headers.common['Authorization'] = `Bearer ${token}`;
  }

  static removeAuthToken(): void {
    delete api.defaults.headers.common['Authorization'];
  }
}

export default api;