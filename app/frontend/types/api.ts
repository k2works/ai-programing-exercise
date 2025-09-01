// 会議室関連の型定義
export interface MeetingRoom {
  roomId: number;
  roomName: string;
}

export interface ReservableRoom {
  roomId: number;
  reservableDate: string; // ISO date string
}

// 予約関連の型定義
export interface Reservation {
  reservationId?: number;
  startTime: string; // HH:mm format
  endTime: string; // HH:mm format
  reservableRoom: ReservableRoom;
  userName?: string;
  userId?: string;
}

export interface ReservationRequest {
  roomId: number;
  reservableDate: string; // ISO date string
  startTime: string; // HH:mm format
  endTime: string; // HH:mm format
}

// 認証関連の型定義
export interface LoginRequest {
  userId: string;
  password: string;
}

export interface LoginResponse {
  token: string;
  userId: string;
  userName: string;
}

// エラーレスポンス
export interface ErrorResponse {
  message: string;
  details?: string[];
}