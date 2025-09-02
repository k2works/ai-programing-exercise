import { apiClient } from '@/lib/api-client';
import { 
  Reservation, 
  ReservationRequest, 
  MeetingRoom, 
  ReservableRoom 
} from '@/types/api';

export const reservationService = {
  // 会議室一覧取得
  async getRooms(): Promise<MeetingRoom[]> {
    const response = await apiClient.get('/rooms');
    return response.data;
  },

  // 指定日の予約可能会議室取得
  async getReservableRooms(date: string): Promise<ReservableRoom[]> {
    const response = await apiClient.get(`/rooms/${date}`);
    return response.data;
  },

  // 指定日の予約状況取得
  async getReservations(date: string, roomId?: number): Promise<Reservation[]> {
    const params = roomId ? { roomId } : {};
    const response = await apiClient.get(`/reservations/${date}`, { params });
    return response.data;
  },

  // 予約作成
  async createReservation(request: ReservationRequest): Promise<Reservation> {
    const response = await apiClient.post('/reservations', request);
    return response.data;
  },

  // 予約キャンセル
  async cancelReservation(reservationId: number): Promise<void> {
    await apiClient.delete(`/reservations/${reservationId}`);
  }
};