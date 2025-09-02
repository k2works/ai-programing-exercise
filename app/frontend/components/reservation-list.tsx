'use client';

import { useState, useEffect, useCallback } from 'react';
import { format } from 'date-fns';
import { reservationService } from '@/services/reservation-service';
import { Reservation, MeetingRoom } from '@/types/api';

interface ReservationListProps {
  date?: string;
  roomId?: number;
}

export default function ReservationList({ date: initialDate, roomId }: ReservationListProps) {
  const [reservations, setReservations] = useState<Reservation[]>([]);
  const [rooms, setRooms] = useState<MeetingRoom[]>([]);
  const [selectedDate, setSelectedDate] = useState(
    initialDate || format(new Date(), 'yyyy-MM-dd')
  );
  const [selectedRoomId, setSelectedRoomId] = useState<number | undefined>(roomId);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const loadRooms = useCallback(async () => {
    try {
      const data = await reservationService.getRooms();
      setRooms(data);
    } catch (err) {
      console.error('会議室の取得に失敗しました:', err);
    }
  }, []);

  const loadReservations = useCallback(async () => {
    setLoading(true);
    setError(null);
    
    try {
      const data = await reservationService.getReservations(selectedDate, selectedRoomId);
      setReservations(data);
    } catch (err) {
      setError('予約情報の取得に失敗しました');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, [selectedDate, selectedRoomId]);

  useEffect(() => {
    loadRooms();
  }, [loadRooms]);

  useEffect(() => {
    loadReservations();
  }, [loadReservations]);

  const handleDateChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setSelectedDate(e.target.value);
  };

  const handleRoomChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const value = e.target.value;
    setSelectedRoomId(value ? Number(value) : undefined);
  };

  const formatTime = (time: string) => {
    return time.substring(0, 5); // HH:mm形式に変換
  };

  const getRoomName = (roomId: number) => {
    const room = rooms.find(r => r.roomId === roomId);
    return room ? room.roomName : `会議室 ${roomId}`;
  };

  return (
    <div className="max-w-6xl mx-auto p-8">
      <h2 className="text-3xl font-bold mb-8 text-gray-800">予約状況</h2>
      
      {/* フィルター */}
      <div className="bg-white rounded-xl shadow-lg border border-gray-200 p-6 mb-8">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-semibold text-gray-800 mb-2">
              予約日
            </label>
            <input
              type="date"
              value={selectedDate}
              onChange={handleDateChange}
              min={format(new Date(), 'yyyy-MM-dd')}
              className="w-full px-4 py-3 border-2 border-gray-300 rounded-lg focus:outline-none focus:border-blue-500 focus:ring-2 focus:ring-blue-200 text-gray-800 bg-white"
            />
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-800 mb-2">
              会議室
            </label>
            <select
              value={selectedRoomId || ''}
              onChange={handleRoomChange}
              className="w-full px-4 py-3 border-2 border-gray-300 rounded-lg focus:outline-none focus:border-blue-500 focus:ring-2 focus:ring-blue-200 text-gray-800 bg-white"
            >
              <option value="">すべての会議室</option>
              {rooms.map((room) => (
                <option key={room.roomId} value={room.roomId}>
                  {room.roomName}
                </option>
              ))}
            </select>
          </div>
        </div>
      </div>

      {/* エラー表示 */}
      {error && (
        <div className="mb-6 p-4 bg-red-50 border-l-4 border-red-500 text-red-800 rounded">
          <strong>エラー:</strong> {error}
        </div>
      )}

      {/* 予約リスト */}
      <div className="bg-white rounded-xl shadow-lg border border-gray-200 overflow-hidden">
        {loading ? (
          <div className="p-8 text-center text-gray-500">
            読み込み中...
          </div>
        ) : reservations.length === 0 ? (
          <div className="p-8 text-center text-gray-500">
            予約がありません
          </div>
        ) : (
          <div className="overflow-x-auto">
            <table className="min-w-full">
              <thead className="bg-gray-50 border-b-2 border-gray-200">
                <tr>
                  <th className="px-6 py-4 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider">
                    時間
                  </th>
                  <th className="px-6 py-4 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider">
                    会議室
                  </th>
                  <th className="px-6 py-4 text-left text-xs font-semibold text-gray-700 uppercase tracking-wider">
                    予約者
                  </th>
                </tr>
              </thead>
              <tbody className="divide-y divide-gray-200">
                {reservations.map((reservation) => (
                  <tr 
                    key={reservation.reservationId}
                    className="hover:bg-gray-50 transition-colors duration-200"
                  >
                    <td className="px-6 py-4 whitespace-nowrap">
                      <div className="flex items-center">
                        <div className="text-sm font-medium text-gray-900">
                          {formatTime(reservation.startTime)} - {formatTime(reservation.endTime)}
                        </div>
                      </div>
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap">
                      <div className="text-sm text-gray-900">
                        {getRoomName(reservation.reservableRoom.roomId)}
                      </div>
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap">
                      <div className="text-sm text-gray-900">
                        {reservation.userName}
                      </div>
                      <div className="text-xs text-gray-500">
                        ({reservation.userId})
                      </div>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>

      {/* 凡例 */}
      <div className="mt-6 text-sm text-gray-600">
        <p>※ 表示される予約は選択した日付と会議室に基づいています</p>
      </div>
    </div>
  );
}