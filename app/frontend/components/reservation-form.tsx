'use client';

import { useState, useEffect } from 'react';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import { format, addDays } from 'date-fns';
import { reservationService } from '@/services/reservation-service';
import { MeetingRoom } from '@/types/api';

// バリデーションスキーマ
const reservationSchema = z.object({
  roomId: z.number().min(1, '会議室を選択してください'),
  reservableDate: z.string().min(1, '予約日を選択してください'),
  startTime: z.string().regex(/^([01]\d|2[0-3]):([0-5]\d)$/, '正しい時刻形式を入力してください'),
  endTime: z.string().regex(/^([01]\d|2[0-3]):([0-5]\d)$/, '正しい時刻形式を入力してください'),
}).refine((data) => {
  return data.startTime < data.endTime;
}, {
  message: '終了時間は開始時間より後である必要があります',
  path: ['endTime'],
});

type ReservationFormData = z.infer<typeof reservationSchema>;

interface ReservationFormProps {
  onSuccess?: () => void;
}

export default function ReservationForm({ onSuccess }: ReservationFormProps) {
  const [rooms, setRooms] = useState<MeetingRoom[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const {
    register,
    handleSubmit,
    formState: { errors },
    reset,
  } = useForm<ReservationFormData>({
    resolver: zodResolver(reservationSchema),
    defaultValues: {
      reservableDate: format(addDays(new Date(), 1), 'yyyy-MM-dd'),
      startTime: '09:00',
      endTime: '10:00',
    },
  });

  useEffect(() => {
    loadRooms();
  }, []);

  const loadRooms = async () => {
    try {
      const data = await reservationService.getRooms();
      setRooms(data);
    } catch (err) {
      setError('会議室の取得に失敗しました');
      console.error(err);
    }
  };

  const onSubmit = async (data: ReservationFormData) => {
    setLoading(true);
    setError(null);
    
    try {
      await reservationService.createReservation(data);
      reset();
      if (onSuccess) {
        onSuccess();
      }
      alert('予約を作成しました');
    } catch (err) {
      const error = err as { response?: { data?: { message?: string } } };
      setError(error.response?.data?.message || '予約の作成に失敗しました');
    } finally {
      setLoading(false);
    }
  };

  // 時間オプション生成
  const timeOptions = [];
  for (let hour = 9; hour <= 21; hour++) {
    for (let minute = 0; minute < 60; minute += 30) {
      const time = `${hour.toString().padStart(2, '0')}:${minute.toString().padStart(2, '0')}`;
      timeOptions.push(time);
    }
  }

  return (
    <div className="max-w-md mx-auto p-6 bg-white rounded-lg shadow-md">
      <h2 className="text-2xl font-bold mb-6">会議室予約</h2>
      
      {error && (
        <div className="mb-4 p-3 bg-red-100 border border-red-400 text-red-700 rounded">
          {error}
        </div>
      )}

      <form onSubmit={handleSubmit(onSubmit)} className="space-y-4">
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            会議室
          </label>
          <select
            {...register('roomId', { valueAsNumber: true })}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            disabled={loading}
          >
            <option value="">選択してください</option>
            {rooms.map((room) => (
              <option key={room.roomId} value={room.roomId}>
                {room.roomName}
              </option>
            ))}
          </select>
          {errors.roomId && (
            <p className="mt-1 text-sm text-red-600">{errors.roomId.message}</p>
          )}
        </div>

        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            予約日
          </label>
          <input
            type="date"
            {...register('reservableDate')}
            min={format(addDays(new Date(), 1), 'yyyy-MM-dd')}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
            disabled={loading}
          />
          {errors.reservableDate && (
            <p className="mt-1 text-sm text-red-600">{errors.reservableDate.message}</p>
          )}
        </div>

        <div className="grid grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              開始時間
            </label>
            <select
              {...register('startTime')}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              disabled={loading}
            >
              {timeOptions.map((time) => (
                <option key={time} value={time}>
                  {time}
                </option>
              ))}
            </select>
            {errors.startTime && (
              <p className="mt-1 text-sm text-red-600">{errors.startTime.message}</p>
            )}
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">
              終了時間
            </label>
            <select
              {...register('endTime')}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              disabled={loading}
            >
              {timeOptions.map((time) => (
                <option key={time} value={time}>
                  {time}
                </option>
              ))}
            </select>
            {errors.endTime && (
              <p className="mt-1 text-sm text-red-600">{errors.endTime.message}</p>
            )}
          </div>
        </div>

        <button
          type="submit"
          disabled={loading}
          className="w-full py-2 px-4 bg-blue-600 hover:bg-blue-700 disabled:bg-gray-400 text-white font-medium rounded-md transition duration-200"
        >
          {loading ? '予約中...' : '予約する'}
        </button>
      </form>
    </div>
  );
}