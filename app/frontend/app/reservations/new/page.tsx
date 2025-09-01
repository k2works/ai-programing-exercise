'use client';

import { useRouter } from 'next/navigation';
import ReservationForm from '@/components/reservation-form';

export default function NewReservationPage() {
  const router = useRouter();

  const handleSuccess = () => {
    // 予約成功後、予約一覧ページへ遷移
    router.push('/reservations');
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 py-12">
      <div className="container mx-auto px-4">
        <h1 className="text-4xl font-bold text-center mb-12 text-gray-800">新規予約</h1>
        <ReservationForm onSuccess={handleSuccess} />
      </div>
    </div>
  );
}