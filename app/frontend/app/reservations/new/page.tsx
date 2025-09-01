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
    <div className="min-h-screen bg-gray-50 py-12">
      <div className="container mx-auto px-4">
        <h1 className="text-3xl font-bold text-center mb-8">新規予約</h1>
        <ReservationForm onSuccess={handleSuccess} />
      </div>
    </div>
  );
}