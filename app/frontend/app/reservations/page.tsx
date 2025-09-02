'use client';

import { Suspense } from 'react';
import { useSearchParams } from 'next/navigation';
import Link from 'next/link';
import dynamic from 'next/dynamic';

const ReservationList = dynamic(() => import('@/components/reservation-list'), {
  ssr: false,
  loading: () => <div className="p-8 text-center">読み込み中...</div>
});

function ReservationPageContent() {
  const searchParams = useSearchParams();
  const date = searchParams.get('date') || undefined;
  const roomId = searchParams.get('roomId');

  return (
    <div className="min-h-screen bg-gray-50 py-8">
      <div className="container mx-auto px-4">
        {/* ナビゲーション */}
        <div className="mb-6 flex justify-between items-center">
          <nav className="flex space-x-4">
            <Link 
              href="/" 
              className="text-blue-600 hover:text-blue-800 font-medium"
            >
              ホーム
            </Link>
            <span className="text-gray-400">/</span>
            <span className="text-gray-700 font-medium">予約状況</span>
          </nav>
          
          <Link
            href="/reservations/new"
            className="px-6 py-2 bg-blue-600 hover:bg-blue-700 text-white font-semibold rounded-lg transition duration-200 shadow-md hover:shadow-lg"
          >
            新規予約
          </Link>
        </div>

        {/* 予約リストコンポーネント */}
        <ReservationList date={date} roomId={roomId ? Number(roomId) : undefined} />
      </div>
    </div>
  );
}

export default function ReservationPage() {
  return (
    <Suspense fallback={<div className="p-8 text-center">読み込み中...</div>}>
      <ReservationPageContent />
    </Suspense>
  );
}