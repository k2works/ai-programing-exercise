import React, { useState, useEffect } from 'react';
import { ApiService, Reservation } from '../services/api';
import './ReservationList.css';

interface ReservationListProps {
  userInfo: {
    userId: string;
    name: string;
    role: string;
  };
}

const ReservationList: React.FC<ReservationListProps> = ({ userInfo }) => {
  const [reservations, setReservations] = useState<Reservation[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [viewMode, setViewMode] = useState<'my' | 'all'>('my');

  useEffect(() => {
    loadReservations();
  }, [viewMode]); // eslint-disable-line react-hooks/exhaustive-deps

  const loadReservations = async () => {
    try {
      setIsLoading(true);
      setError(null);
      
      const data = viewMode === 'my' 
        ? await ApiService.getMyReservations()
        : await ApiService.getReservations();
      
      setReservations(Array.isArray(data) ? data : []);
    } catch (err: any) {
      setError(err.response?.data?.message || '予約情報の取得に失敗しました');
      setReservations([]);
    } finally {
      setIsLoading(false);
    }
  };

  const formatDateTime = (isoString: string) => {
    const date = new Date(isoString);
    return date.toLocaleString('ja-JP', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      timeZone: 'Asia/Tokyo'
    });
  };

  const formatDuration = (startTime: string, endTime: string) => {
    const start = new Date(startTime);
    const end = new Date(endTime);
    const durationMs = end.getTime() - start.getTime();
    const hours = Math.floor(durationMs / (1000 * 60 * 60));
    const minutes = Math.floor((durationMs % (1000 * 60 * 60)) / (1000 * 60));
    
    if (hours > 0 && minutes > 0) {
      return `${hours}時間${minutes}分`;
    } else if (hours > 0) {
      return `${hours}時間`;
    } else {
      return `${minutes}分`;
    }
  };

  const getStatusBadgeClass = (status: string) => {
    switch (status.toLowerCase()) {
      case 'confirmed':
        return 'status-confirmed';
      case 'cancelled':
        return 'status-cancelled';
      default:
        return 'status-default';
    }
  };

  const getStatusText = (status: string) => {
    switch (status.toLowerCase()) {
      case 'confirmed':
        return '確定';
      case 'cancelled':
        return 'キャンセル';
      default:
        return status;
    }
  };

  const isUpcoming = (startTime: string) => {
    return new Date(startTime) > new Date();
  };

  const isPast = (endTime: string) => {
    return new Date(endTime) < new Date();
  };

  const sortedReservations = [...reservations].sort((a, b) => {
    return new Date(b.startTime).getTime() - new Date(a.startTime).getTime();
  });

  if (isLoading) {
    return (
      <div className="reservation-list-container">
        <div className="loading">読み込み中...</div>
      </div>
    );
  }

  return (
    <div className="reservation-list-container">
      <div className="section-header">
        <h2>予約一覧</h2>
        <div className="view-controls">
          <button 
            className={`view-btn ${viewMode === 'my' ? 'active' : ''}`}
            onClick={() => setViewMode('my')}
          >
            マイ予約
          </button>
          <button 
            className={`view-btn ${viewMode === 'all' ? 'active' : ''}`}
            onClick={() => setViewMode('all')}
          >
            全予約
          </button>
          <button onClick={loadReservations} className="refresh-btn">
            更新
          </button>
        </div>
      </div>

      {error && (
        <div className="error-message">
          {error}
          <button onClick={loadReservations}>再試行</button>
        </div>
      )}

      <div className="reservations-list">
        {sortedReservations.length === 0 ? (
          <div className="no-reservations">
            {viewMode === 'my' ? '予約はありません。' : '予約が見つかりませんでした。'}
          </div>
        ) : (
          sortedReservations.map((reservation) => (
            <div 
              key={reservation.reservationId} 
              className={`reservation-card ${isPast(reservation.endTime) ? 'past' : ''} ${isUpcoming(reservation.startTime) ? 'upcoming' : ''}`}
            >
              <div className="reservation-header">
                <h3 className="reservation-title">{reservation.title}</h3>
                <div className={`status-badge ${getStatusBadgeClass(reservation.status)}`}>
                  {getStatusText(reservation.status)}
                </div>
              </div>
              
              <div className="reservation-details">
                <div className="detail-row">
                  <span className="label">会議室:</span>
                  <span className="value">{reservation.roomId}</span>
                </div>
                
                <div className="detail-row">
                  <span className="label">開始時刻:</span>
                  <span className="value">{formatDateTime(reservation.startTime)}</span>
                </div>
                
                <div className="detail-row">
                  <span className="label">終了時刻:</span>
                  <span className="value">{formatDateTime(reservation.endTime)}</span>
                </div>
                
                <div className="detail-row">
                  <span className="label">時間:</span>
                  <span className="value">{formatDuration(reservation.startTime, reservation.endTime)}</span>
                </div>
                
                {viewMode === 'all' && (
                  <div className="detail-row">
                    <span className="label">予約者:</span>
                    <span className="value">{reservation.userId}</span>
                  </div>
                )}
                
                {reservation.participants && reservation.participants.length > 0 && (
                  <div className="detail-row">
                    <span className="label">参加者:</span>
                    <span className="value">{reservation.participants.join(', ')}</span>
                  </div>
                )}
                
                <div className="detail-row">
                  <span className="label">作成日時:</span>
                  <span className="value">{formatDateTime(reservation.createdAt)}</span>
                </div>
              </div>
              
              {reservation.userId === userInfo.userId && isUpcoming(reservation.startTime) && reservation.status === 'confirmed' && (
                <div className="reservation-actions">
                  <button className="edit-btn" disabled>
                    編集（実装予定）
                  </button>
                </div>
              )}
            </div>
          ))
        )}
      </div>
    </div>
  );
};

export default ReservationList;