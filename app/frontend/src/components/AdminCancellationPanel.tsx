import React, { useState, useEffect } from 'react';
import { ApiService, Reservation } from '../services/api';
import CancelReservationDialog from './CancelReservationDialog';
import CancellationHistory from './CancellationHistory';
import './AdminCancellationPanel.css';

interface AdminCancellationPanelProps {
  userInfo: {
    userId: string;
    name: string;
    role: string;
  };
  isOpen: boolean;
  onClose: () => void;
}

const AdminCancellationPanel: React.FC<AdminCancellationPanelProps> = ({
  userInfo,
  isOpen,
  onClose
}) => {
  const [reservations, setReservations] = useState<Reservation[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [statusFilter, setStatusFilter] = useState<'all' | 'confirmed' | 'cancelled'>('confirmed');
  const [cancelDialogOpen, setCancelDialogOpen] = useState(false);
  const [historyDialogOpen, setHistoryDialogOpen] = useState(false);
  const [selectedReservation, setSelectedReservation] = useState<Reservation | null>(null);

  useEffect(() => {
    if (isOpen) {
      loadReservations();
    }
  }, [isOpen]);

  const loadReservations = async () => {
    try {
      setIsLoading(true);
      setError(null);
      const data = await ApiService.getReservations();
      setReservations(Array.isArray(data) ? data : []);
    } catch (err: any) {
      setError(err.response?.data?.message || '予約情報の取得に失敗しました');
      setReservations([]);
    } finally {
      setIsLoading(false);
    }
  };

  const handleCancelClick = (reservation: Reservation) => {
    setSelectedReservation(reservation);
    setCancelDialogOpen(true);
  };

  const handleCancelDialogClose = () => {
    setCancelDialogOpen(false);
    setSelectedReservation(null);
  };

  const handleCancelSuccess = () => {
    loadReservations(); // 予約リストを再読み込み
  };

  const handleHistoryClick = (reservation?: Reservation) => {
    setSelectedReservation(reservation || null);
    setHistoryDialogOpen(true);
  };

  const handleHistoryClose = () => {
    setHistoryDialogOpen(false);
    setSelectedReservation(null);
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

  const getStatusClass = (status: string) => {
    switch (status.toLowerCase()) {
      case 'confirmed':
        return 'status-confirmed';
      case 'cancelled':
        return 'status-cancelled';
      default:
        return 'status-default';
    }
  };

  const isUpcoming = (startTime: string) => {
    return new Date(startTime) > new Date();
  };

  const canCancelReservation = (reservation: Reservation) => {
    return reservation.status === 'confirmed';
  };

  // フィルタリング処理
  const filteredReservations = reservations.filter(reservation => {
    const matchesSearch = searchTerm === '' || 
      reservation.title.toLowerCase().includes(searchTerm.toLowerCase()) ||
      reservation.userId.toLowerCase().includes(searchTerm.toLowerCase()) ||
      reservation.roomId.toLowerCase().includes(searchTerm.toLowerCase());
    
    const matchesStatus = statusFilter === 'all' || reservation.status === statusFilter;
    
    return matchesSearch && matchesStatus;
  });

  // 日時順でソート（最新順）
  const sortedReservations = [...filteredReservations].sort((a, b) => {
    return new Date(b.startTime).getTime() - new Date(a.startTime).getTime();
  });

  if (!isOpen) {
    return null;
  }

  if (userInfo.role !== 'Admin') {
    return (
      <div className="dialog-overlay" onClick={onClose}>
        <div className="dialog-content" onClick={e => e.stopPropagation()}>
          <div className="dialog-header">
            <h3>アクセス拒否</h3>
            <button className="dialog-close-btn" onClick={onClose}>×</button>
          </div>
          <div className="dialog-body">
            <div className="error-message">
              この機能は管理者のみ利用できます。
            </div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="dialog-overlay" onClick={onClose}>
      <div className="dialog-content admin-panel-dialog" onClick={e => e.stopPropagation()}>
        <div className="dialog-header">
          <h3>管理者キャンセル機能</h3>
          <button 
            type="button" 
            className="dialog-close-btn"
            onClick={onClose}
          >
            ×
          </button>
        </div>

        <div className="dialog-body">
          {/* 検索・フィルター */}
          <div className="admin-controls">
            <div className="search-container">
              <input
                type="text"
                placeholder="予約タイトル、ユーザーID、会議室IDで検索..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="search-input"
              />
            </div>
            
            <div className="filter-container">
              <select 
                value={statusFilter} 
                onChange={(e) => setStatusFilter(e.target.value as any)}
                className="status-filter"
              >
                <option value="all">すべてのステータス</option>
                <option value="confirmed">確定</option>
                <option value="cancelled">キャンセル</option>
              </select>
            </div>

            <div className="action-buttons">
              <button onClick={loadReservations} className="refresh-btn">
                更新
              </button>
              <button onClick={() => handleHistoryClick()} className="history-btn">
                全履歴表示
              </button>
            </div>
          </div>

          {isLoading && (
            <div className="loading">読み込み中...</div>
          )}

          {error && (
            <div className="error-message">
              {error}
              <button onClick={loadReservations}>再試行</button>
            </div>
          )}

          {!isLoading && !error && sortedReservations.length === 0 && (
            <div className="no-reservations">
              {searchTerm || statusFilter !== 'all' 
                ? '検索条件に一致する予約が見つかりませんでした。'
                : '予約がありません。'
              }
            </div>
          )}

          {!isLoading && !error && sortedReservations.length > 0 && (
            <div className="admin-reservations-list">
              {sortedReservations.map((reservation) => (
                <div key={reservation.reservationId} className="admin-reservation-card">
                  <div className="reservation-header">
                    <div className="reservation-title-section">
                      <h4 className="reservation-title">{reservation.title}</h4>
                      <div className={`status-badge ${getStatusClass(reservation.status)}`}>
                        {getStatusText(reservation.status)}
                      </div>
                    </div>
                    <div className="reservation-meta">
                      <span className={`time-indicator ${isUpcoming(reservation.startTime) ? 'upcoming' : 'past'}`}>
                        {isUpcoming(reservation.startTime) ? '予定' : '過去'}
                      </span>
                    </div>
                  </div>

                  <div className="reservation-details">
                    <div className="detail-grid">
                      <div className="detail-item">
                        <span className="label">予約者:</span>
                        <span className="value">{reservation.userId}</span>
                      </div>
                      <div className="detail-item">
                        <span className="label">会議室:</span>
                        <span className="value">{reservation.roomId}</span>
                      </div>
                      <div className="detail-item">
                        <span className="label">開始:</span>
                        <span className="value">{formatDateTime(reservation.startTime)}</span>
                      </div>
                      <div className="detail-item">
                        <span className="label">終了:</span>
                        <span className="value">{formatDateTime(reservation.endTime)}</span>
                      </div>
                      {reservation.participants && reservation.participants.length > 0 && (
                        <div className="detail-item participants">
                          <span className="label">参加者:</span>
                          <span className="value">{reservation.participants.join(', ')}</span>
                        </div>
                      )}
                    </div>
                  </div>

                  <div className="reservation-actions">
                    <button 
                      className="btn-small history-btn-small"
                      onClick={() => handleHistoryClick(reservation)}
                      title="この予約の履歴を表示"
                    >
                      履歴
                    </button>
                    
                    {canCancelReservation(reservation) && (
                      <button 
                        className="btn-small cancel-btn-small"
                        onClick={() => handleCancelClick(reservation)}
                        title="この予約をキャンセル"
                      >
                        キャンセル
                      </button>
                    )}
                  </div>
                </div>
              ))}
            </div>
          )}
        </div>

        <div className="dialog-actions">
          <button 
            type="button" 
            className="btn btn-secondary"
            onClick={onClose}
          >
            閉じる
          </button>
        </div>
      </div>

      {/* キャンセル確認ダイアログ */}
      <CancelReservationDialog
        isOpen={cancelDialogOpen}
        reservation={selectedReservation}
        userInfo={userInfo}
        onClose={handleCancelDialogClose}
        onSuccess={handleCancelSuccess}
      />

      {/* キャンセル履歴ダイアログ */}
      <CancellationHistory
        isOpen={historyDialogOpen}
        reservationId={selectedReservation?.reservationId}
        userInfo={userInfo}
        onClose={handleHistoryClose}
      />
    </div>
  );
};

export default AdminCancellationPanel;