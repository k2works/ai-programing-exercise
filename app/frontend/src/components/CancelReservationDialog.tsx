import React, { useState } from 'react';
import { ApiService, Reservation } from '../services/api';
import './CancelReservationDialog.css';

interface CancelReservationDialogProps {
  isOpen: boolean;
  reservation: Reservation | null;
  userInfo: {
    userId: string;
    name: string;
    role: string;
  };
  onClose: () => void;
  onSuccess: () => void;
}

const CancelReservationDialog: React.FC<CancelReservationDialogProps> = ({
  isOpen,
  reservation,
  userInfo,
  onClose,
  onSuccess
}) => {
  const [reason, setReason] = useState('');
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!reservation) return;
    
    if (!reason.trim()) {
      setError('キャンセル理由を入力してください。');
      return;
    }

    if (reason.trim().length > 500) {
      setError('キャンセル理由は500文字以内で入力してください。');
      return;
    }

    setIsSubmitting(true);
    setError(null);

    try {
      await ApiService.cancelReservation(reservation.reservationId, {
        reason: reason.trim()
      });
      
      // 成功時の処理
      setReason('');
      onSuccess();
      onClose();
    } catch (err: any) {
      // エラーハンドリング
      if (err.response?.status === 401) {
        setError('この予約をキャンセルする権限がありません。');
      } else if (err.response?.status === 404) {
        setError('予約が見つかりません。既にキャンセルされている可能性があります。');
      } else if (err.response?.status === 409) {
        setError('この予約は既にキャンセルされています。');
      } else {
        setError(err.response?.data?.message || 'キャンセル処理に失敗しました。');
      }
    } finally {
      setIsSubmitting(false);
    }
  };

  const handleClose = () => {
    if (isSubmitting) return;
    setReason('');
    setError(null);
    onClose();
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

  const isOwner = reservation?.userId === userInfo.userId;
  const isAdmin = userInfo.role === 'Admin';
  const canCancel = isOwner || isAdmin;

  if (!isOpen || !reservation) {
    return null;
  }

  return (
    <div className="dialog-overlay" onClick={handleClose}>
      <div className="dialog-content" onClick={e => e.stopPropagation()}>
        <div className="dialog-header">
          <h3>予約キャンセル</h3>
          <button 
            type="button" 
            className="dialog-close-btn"
            onClick={handleClose}
            disabled={isSubmitting}
          >
            ×
          </button>
        </div>

        <div className="dialog-body">
          <div className="reservation-info">
            <h4>キャンセル対象の予約</h4>
            <div className="info-grid">
              <div className="info-row">
                <span className="label">予約タイトル:</span>
                <span className="value">{reservation.title}</span>
              </div>
              <div className="info-row">
                <span className="label">会議室:</span>
                <span className="value">{reservation.roomId}</span>
              </div>
              <div className="info-row">
                <span className="label">開始時刻:</span>
                <span className="value">{formatDateTime(reservation.startTime)}</span>
              </div>
              <div className="info-row">
                <span className="label">終了時刻:</span>
                <span className="value">{formatDateTime(reservation.endTime)}</span>
              </div>
              {!isOwner && (
                <div className="info-row">
                  <span className="label">予約者:</span>
                  <span className="value">{reservation.userId}</span>
                </div>
              )}
            </div>
          </div>

          {!canCancel && (
            <div className="warning-message">
              この予約をキャンセルする権限がありません。
            </div>
          )}

          {canCancel && (
            <form onSubmit={handleSubmit}>
              <div className="form-group">
                <label htmlFor="cancel-reason">
                  キャンセル理由 <span className="required">*</span>
                </label>
                <textarea
                  id="cancel-reason"
                  value={reason}
                  onChange={(e) => setReason(e.target.value)}
                  placeholder="キャンセルの理由を入力してください"
                  maxLength={500}
                  rows={4}
                  disabled={isSubmitting}
                  required
                />
                <div className="char-count">
                  {reason.length}/500文字
                </div>
              </div>

              {error && (
                <div className="error-message">
                  {error}
                </div>
              )}

              <div className="dialog-actions">
                <button 
                  type="button" 
                  className="btn btn-secondary"
                  onClick={handleClose}
                  disabled={isSubmitting}
                >
                  キャンセル
                </button>
                <button 
                  type="submit" 
                  className="btn btn-danger"
                  disabled={isSubmitting || !reason.trim()}
                >
                  {isSubmitting ? 'キャンセル中...' : '予約をキャンセル'}
                </button>
              </div>
            </form>
          )}
        </div>
      </div>
    </div>
  );
};

export default CancelReservationDialog;