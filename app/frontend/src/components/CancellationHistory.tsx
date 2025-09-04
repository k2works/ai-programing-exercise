import React, { useState, useEffect } from 'react';
import { ApiService, CancellationAuditLog } from '../services/api';
import './CancellationHistory.css';

interface CancellationHistoryProps {
  userInfo: {
    userId: string;
    name: string;
    role: string;
  };
  reservationId?: string;
  isOpen: boolean;
  onClose: () => void;
}

const CancellationHistory: React.FC<CancellationHistoryProps> = ({
  userInfo,
  reservationId,
  isOpen,
  onClose
}) => {
  const [logs, setLogs] = useState<CancellationAuditLog[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (isOpen) {
      loadCancellationLogs();
    }
  }, [isOpen, reservationId]);

  const loadCancellationLogs = async () => {
    try {
      setIsLoading(true);
      setError(null);
      const data = await ApiService.getCancellationLogs(reservationId);
      setLogs(Array.isArray(data) ? data : []);
    } catch (err: any) {
      setError(err.response?.data?.message || 'キャンセル履歴の取得に失敗しました');
      setLogs([]);
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

  const getActionText = (action: string) => {
    switch (action.toLowerCase()) {
      case 'cancelled':
        return 'キャンセル';
      case 'restored':
        return '復元';
      default:
        return action;
    }
  };

  const getRoleText = (role: string) => {
    switch (role.toLowerCase()) {
      case 'admin':
        return '管理者';
      case 'user':
        return '一般ユーザー';
      default:
        return role;
    }
  };

  const handleClose = () => {
    onClose();
  };

  if (!isOpen) {
    return null;
  }

  const isAdmin = userInfo.role === 'Admin';

  return (
    <div className="dialog-overlay" onClick={handleClose}>
      <div className="dialog-content cancellation-history-dialog" onClick={e => e.stopPropagation()}>
        <div className="dialog-header">
          <h3>キャンセル履歴</h3>
          <button 
            type="button" 
            className="dialog-close-btn"
            onClick={handleClose}
          >
            ×
          </button>
        </div>

        <div className="dialog-body">
          {isLoading && (
            <div className="loading">履歴を読み込み中...</div>
          )}

          {error && (
            <div className="error-message">
              {error}
              <button onClick={loadCancellationLogs}>再試行</button>
            </div>
          )}

          {!isLoading && !error && logs.length === 0 && (
            <div className="no-logs">
              {reservationId ? 'この予約のキャンセル履歴はありません。' : 'キャンセル履歴がありません。'}
            </div>
          )}

          {!isLoading && !error && logs.length > 0 && (
            <div className="logs-container">
              {logs.map((log) => (
                <div key={log.auditId} className="log-entry">
                  <div className="log-header">
                    <div className="log-action">
                      <span className={`action-badge action-${log.action.toLowerCase()}`}>
                        {getActionText(log.action)}
                      </span>
                    </div>
                    <div className="log-time">
                      {formatDateTime(log.performedAt)}
                    </div>
                  </div>

                  <div className="log-details">
                    <div className="detail-row">
                      <span className="label">実行者:</span>
                      <span className="value">
                        {log.performedBy} ({getRoleText(log.userRole)})
                      </span>
                    </div>
                    
                    {log.reservation && (
                      <>
                        <div className="detail-row">
                          <span className="label">予約タイトル:</span>
                          <span className="value">{log.reservation.title}</span>
                        </div>
                        <div className="detail-row">
                          <span className="label">会議室:</span>
                          <span className="value">{log.reservation.roomId}</span>
                        </div>
                        <div className="detail-row">
                          <span className="label">予約時間:</span>
                          <span className="value">
                            {formatDateTime(log.reservation.startTime)} - {formatDateTime(log.reservation.endTime)}
                          </span>
                        </div>
                      </>
                    )}

                    {!log.reservation && (
                      <div className="detail-row">
                        <span className="label">予約ID:</span>
                        <span className="value">{log.reservationId}</span>
                      </div>
                    )}

                    {log.reason && (
                      <div className="detail-row">
                        <span className="label">理由:</span>
                        <span className="value reason-text">{log.reason}</span>
                      </div>
                    )}

                    {log.adminComment && isAdmin && (
                      <div className="detail-row">
                        <span className="label">管理者コメント:</span>
                        <span className="value admin-comment">{log.adminComment}</span>
                      </div>
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
            onClick={handleClose}
          >
            閉じる
          </button>
        </div>
      </div>
    </div>
  );
};

export default CancellationHistory;