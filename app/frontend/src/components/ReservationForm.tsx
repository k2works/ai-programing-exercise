import React, { useState } from 'react';
import { ApiService, CreateReservationRequest } from '../services/api';
import './ReservationForm.css';

interface ReservationFormProps {
  roomId: string;
  roomName: string;
  onSuccess: () => void;
  onCancel: () => void;
}

const ReservationForm: React.FC<ReservationFormProps> = ({ 
  roomId, 
  roomName, 
  onSuccess, 
  onCancel 
}) => {
  const [formData, setFormData] = useState({
    title: '',
    startTime: '',
    endTime: '',
    participants: ''
  });
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    const { name, value } = e.target;
    setFormData(prev => ({
      ...prev,
      [name]: value
    }));
  };

  const validateForm = () => {
    if (!formData.title.trim()) {
      return '会議のタイトルを入力してください。';
    }
    if (!formData.startTime) {
      return '開始時刻を選択してください。';
    }
    if (!formData.endTime) {
      return '終了時刻を選択してください。';
    }
    if (new Date(formData.startTime) >= new Date(formData.endTime)) {
      return '終了時刻は開始時刻より後である必要があります。';
    }
    if (new Date(formData.startTime) < new Date()) {
      return '過去の時刻では予約できません。';
    }
    return null;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    const validationError = validateForm();
    if (validationError) {
      setError(validationError);
      return;
    }

    setIsSubmitting(true);
    setError(null);

    try {
      const participants = formData.participants
        .split(',')
        .map(p => p.trim())
        .filter(p => p.length > 0);

      const request: CreateReservationRequest = {
        roomId,
        title: formData.title.trim(),
        startTime: new Date(formData.startTime).toISOString(),
        endTime: new Date(formData.endTime).toISOString(),
        participants: participants.length > 0 ? participants : undefined
      };

      await ApiService.createReservation(request);
      onSuccess();
    } catch (err: any) {
      const errorMessage = err.response?.data?.message || 
                          err.response?.data?.error?.message ||
                          '予約の作成に失敗しました。';
      setError(errorMessage);
    } finally {
      setIsSubmitting(false);
    }
  };

  // デフォルトの開始時刻（現在時刻の次の時間、分は00に）
  const getDefaultStartTime = () => {
    const now = new Date();
    const nextHour = new Date(now.getFullYear(), now.getMonth(), now.getDate(), now.getHours() + 1, 0);
    return nextHour.toISOString().slice(0, 16);
  };

  // デフォルトの終了時刻（開始時刻の1時間後）
  const getDefaultEndTime = (startTime: string) => {
    if (!startTime) return '';
    const start = new Date(startTime);
    const end = new Date(start.getTime() + 60 * 60 * 1000); // 1時間後
    return end.toISOString().slice(0, 16);
  };

  // 開始時刻が変更された時の処理
  const handleStartTimeChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const startTime = e.target.value;
    setFormData(prev => ({
      ...prev,
      startTime,
      endTime: getDefaultEndTime(startTime)
    }));
  };

  return (
    <div className="reservation-form-overlay">
      <div className="reservation-form-container">
        <div className="form-header">
          <h2>予約作成</h2>
          <p className="room-info">会議室: {roomName}</p>
        </div>

        <form onSubmit={handleSubmit} className="reservation-form">
          <div className="form-group">
            <label htmlFor="title">会議タイトル *</label>
            <input
              type="text"
              id="title"
              name="title"
              value={formData.title}
              onChange={handleInputChange}
              placeholder="会議のタイトルを入力してください"
              required
              disabled={isSubmitting}
            />
          </div>

          <div className="form-row">
            <div className="form-group">
              <label htmlFor="startTime">開始時刻 *</label>
              <input
                type="datetime-local"
                id="startTime"
                name="startTime"
                value={formData.startTime || getDefaultStartTime()}
                onChange={handleStartTimeChange}
                required
                disabled={isSubmitting}
                min={new Date().toISOString().slice(0, 16)}
              />
            </div>

            <div className="form-group">
              <label htmlFor="endTime">終了時刻 *</label>
              <input
                type="datetime-local"
                id="endTime"
                name="endTime"
                value={formData.endTime}
                onChange={handleInputChange}
                required
                disabled={isSubmitting}
                min={formData.startTime}
              />
            </div>
          </div>

          <div className="form-group">
            <label htmlFor="participants">参加者（オプション）</label>
            <textarea
              id="participants"
              name="participants"
              value={formData.participants}
              onChange={handleInputChange}
              placeholder="参加者名をカンマ区切りで入力してください（例: 田中, 佐藤, 山田）"
              rows={3}
              disabled={isSubmitting}
            />
            <small className="form-help">
              参加者名をカンマ（,）で区切って入力してください
            </small>
          </div>

          {error && (
            <div className="error-message">
              {error}
            </div>
          )}

          <div className="form-actions">
            <button 
              type="button" 
              onClick={onCancel}
              className="cancel-btn"
              disabled={isSubmitting}
            >
              キャンセル
            </button>
            <button 
              type="submit" 
              className="submit-btn"
              disabled={isSubmitting}
            >
              {isSubmitting ? '作成中...' : '予約を作成'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
};

export default ReservationForm;