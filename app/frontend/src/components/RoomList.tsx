import React, { useState, useEffect } from 'react';
import { ApiService, Room } from '../services/api';
import ReservationForm from './ReservationForm';
import ReservationList from './ReservationList';
import './RoomList.css';

interface RoomListProps {
  userInfo: {
    userId: string;
    name: string;
    role: string;
  };
  onLogout: () => void;
}

const RoomList: React.FC<RoomListProps> = ({ userInfo, onLogout }) => {
  const [rooms, setRooms] = useState<Room[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [selectedRoom, setSelectedRoom] = useState<Room | null>(null);
  const [showReservationForm, setShowReservationForm] = useState(false);

  useEffect(() => {
    loadRooms();
  }, []);

  const loadRooms = async () => {
    try {
      setIsLoading(true);
      setError(null);
      const roomsData = await ApiService.getRooms();
      setRooms(Array.isArray(roomsData) ? roomsData : []);
    } catch (err: any) {
      setError(err.response?.data?.message || '会議室情報の取得に失敗しました');
      setRooms([]);
    } finally {
      setIsLoading(false);
    }
  };

  const handleLogout = async () => {
    try {
      await ApiService.logout();
    } catch (err) {
      console.error('ログアウトエラー:', err);
    } finally {
      onLogout();
    }
  };

  const handleReserveRoom = (room: Room) => {
    setSelectedRoom(room);
    setShowReservationForm(true);
  };

  const handleReservationSuccess = () => {
    setShowReservationForm(false);
    setSelectedRoom(null);
    // 予約一覧の更新をトリガーする（ReservationListコンポーネントで自動更新されます）
  };

  const handleReservationCancel = () => {
    setShowReservationForm(false);
    setSelectedRoom(null);
  };

  if (isLoading) {
    return (
      <div className="room-list-container">
        <div className="loading">読み込み中...</div>
      </div>
    );
  }

  return (
    <div className="room-list-container">
      <header className="app-header">
        <h1>会議室予約システム (MRS)</h1>
        <div className="user-info">
          <span>ようこそ、{userInfo.name}さん ({userInfo.role})</span>
          <button onClick={handleLogout} className="logout-btn">
            ログアウト
          </button>
        </div>
      </header>

      <main className="room-list-content">
        <div className="section-header">
          <h2>会議室一覧</h2>
          <button onClick={loadRooms} className="refresh-btn">
            更新
          </button>
        </div>

        {error && (
          <div className="error-message">
            {error}
            <button onClick={loadRooms}>再試行</button>
          </div>
        )}

        <div className="rooms-grid">
          {Array.isArray(rooms) && rooms.map((room) => (
            <div key={room.roomId} className={`room-card ${room.isActive ? 'active' : 'inactive'}`}>
              <h3>{room.roomName}</h3>
              <div className="room-details">
                <p><strong>会議室ID:</strong> {room.roomId}</p>
                <p><strong>収容人数:</strong> {room.capacity}名</p>
                <p><strong>状態:</strong> {room.isActive ? '利用可能' : '利用不可'}</p>
              </div>
              {room.isActive && (
                <button 
                  className="reserve-btn"
                  onClick={() => handleReserveRoom(room)}
                >
                  予約する
                </button>
              )}
            </div>
          ))}
        </div>

        {Array.isArray(rooms) && rooms.length === 0 && !isLoading && !error && (
          <div className="no-rooms">
            会議室が見つかりませんでした。
          </div>
        )}
      </main>

      <ReservationList userInfo={userInfo} />

      {showReservationForm && selectedRoom && (
        <ReservationForm
          roomId={selectedRoom.roomId}
          roomName={selectedRoom.roomName}
          onSuccess={handleReservationSuccess}
          onCancel={handleReservationCancel}
        />
      )}
    </div>
  );
};

export default RoomList;