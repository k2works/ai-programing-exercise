import React from 'react';
import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import AdminCancellationPanel from './AdminCancellationPanel';
import { ApiService } from '../services/api';

// API Service をモック化
jest.mock('../services/api');
const mockedApiService = ApiService as jest.Mocked<typeof ApiService>;

const mockAdminInfo = {
  userId: 'admin',
  name: '管理者',
  role: 'Admin'
};

const mockUserInfo = {
  userId: 'user01',
  name: 'テストユーザー',
  role: 'User'
};

const mockReservations = [
  {
    reservationId: 'res-001',
    roomId: 'room-001',
    userId: 'user01',
    title: 'ユーザー1の会議',
    startTime: '2025-09-10T10:00:00Z',
    endTime: '2025-09-10T11:00:00Z',
    participants: ['user02'],
    status: 'confirmed',
    rowVersion: 1,
    createdAt: '2025-09-01T09:00:00Z',
    updatedAt: '2025-09-01T09:00:00Z'
  },
  {
    reservationId: 'res-002',
    roomId: 'room-002',
    userId: 'user02',
    title: 'ユーザー2の会議',
    startTime: '2025-09-11T14:00:00Z',
    endTime: '2025-09-11T15:00:00Z',
    participants: [],
    status: 'cancelled',
    rowVersion: 2,
    createdAt: '2025-09-02T10:00:00Z',
    updatedAt: '2025-09-02T10:30:00Z'
  }
];

const mockProps = {
  isOpen: true,
  userInfo: mockAdminInfo,
  onClose: jest.fn()
};

describe('AdminCancellationPanel', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('権限制御テスト', () => {
    test('管理者でない場合はアクセス拒否メッセージが表示される', () => {
      render(
        <AdminCancellationPanel 
          {...mockProps} 
          userInfo={mockUserInfo}
        />
      );
      
      expect(screen.getByText('アクセス拒否')).toBeInTheDocument();
      expect(screen.getByText('この機能は管理者のみ利用できます。')).toBeInTheDocument();
    });

    test('管理者の場合は管理パネルが表示される', async () => {
      mockedApiService.getReservations.mockResolvedValue(mockReservations);
      
      render(<AdminCancellationPanel {...mockProps} />);
      
      expect(screen.getByText('管理者キャンセル機能')).toBeInTheDocument();
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });
    });

    test('isOpen が false の時は表示されない', () => {
      render(<AdminCancellationPanel {...mockProps} isOpen={false} />);
      
      expect(screen.queryByText('管理者キャンセル機能')).not.toBeInTheDocument();
    });
  });

  describe('データ取得テスト', () => {
    test('予約データが正しく取得・表示される', async () => {
      mockedApiService.getReservations.mockResolvedValue(mockReservations);
      
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(mockedApiService.getReservations).toHaveBeenCalled();
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });
    });

    test('API エラー時にエラーメッセージが表示される', async () => {
      mockedApiService.getReservations.mockRejectedValue({
        response: { data: { message: 'API Error' } }
      });
      
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(screen.getByText('API Error')).toBeInTheDocument();
      });
    });

    test('予約データが空の場合はメッセージが表示される', async () => {
      mockedApiService.getReservations.mockResolvedValue([]);
      
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(screen.getByText('予約がありません。')).toBeInTheDocument();
      });
    });
  });

  describe('検索・フィルタリング機能テスト', () => {
    beforeEach(() => {
      mockedApiService.getReservations.mockResolvedValue(mockReservations);
    });

    test('タイトル検索が正しく機能する', async () => {
      const user = userEvent;
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });

      const searchInput = screen.getByPlaceholderText('予約タイトル、ユーザーID、会議室IDで検索...');
      await user.type(searchInput, 'ユーザー1');
      
      expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
      expect(screen.queryByText('ユーザー2の会議')).not.toBeInTheDocument();
    });

    test('ユーザーID検索が正しく機能する', async () => {
      const user = userEvent;
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });

      const searchInput = screen.getByPlaceholderText('予約タイトル、ユーザーID、会議室IDで検索...');
      await user.type(searchInput, 'user02');
      
      expect(screen.queryByText('ユーザー1の会議')).not.toBeInTheDocument();
      expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
    });

    test('ステータスフィルターが正しく機能する', async () => {
      const user = userEvent;
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });

      const statusFilter = screen.getByDisplayValue('確定');
      await user.selectOptions(statusFilter, 'confirmed');
      
      expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
      expect(screen.queryByText('ユーザー2の会議')).not.toBeInTheDocument();
    });

    test('キャンセル済みフィルターが正しく機能する', async () => {
      const user = userEvent;
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });

      const statusFilter = screen.getByDisplayValue('確定');
      await user.selectOptions(statusFilter, 'cancelled');
      
      expect(screen.queryByText('ユーザー1の会議')).not.toBeInTheDocument();
      expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
    });

    test('検索条件に一致しない場合のメッセージが表示される', async () => {
      const user = userEvent;
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
      });

      const searchInput = screen.getByPlaceholderText('予約タイトル、ユーザーID、会議室IDで検索...');
      await user.type(searchInput, '存在しない検索語');
      
      expect(screen.getByText('検索条件に一致する予約が見つかりませんでした。')).toBeInTheDocument();
    });
  });

  describe('予約カード表示テスト', () => {
    beforeEach(() => {
      mockedApiService.getReservations.mockResolvedValue(mockReservations);
    });

    test('確定済み予約にはキャンセルボタンが表示される', async () => {
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        const reservationCards = screen.getAllByText('キャンセル');
        // 確定済み予約（res-001）にのみキャンセルボタンが表示される
        expect(reservationCards).toHaveLength(1);
      });
    });

    test('キャンセル済み予約にはキャンセルボタンが表示されない', async () => {
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
        // キャンセル済みの予約カード内にキャンセルボタンがないことを確認
        const user2Card = screen.getByText('ユーザー2の会議').closest('.admin-reservation-card');
        expect(user2Card).not.toHaveTextContent('キャンセル');
      });
    });

    test('すべての予約に履歴ボタンが表示される', async () => {
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        const historyButtons = screen.getAllByText('履歴');
        // 2つの予約 + 1つの全履歴ボタン = 3つ
        expect(historyButtons).toHaveLength(3);
      });
    });

    test('予約詳細が正しく表示される', async () => {
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        // 予約者情報
        expect(screen.getByText('user01')).toBeInTheDocument();
        expect(screen.getByText('user02')).toBeInTheDocument();
        
        // 会議室情報
        expect(screen.getByText('room-001')).toBeInTheDocument();
        expect(screen.getByText('room-002')).toBeInTheDocument();
        
        // ステータス表示
        expect(screen.getByText('確定')).toBeInTheDocument();
        expect(screen.getByText('キャンセル')).toBeInTheDocument();
      });
    });
  });

  describe('アクション機能テスト', () => {
    beforeEach(() => {
      mockedApiService.getReservations.mockResolvedValue(mockReservations);
    });

    test('更新ボタンで予約リストが再取得される', async () => {
      const user = userEvent;
      render(<AdminCancellationPanel {...mockProps} />);
      
      await waitFor(() => {
        expect(mockedApiService.getReservations).toHaveBeenCalledTimes(1);
      });

      const refreshButton = screen.getByText('更新');
      await user.click(refreshButton);
      
      expect(mockedApiService.getReservations).toHaveBeenCalledTimes(2);
    });

    test('閉じるボタンでパネルが閉じられる', async () => {
      const user = userEvent;
      render(<AdminCancellationPanel {...mockProps} />);
      
      const closeButton = screen.getByText('閉じる');
      await user.click(closeButton);
      
      expect(mockProps.onClose).toHaveBeenCalled();
    });

    test('×ボタンでパネルが閉じられる', async () => {
      const user = userEvent;
      render(<AdminCancellationPanel {...mockProps} />);
      
      const xButton = screen.getByText('×');
      await user.click(xButton);
      
      expect(mockProps.onClose).toHaveBeenCalled();
    });
  });
});