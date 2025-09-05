import React from 'react';
import { render, screen, waitFor, act } from '@testing-library/react';
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
      
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      expect(screen.getByText('管理者キャンセル機能')).toBeInTheDocument();
      
      await waitFor(() => {
        // 初期フィルターが 'confirmed' のため、確定済み予約のみ表示される
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        // 'ユーザー2の会議' は cancelled ステータスのため初期表示されない
        expect(screen.queryByText('ユーザー2の会議')).not.toBeInTheDocument();
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
      
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      await waitFor(() => {
        expect(mockedApiService.getReservations).toHaveBeenCalled();
        // 初期フィルターが 'confirmed' のため、確定済み予約のみ表示される
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
      });
    });

    test('API エラー時にエラーメッセージが表示される', async () => {
      mockedApiService.getReservations.mockRejectedValue({
        response: { data: { message: 'API Error' } }
      });
      
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      await waitFor(() => {
        expect(screen.getByText('API Error')).toBeInTheDocument();
      });
    });

    test('予約データが空の場合はメッセージが表示される', async () => {
      mockedApiService.getReservations.mockResolvedValue([]);
      
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      await waitFor(() => {
        // 空のデータがフィルタリングされた結果、検索条件に一致しないメッセージが表示される
        expect(screen.getByText('検索条件に一致する予約が見つかりませんでした。')).toBeInTheDocument();
      });
    });
  });

  describe('検索・フィルタリング機能テスト', () => {
    beforeEach(() => {
      mockedApiService.getReservations.mockResolvedValue(mockReservations);
    });

    test('タイトル検索が正しく機能する', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      // まず、すべてのステータスを表示するようにフィルターを変更
      const statusFilter = screen.getByDisplayValue('確定');
      
      await act(async () => {
        await userEvent.selectOptions(statusFilter, 'all');
      });
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });

      const searchInput = screen.getByPlaceholderText('予約タイトル、ユーザーID、会議室IDで検索...');
      
      await act(async () => {
        await userEvent.type(searchInput, 'ユーザー1');
      });
      
      expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
      expect(screen.queryByText('ユーザー2の会議')).not.toBeInTheDocument();
    });

    test('ユーザーID検索が正しく機能する', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      // まず、すべてのステータスを表示するようにフィルターを変更
      const statusFilter = screen.getByDisplayValue('確定');
      
      await act(async () => {
        await userEvent.selectOptions(statusFilter, 'all');
      });
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });

      const searchInput = screen.getByPlaceholderText('予約タイトル、ユーザーID、会議室IDで検索...');
      
      await act(async () => {
        await userEvent.type(searchInput, 'user02');
      });
      
      expect(screen.queryByText('ユーザー1の会議')).not.toBeInTheDocument();
      expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
    });

    test('ステータスフィルターが正しく機能する', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      // 初期状態では 'confirmed' フィルターが選択されている
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.queryByText('ユーザー2の会議')).not.toBeInTheDocument();
      });
      
      // confirmed フィルターの動作は既に確認済みなので、別のフィルターをテスト
      const statusFilter = screen.getByDisplayValue('確定');
      
      await act(async () => {
        await userEvent.selectOptions(statusFilter, 'all');
      });
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });
    });

    test('キャンセル済みフィルターが正しく機能する', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      // 初期状態では 'confirmed' フィルターが選択されているため、確定済み予約のみ表示
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.queryByText('ユーザー2の会議')).not.toBeInTheDocument();
      });

      const statusFilter = screen.getByDisplayValue('確定');
      
      await act(async () => {
        await userEvent.selectOptions(statusFilter, 'cancelled');
      });
      
      await waitFor(() => {
        expect(screen.queryByText('ユーザー1の会議')).not.toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });
    });

    test('検索条件に一致しない場合のメッセージが表示される', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
      });

      const searchInput = screen.getByPlaceholderText('予約タイトル、ユーザーID、会議室IDで検索...');
      
      await act(async () => {
        await userEvent.type(searchInput, '存在しない検索語');
      });
      
      await waitFor(() => {
        expect(screen.getByText('検索条件に一致する予約が見つかりませんでした。')).toBeInTheDocument();
      });
    });
  });

  describe('予約カード表示テスト', () => {
    beforeEach(() => {
      mockedApiService.getReservations.mockResolvedValue(mockReservations);
    });

    test('確定済み予約にはキャンセルボタンが表示される', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      await waitFor(() => {
        // フィルターオプションの 'キャンセル' を除外し、ボタンのみをカウント
        const cancelButtons = screen.getAllByRole('button', { name: /キャンセル/i });
        // 確定済み予約（res-001）にのみキャンセルボタンが表示される
        expect(cancelButtons).toHaveLength(1);
      });
    });

    test('キャンセル済み予約にはキャンセルボタンが表示されない', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      // キャンセル済み予約を表示するためにフィルターを変更
      const statusFilter = screen.getByDisplayValue('確定');
      
      await act(async () => {
        await userEvent.selectOptions(statusFilter, 'cancelled');
      });
      
      await waitFor(() => {
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
        // キャンセル済みの予約カード内のキャンセルボタンを確認
        const user2Card = screen.getByText('ユーザー2の会議').closest('.admin-reservation-card');
        const cancelButtons = user2Card?.querySelectorAll('button.cancel-btn-small');
        // キャンセル済み予約にはキャンセルボタンが表示されない
        expect(cancelButtons).toHaveLength(0);
      });
    });

    test('すべての予約に履歴ボタンが表示される', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      // まず、すべてのステータスを表示するようにフィルターを変更
      const statusFilter = screen.getByDisplayValue('確定');
      
      await act(async () => {
        await userEvent.selectOptions(statusFilter, 'all');
      });
      
      await waitFor(() => {
        // 各予約カードの履歴ボタンをカウント
        const individualHistoryButtons = screen.getAllByTitle('この予約の履歴を表示');
        // 全履歴表示ボタンをカウント
        const globalHistoryButton = screen.getByText('全履歴表示');
        
        // 2つの予約の履歴ボタン + 1つの全履歴ボタン
        expect(individualHistoryButtons).toHaveLength(2);
        expect(globalHistoryButton).toBeInTheDocument();
      });
    });

    test('予約詳細が正しく表示される', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      // まず、すべてのステータスを表示するようにフィルターを変更
      const statusFilter = screen.getByDisplayValue('確定');
      
      await act(async () => {
        await userEvent.selectOptions(statusFilter, 'all');
      });
      
      await waitFor(() => {
        // 予約タイトルが表示されてから詳細を確認
        expect(screen.getByText('ユーザー1の会議')).toBeInTheDocument();
        expect(screen.getByText('ユーザー2の会議')).toBeInTheDocument();
      });
      
      await waitFor(() => {
        // 予約者情報 - 複数存在する場合もあるため、少なくとも1つは存在することを確認
        expect(screen.getAllByText('user01')).toHaveLength(1);
        expect(screen.getAllByText('user02').length).toBeGreaterThanOrEqual(1);
        
        // 会議室情報
        expect(screen.getByText('room-001')).toBeInTheDocument();
        expect(screen.getByText('room-002')).toBeInTheDocument();
        
        // ステータス表示 - フィルターオプションとステータスバッジが存在
        expect(screen.getAllByText('確定').length).toBeGreaterThanOrEqual(1);
        expect(screen.getAllByText('キャンセル').length).toBeGreaterThanOrEqual(1);
      });
    });
  });

  describe('アクション機能テスト', () => {
    beforeEach(() => {
      mockedApiService.getReservations.mockResolvedValue(mockReservations);
    });

    test('更新ボタンで予約リストが再取得される', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      await waitFor(() => {
        expect(mockedApiService.getReservations).toHaveBeenCalledTimes(1);
      });

      const refreshButton = screen.getByText('更新');
      
      await act(async () => {
        await userEvent.click(refreshButton);
      });
      
      expect(mockedApiService.getReservations).toHaveBeenCalledTimes(2);
    });

    test('閉じるボタンでパネルが閉じられる', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      const closeButton = screen.getByText('閉じる');
      
      await act(async () => {
        await userEvent.click(closeButton);
      });
      
      expect(mockProps.onClose).toHaveBeenCalled();
    });

    test('×ボタンでパネルが閉じられる', async () => {
      await act(async () => {
        render(<AdminCancellationPanel {...mockProps} />);
      });
      
      const xButton = screen.getByText('×');
      
      await act(async () => {
        await userEvent.click(xButton);
      });
      
      expect(mockProps.onClose).toHaveBeenCalled();
    });
  });
});