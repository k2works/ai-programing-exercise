import React from 'react';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import CancelReservationDialog from './CancelReservationDialog';
import { ApiService } from '../services/api';

// API Service をモック化
jest.mock('../services/api');
const mockedApiService = ApiService as jest.Mocked<typeof ApiService>;

const mockUserInfo = {
  userId: 'user01',
  name: 'テストユーザー',
  role: 'User'
};

const mockAdminInfo = {
  userId: 'admin',
  name: '管理者',
  role: 'Admin'
};

const mockReservation = {
  reservationId: 'res-001',
  roomId: 'room-001',
  userId: 'user01',
  title: 'テスト会議',
  startTime: '2025-09-10T10:00:00Z',
  endTime: '2025-09-10T11:00:00Z',
  participants: ['user02', 'user03'],
  status: 'confirmed',
  rowVersion: 1,
  createdAt: '2025-09-01T09:00:00Z',
  updatedAt: '2025-09-01T09:00:00Z'
};

const mockProps = {
  isOpen: true,
  reservation: mockReservation,
  userInfo: mockUserInfo,
  onClose: jest.fn(),
  onSuccess: jest.fn()
};

describe('CancelReservationDialog', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('UI 表示テスト', () => {
    test('ダイアログが正しく表示される', () => {
      render(<CancelReservationDialog {...mockProps} />);
      
      expect(screen.getByText('予約キャンセル')).toBeInTheDocument();
      expect(screen.getByText('キャンセル対象の予約')).toBeInTheDocument();
      expect(screen.getByText('テスト会議')).toBeInTheDocument();
      expect(screen.getByText('room-001')).toBeInTheDocument();
    });

    test('isOpen が false の時は表示されない', () => {
      render(<CancelReservationDialog {...mockProps} isOpen={false} />);
      
      expect(screen.queryByText('予約キャンセル')).not.toBeInTheDocument();
    });

    test('reservation が null の時は表示されない', () => {
      render(<CancelReservationDialog {...mockProps} reservation={null} />);
      
      expect(screen.queryByText('予約キャンセル')).not.toBeInTheDocument();
    });
  });

  describe('権限制御テスト', () => {
    test('本人の予約の場合はキャンセルフォームが表示される', () => {
      render(<CancelReservationDialog {...mockProps} />);
      
      expect(screen.getByLabelText('キャンセル理由 *')).toBeInTheDocument();
      expect(screen.getByText('予約をキャンセル')).toBeInTheDocument();
    });

    test('管理者の場合は他者の予約でもキャンセルフォームが表示される', () => {
      const otherUserReservation = { ...mockReservation, userId: 'user02' };
      render(
        <CancelReservationDialog 
          {...mockProps} 
          reservation={otherUserReservation} 
          userInfo={mockAdminInfo}
        />
      );
      
      expect(screen.getByLabelText('キャンセル理由 *')).toBeInTheDocument();
      expect(screen.getByText('予約をキャンセル')).toBeInTheDocument();
      expect(screen.getByText('user02')).toBeInTheDocument(); // 予約者が表示される
    });

    test('権限がない場合は警告メッセージが表示される', () => {
      const otherUserReservation = { ...mockReservation, userId: 'user02' };
      render(
        <CancelReservationDialog 
          {...mockProps} 
          reservation={otherUserReservation}
        />
      );
      
      expect(screen.getByText('この予約をキャンセルする権限がありません。')).toBeInTheDocument();
      expect(screen.queryByLabelText('キャンセル理由 *')).not.toBeInTheDocument();
    });
  });

  describe('フォームバリデーションテスト', () => {
    test('理由が空の場合はエラーメッセージが表示される', async () => {
      const user = userEvent;
      render(<CancelReservationDialog {...mockProps} />);
      
      const cancelButton = screen.getByText('予約をキャンセル');
      await user.click(cancelButton);
      
      expect(screen.getByText('キャンセル理由を入力してください。')).toBeInTheDocument();
    });

    test('理由が500文字を超える場合はエラーメッセージが表示される', async () => {
      const user = userEvent;
      render(<CancelReservationDialog {...mockProps} />);
      
      const textarea = screen.getByLabelText('キャンセル理由 *');
      const longText = 'a'.repeat(501);
      
      await user.type(textarea, longText);
      
      const cancelButton = screen.getByText('予約をキャンセル');
      await user.click(cancelButton);
      
      expect(screen.getByText('キャンセル理由は500文字以内で入力してください。')).toBeInTheDocument();
    });

    test('文字数カウントが正しく表示される', async () => {
      const user = userEvent;
      render(<CancelReservationDialog {...mockProps} />);
      
      const textarea = screen.getByLabelText('キャンセル理由 *');
      await user.type(textarea, 'テストの理由です');
      
      expect(screen.getByText('8/500文字')).toBeInTheDocument();
    });
  });

  describe('キャンセル処理テスト', () => {
    test('正常なキャンセル処理が完了する', async () => {
      const user = userEvent;
      mockedApiService.cancelReservation.mockResolvedValue();
      
      render(<CancelReservationDialog {...mockProps} />);
      
      const textarea = screen.getByLabelText('キャンセル理由 *');
      await user.type(textarea, '会議が延期になりました');
      
      const cancelButton = screen.getByText('予約をキャンセル');
      await user.click(cancelButton);
      
      await waitFor(() => {
        expect(mockedApiService.cancelReservation).toHaveBeenCalledWith('res-001', {
          reason: '会議が延期になりました'
        });
      });
      
      expect(mockProps.onSuccess).toHaveBeenCalled();
      expect(mockProps.onClose).toHaveBeenCalled();
    });

    test('キャンセル処理中はボタンが無効になる', async () => {
      const user = userEvent;
      // レスポンスを遅延させる
      mockedApiService.cancelReservation.mockImplementation(
        () => new Promise(resolve => setTimeout(resolve, 1000))
      );
      
      render(<CancelReservationDialog {...mockProps} />);
      
      const textarea = screen.getByLabelText('キャンセル理由 *');
      await user.type(textarea, 'テストの理由');
      
      const cancelButton = screen.getByText('予約をキャンセル');
      await user.click(cancelButton);
      
      // ローディング状態の確認
      expect(screen.getByText('キャンセル中...')).toBeInTheDocument();
      expect(cancelButton).toBeDisabled();
    });

    test('API エラー時にエラーメッセージが表示される', async () => {
      const user = userEvent;
      mockedApiService.cancelReservation.mockRejectedValue({
        response: {
          status: 401,
          data: { message: 'Unauthorized' }
        }
      });
      
      render(<CancelReservationDialog {...mockProps} />);
      
      const textarea = screen.getByLabelText('キャンセル理由 *');
      await user.type(textarea, 'テストの理由');
      
      const cancelButton = screen.getByText('予約をキャンセル');
      await user.click(cancelButton);
      
      await waitFor(() => {
        expect(screen.getByText('この予約をキャンセルする権限がありません。')).toBeInTheDocument();
      });
    });

    test('404 エラー時に適切なメッセージが表示される', async () => {
      const user = userEvent;
      mockedApiService.cancelReservation.mockRejectedValue({
        response: { status: 404 }
      });
      
      render(<CancelReservationDialog {...mockProps} />);
      
      const textarea = screen.getByLabelText('キャンセル理由 *');
      await user.type(textarea, 'テストの理由');
      
      const cancelButton = screen.getByText('予約をキャンセル');
      await user.click(cancelButton);
      
      await waitFor(() => {
        expect(screen.getByText('予約が見つかりません。既にキャンセルされている可能性があります。')).toBeInTheDocument();
      });
    });

    test('409 エラー時に適切なメッセージが表示される', async () => {
      const user = userEvent;
      mockedApiService.cancelReservation.mockRejectedValue({
        response: { status: 409 }
      });
      
      render(<CancelReservationDialog {...mockProps} />);
      
      const textarea = screen.getByLabelText('キャンセル理由 *');
      await user.type(textarea, 'テストの理由');
      
      const cancelButton = screen.getByText('予約をキャンセル');
      await user.click(cancelButton);
      
      await waitFor(() => {
        expect(screen.getByText('この予約は既にキャンセルされています。')).toBeInTheDocument();
      });
    });
  });

  describe('ダイアログの操作テスト', () => {
    test('閉じるボタンでダイアログが閉じられる', async () => {
      const user = userEvent;
      render(<CancelReservationDialog {...mockProps} />);
      
      const closeButton = screen.getByText('×');
      await user.click(closeButton);
      
      expect(mockProps.onClose).toHaveBeenCalled();
    });

    test('キャンセルボタンでダイアログが閉じられる', async () => {
      const user = userEvent;
      render(<CancelReservationDialog {...mockProps} />);
      
      const cancelButton = screen.getByText('キャンセル');
      await user.click(cancelButton);
      
      expect(mockProps.onClose).toHaveBeenCalled();
    });

    test('オーバーレイクリックでダイアログが閉じられる', async () => {
      render(<CancelReservationDialog {...mockProps} />);
      
      const overlay = document.querySelector('.dialog-overlay');
      fireEvent.click(overlay!);
      
      expect(mockProps.onClose).toHaveBeenCalled();
    });

    test('ダイアログコンテンツクリックでは閉じられない', async () => {
      render(<CancelReservationDialog {...mockProps} />);
      
      const dialogContent = document.querySelector('.dialog-content');
      fireEvent.click(dialogContent!);
      
      expect(mockProps.onClose).not.toHaveBeenCalled();
    });
  });
});