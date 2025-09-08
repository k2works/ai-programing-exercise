// テスト用のモックデータとフック

export const mockUser = {
  id: '1',
  email: 'test@example.com',
  name: 'Test User',
  role: 'hr_staff' as const,
  organizationId: 'org-1',
  createdAt: '2025-01-01T00:00:00Z',
  updatedAt: '2025-01-01T00:00:00Z',
};

// テスト用ユーザーフック
export const useUser = () => {
  return {
    data: mockUser,
    isLoading: false,
    error: null,
  };
};