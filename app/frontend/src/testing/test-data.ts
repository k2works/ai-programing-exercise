// Mock data for testing purposes
export type User = {
  id: string;
  name: string;
  email: string;
  organizationId: string;
  role: 'admin' | 'user';
};

export type Organization = {
  id: string;
  name: string;
  slug: string;
  description?: string;
};

// Mock user data
const mockUser: User = {
  id: '1',
  name: 'テストユーザー',
  email: 'test@example.com',
  organizationId: 'org-1',
  role: 'admin',
};

// Mock organization data
const mockOrganization: Organization = {
  id: 'org-1',
  name: 'テスト株式会社',
  slug: 'test-company',
  description: 'テスト用の会社です',
};

// Hook to simulate user data fetching
export const useUser = () => {
  return {
    data: mockUser,
    isLoading: false,
    error: null,
  };
};

// Hook to simulate organization data fetching
export const useOrganization = (id: string) => {
  return {
    data: id === mockOrganization.id ? mockOrganization : null,
    isLoading: false,
    error: null,
  };
};