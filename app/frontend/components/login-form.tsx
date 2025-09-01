'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import { authService } from '@/services/auth-service';

const loginSchema = z.object({
  userId: z.string().min(1, 'ユーザーIDを入力してください'),
  password: z.string().min(1, 'パスワードを入力してください'),
});

type LoginFormData = z.infer<typeof loginSchema>;

export default function LoginForm() {
  const router = useRouter();
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const {
    register,
    handleSubmit,
    formState: { errors },
  } = useForm<LoginFormData>({
    resolver: zodResolver(loginSchema),
  });

  const onSubmit = async (data: LoginFormData) => {
    setLoading(true);
    setError(null);

    try {
      await authService.login(data);
      router.push('/reservations/new');
    } catch (err) {
      const error = err as { response?: { data?: { message?: string } } };
      setError(error.response?.data?.message || 'ログインに失敗しました');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="max-w-md mx-auto p-8 bg-white rounded-xl shadow-lg border border-gray-200">
      <h2 className="text-3xl font-bold mb-8 text-center text-gray-800">ログイン</h2>

      {error && (
        <div className="mb-6 p-4 bg-red-50 border-l-4 border-red-500 text-red-800 rounded">
          <strong>エラー:</strong> {error}
        </div>
      )}

      <form onSubmit={handleSubmit(onSubmit)} className="space-y-6">
        <div>
          <label className="block text-sm font-semibold text-gray-800 mb-2">
            ユーザーID
          </label>
          <input
            type="text"
            {...register('userId')}
            className="w-full px-4 py-3 border-2 border-gray-300 rounded-lg focus:outline-none focus:border-blue-500 focus:ring-2 focus:ring-blue-200 text-gray-800 bg-white"
            disabled={loading}
            placeholder="例: taro"
          />
          {errors.userId && (
            <p className="mt-2 text-sm text-red-600 font-medium">{errors.userId.message}</p>
          )}
        </div>

        <div>
          <label className="block text-sm font-semibold text-gray-800 mb-2">
            パスワード
          </label>
          <input
            type="password"
            {...register('password')}
            className="w-full px-4 py-3 border-2 border-gray-300 rounded-lg focus:outline-none focus:border-blue-500 focus:ring-2 focus:ring-blue-200 text-gray-800 bg-white"
            disabled={loading}
            placeholder="パスワード"
          />
          {errors.password && (
            <p className="mt-2 text-sm text-red-600 font-medium">{errors.password.message}</p>
          )}
        </div>

        <button
          type="submit"
          disabled={loading}
          className="w-full py-3 px-4 bg-blue-600 hover:bg-blue-700 disabled:bg-gray-400 disabled:cursor-not-allowed text-white font-semibold rounded-lg transition duration-200 shadow-md hover:shadow-lg"
        >
          {loading ? 'ログイン中...' : 'ログイン'}
        </button>
      </form>

      <div className="mt-6 p-4 bg-blue-50 border border-blue-200 rounded-lg">
        <p className="text-sm font-semibold text-blue-800 mb-2">デモ用ログイン情報:</p>
        <p className="text-sm text-blue-700 font-mono">ID: user1 / Pass: demo</p>
        <p className="text-sm text-blue-700 font-mono">ID: admin1 / Pass: demo</p>
      </div>
    </div>
  );
}