import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { CheckCircle, Calendar } from 'lucide-react'

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      refetchOnWindowFocus: false,
    },
  },
})

function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 py-8">
        <div className="container mx-auto px-4">
          <header className="mb-8 text-center">
            <h1 className="mb-4 flex items-center justify-center gap-3 text-4xl font-bold text-gray-900">
              <Calendar className="h-10 w-10 text-blue-600" />
              会議室予約システム
            </h1>
            <p className="mx-auto max-w-2xl text-lg text-gray-600">
              効率的な会議室管理と予約システムで、チームの生産性を向上させましょう
            </p>
          </header>

          <div className="mb-8 grid gap-6 md:grid-cols-2 lg:grid-cols-3">
            <div className="rounded-lg bg-white p-6 shadow-md">
              <div className="mb-4 flex items-center">
                <CheckCircle className="mr-3 h-8 w-8 text-green-600" />
                <h2 className="text-xl font-semibold text-gray-900">環境構築完了</h2>
              </div>
              <p className="text-gray-600">
                React 19 + TypeScript + Vite + TailwindCSS による
                モダンなフロントエンド開発環境が構築されました。
              </p>
            </div>

            <div className="rounded-lg bg-white p-6 shadow-md">
              <div className="mb-4 flex items-center">
                <CheckCircle className="mr-3 h-8 w-8 text-green-600" />
                <h2 className="text-xl font-semibold text-gray-900">技術スタック</h2>
              </div>
              <ul className="space-y-1 text-gray-600">
                <li>• React Query (サーバー状態管理)</li>
                <li>• Zustand (クライアント状態管理)</li>
                <li>• React Hook Form + Zod (フォーム)</li>
                <li>• Vitest (テスト)</li>
              </ul>
            </div>

            <div className="rounded-lg bg-white p-6 shadow-md">
              <div className="mb-4 flex items-center">
                <CheckCircle className="mr-3 h-8 w-8 text-green-600" />
                <h2 className="text-xl font-semibold text-gray-900">開発準備完了</h2>
              </div>
              <p className="text-gray-600">
                TDD対応、品質管理ツール、自動化環境が 整備され、開発を開始できます。
              </p>
            </div>
          </div>

          <div className="text-center">
            <p className="text-sm text-gray-500">
              フロントエンド環境構築ガイド準拠 | 設計書ベース統合セットアップ
            </p>
          </div>
        </div>
      </div>
    </QueryClientProvider>
  )
}

export default App
