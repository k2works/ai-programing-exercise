'use client';

import { Button, Link, Loading, NotFound } from '@/components';
import { DashboardLayout } from '@/layouts';

export default function DemoPage() {
  return (
    <DashboardLayout>
      <div className="max-w-4xl mx-auto space-y-12">
        {/* Header */}
        <div className="text-center space-y-4">
          <div className="inline-flex items-center px-4 py-2 rounded-full bg-primary-100 text-primary-700 text-sm font-medium">
            Chapter 4 - Layout & Components Demo
          </div>
          <h1 className="text-4xl font-bold text-gray-900">
            コンポーネントライブラリ
          </h1>
          <p className="text-xl text-gray-600">
            Tailwind CSS ベースの再利用可能なUIコンポーネント
          </p>
        </div>

        {/* Button Components */}
        <section className="space-y-6">
          <h2 className="text-2xl font-bold text-gray-900">Button コンポーネント</h2>
          
          <div className="space-y-4">
            <div>
              <h3 className="text-lg font-semibold text-gray-700 mb-3">バリアント</h3>
              <div className="flex flex-wrap gap-4">
                <Button variant="solid">Solid Button</Button>
                <Button variant="outline">Outline Button</Button>
                <Button variant="ghost">Ghost Button</Button>
              </div>
            </div>

            <div>
              <h3 className="text-lg font-semibold text-gray-700 mb-3">サイズ</h3>
              <div className="flex flex-wrap items-center gap-4">
                <Button size="sm">Small</Button>
                <Button size="md">Medium</Button>
                <Button size="lg">Large</Button>
              </div>
            </div>

            <div>
              <h3 className="text-lg font-semibold text-gray-700 mb-3">状態</h3>
              <div className="flex flex-wrap gap-4">
                <Button isLoading>Loading</Button>
                <Button isDisabled>Disabled</Button>
                <Button 
                  icon={
                    <svg className="h-4 w-4" fill="currentColor" viewBox="0 0 20 20">
                      <path fillRule="evenodd" d="M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z" clipRule="evenodd" />
                    </svg>
                  }
                >
                  With Icon
                </Button>
              </div>
            </div>
          </div>
        </section>

        {/* Link Components */}
        <section className="space-y-6">
          <h2 className="text-2xl font-bold text-gray-900">Link コンポーネント</h2>
          
          <div className="space-y-4">
            <div>
              <h3 className="text-lg font-semibold text-gray-700 mb-3">バリアント</h3>
              <div className="flex flex-wrap gap-4">
                <Link href="#">Default Link</Link>
                <Link href="#" variant="solid">Solid Link</Link>
                <Link href="#" variant="outline">Outline Link</Link>
                <Link href="#" variant="ghost">Ghost Link</Link>
              </div>
            </div>

            <div>
              <h3 className="text-lg font-semibold text-gray-700 mb-3">アイコン付き</h3>
              <div className="flex flex-wrap gap-4">
                <Link 
                  href="#" 
                  icon={
                    <svg className="h-4 w-4" fill="currentColor" viewBox="0 0 20 20">
                      <path d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
                    </svg>
                  }
                >
                  Success Link
                </Link>
                <Link href="https://example.com" external>
                  External Link
                </Link>
              </div>
            </div>
          </div>
        </section>

        {/* Loading Component */}
        <section className="space-y-6">
          <h2 className="text-2xl font-bold text-gray-900">Loading コンポーネント</h2>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div className="border rounded-lg p-6">
              <h3 className="text-lg font-semibold text-gray-700 mb-3">デフォルト</h3>
              <Loading />
            </div>
            
            <div className="border rounded-lg p-6">
              <h3 className="text-lg font-semibold text-gray-700 mb-3">カスタム</h3>
              <Loading text="データを読み込み中..." size="lg" />
            </div>
          </div>
        </section>

        {/* NotFound Component */}
        <section className="space-y-6">
          <h2 className="text-2xl font-bold text-gray-900">NotFound コンポーネント</h2>
          
          <div className="border rounded-lg p-6">
            <NotFound 
              title="デモページが見つかりません"
              message="これはデモ用の NotFound コンポーネントです。"
              actionLabel="デモページに戻る"
              actionHref="/demo"
            />
          </div>
        </section>

        {/* Navigation */}
        <section className="space-y-6">
          <h2 className="text-2xl font-bold text-gray-900">ナビゲーション</h2>
          
          <div className="flex flex-wrap gap-4">
            <Link href="/" variant="outline">ホームに戻る</Link>
            <Link href="/dashboard/jobs" variant="solid">Jobs ダッシュボード</Link>
          </div>
        </section>
      </div>
    </DashboardLayout>
  );
}