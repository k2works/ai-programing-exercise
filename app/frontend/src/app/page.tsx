'use client';

import { Button, Link } from '@/components';
import { DashboardLayout } from '@/layouts';

export default function Home() {
  return (
    <DashboardLayout>
      <div className="max-w-4xl mx-auto text-center space-y-8">
        <div className="space-y-4">
          <div className="inline-flex items-center px-4 py-2 rounded-full bg-primary-100 text-primary-700 text-sm font-medium">
            Chapter 4 - Layout & Components Complete
          </div>
          <h1 className="text-5xl font-bold text-primary-700 tracking-tight">
            React Job Board Application
          </h1>
          <p className="text-xl text-gray-600 max-w-2xl mx-auto">
            レイアウトシステムとコンポーネントライブラリ構築完了
          </p>
        </div>
        
        <div className="space-y-6">
          <div className="text-lg text-gray-700 leading-relaxed">
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4 max-w-3xl mx-auto">
              <div className="flex items-center space-x-2">
                <span className="text-success">✅</span>
                <span>Next.js 14 + TypeScript 基盤</span>
              </div>
              <div className="flex items-center space-x-2">
                <span className="text-success">✅</span>
                <span>Tailwind CSS デザインシステム</span>
              </div>
              <div className="flex items-center space-x-2">
                <span className="text-success">✅</span>
                <span>DashboardLayout システム</span>
              </div>
              <div className="flex items-center space-x-2">
                <span className="text-success">✅</span>
                <span>再利用可能コンポーネント</span>
              </div>
              <div className="flex items-center space-x-2">
                <span className="text-success">✅</span>
                <span>ナビゲーションシステム</span>
              </div>
              <div className="flex items-center space-x-2">
                <span className="text-success">✅</span>
                <span>レスポンシブデザイン対応</span>
              </div>
            </div>
          </div>
          
          <div className="flex flex-wrap justify-center gap-4">
            <Button 
              size="lg"
              onClick={() => console.log('求人検索開始')}
            >
              求人検索を開始
            </Button>
            
            <Link href="/demo" variant="outline">
              コンポーネントデモ
            </Link>
          </div>
        </div>
      </div>
    </DashboardLayout>
  );
}
