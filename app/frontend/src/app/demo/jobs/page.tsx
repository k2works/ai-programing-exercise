'use client';

import { useState } from 'react';
import { JobsList, CreateJobForm, CreateJobData } from '@/features/jobs';
import { DashboardLayout } from '@/layouts';
import { mockJobs } from '@/testing/mock-data';
import { Button, Link } from '@/components';

export default function JobsDemoPage() {
  const [showCreateForm, setShowCreateForm] = useState(false);
  const [isLoading, setIsLoading] = useState(false);

  const handleCreateJob = async (data: CreateJobData) => {
    setIsLoading(true);
    // シミュレートされた API 呼び出し
    await new Promise((resolve) => setTimeout(resolve, 1500));
    console.log('Creating job:', data);
    setIsLoading(false);
    setShowCreateForm(false);
  };

  return (
    <DashboardLayout>
      <div className="max-w-6xl mx-auto space-y-8">
        {/* Header */}
        <div className="text-center space-y-4">
          <div className="inline-flex items-center px-4 py-2 rounded-full bg-primary-100 text-primary-700 text-sm font-medium">
            Chapter 5 - Jobs Feature Demo
          </div>
          <h1 className="text-4xl font-bold text-gray-900">
            Jobs Feature Architecture
          </h1>
          <p className="text-xl text-gray-600">
            Feature-based モジュール設計による Job 機能の実装
          </p>
        </div>

        {/* Navigation */}
        <div className="flex justify-center space-x-4">
          <Link href="/demo" variant="outline">
            コンポーネントデモに戻る
          </Link>
          <Button
            variant={showCreateForm ? "outline" : "solid"}
            onClick={() => setShowCreateForm(!showCreateForm)}
          >
            {showCreateForm ? 'Job リストを表示' : 'Job 作成フォームを表示'}
          </Button>
        </div>

        {/* Content */}
        {showCreateForm ? (
          <section className="space-y-6">
            <h2 className="text-2xl font-bold text-gray-900 text-center">
              Create Job Form
            </h2>
            <div className="bg-white p-8 rounded-lg shadow-lg">
              <CreateJobForm
                onSubmit={handleCreateJob}
                isLoading={isLoading}
              />
            </div>
          </section>
        ) : (
          <section className="space-y-6">
            <div className="text-center">
              <h2 className="text-2xl font-bold text-gray-900">
                Jobs List Component
              </h2>
              <p className="text-gray-600 mt-2">
                ダッシュボード形式の求人一覧表示
              </p>
            </div>
            
            <div className="bg-white p-6 rounded-lg shadow-lg">
              <JobsList
                jobs={mockJobs}
                type="dashboard"
                organizationId="org-1"
                isLoading={false}
              />
            </div>

            <div className="text-center">
              <h3 className="text-xl font-semibold text-gray-900 mb-4">
                パブリック形式の求人一覧
              </h3>
              <div className="bg-gray-50 p-6 rounded-lg">
                <JobsList
                  jobs={mockJobs.slice(0, 3)}
                  type="public"
                  organizationId="org-1"
                  isLoading={false}
                />
              </div>
            </div>
          </section>
        )}

        {/* Feature Information */}
        <section className="space-y-6 bg-gray-50 p-8 rounded-lg">
          <h3 className="text-2xl font-bold text-gray-900 text-center">
            Feature Architecture 特徴
          </h3>
          
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6 max-w-4xl mx-auto">
            <div className="bg-white p-6 rounded-lg shadow">
              <h4 className="text-lg font-semibold text-gray-900 mb-3">
                型定義システム
              </h4>
              <ul className="text-gray-600 space-y-2 text-sm">
                <li>• Entity 基底型の拡張</li>
                <li>• Pick utility type の活用</li>
                <li>• CreateJobData, UpdateJobData</li>
                <li>• フィルターとソート定義</li>
              </ul>
            </div>
            
            <div className="bg-white p-6 rounded-lg shadow">
              <h4 className="text-lg font-semibold text-gray-900 mb-3">
                コンポーネント設計
              </h4>
              <ul className="text-gray-600 space-y-2 text-sm">
                <li>• JobsList - 表形式表示</li>
                <li>• CreateJobForm - バリデーション付き</li>
                <li>• ダッシュボード / パブリック対応</li>
                <li>• DataTable の再利用</li>
              </ul>
            </div>
            
            <div className="bg-white p-6 rounded-lg shadow">
              <h4 className="text-lg font-semibold text-gray-900 mb-3">
                Feature 分離
              </h4>
              <ul className="text-gray-600 space-y-2 text-sm">
                <li>• types/ - 型定義</li>
                <li>• components/ - UI コンポーネント</li>
                <li>• index.ts - パブリック API</li>
                <li>• 明確な責任分離</li>
              </ul>
            </div>
            
            <div className="bg-white p-6 rounded-lg shadow">
              <h4 className="text-lg font-semibold text-gray-900 mb-3">
                スケーラビリティ
              </h4>
              <ul className="text-gray-600 space-y-2 text-sm">
                <li>• 機能単位での独立性</li>
                <li>• 再利用可能な設計</li>
                <li>• 他 Feature との疎結合</li>
                <li>• チーム並行開発対応</li>
              </ul>
            </div>
          </div>
        </section>
      </div>
    </DashboardLayout>
  );
}