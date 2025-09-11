'use client';

import { DashboardLayout } from '@/layouts';
import { Button, Link } from '@/components';

export default function JobsDemoPage() {
  return (
    <DashboardLayout>
      <div className="max-w-6xl mx-auto space-y-8">
        {/* Header */}
        <div className="text-center space-y-4">
          <div className="inline-flex items-center px-4 py-2 rounded-full bg-primary-100 text-primary-700 text-sm font-medium">
            🚀 Jobs Feature Demo
          </div>
          <h1 className="text-4xl font-bold text-gray-900">
            Jobs Management System
          </h1>
          <p className="text-lg text-gray-800 max-w-3xl mx-auto">
            Comprehensive job management system with CRUD operations, search functionality, 
            and real-time updates using React Query and Zustand.
          </p>
          <div className="mt-6">
            <Link href="/demo" className="text-blue-600 hover:text-blue-800">
              ← Back to Demo Overview
            </Link>
          </div>
        </div>

        {/* Demo Status */}
        <section className="bg-white rounded-lg shadow p-6">
          <h2 className="text-2xl font-bold text-gray-900 mb-4">Demo Status</h2>
          <div className="space-y-4">
            <div className="flex justify-between items-center p-3 bg-yellow-50 rounded-lg">
              <span className="text-sm font-medium">Jobs Feature:</span>
              <span className="text-sm text-orange-600 font-semibold">Under Development</span>
            </div>
            <div className="flex justify-between items-center p-3 bg-blue-50 rounded-lg">
              <span className="text-sm font-medium">API Integration:</span>
              <span className="text-sm text-blue-600 font-semibold">Ready</span>
            </div>
            <div className="flex justify-between items-center p-3 bg-green-50 rounded-lg">
              <span className="text-sm font-medium">Components:</span>
              <span className="text-sm text-green-600 font-semibold">Available</span>
            </div>
          </div>
        </section>

        {/* Coming Soon Features */}
        <section className="bg-white rounded-lg shadow p-6">
          <h2 className="text-2xl font-bold text-gray-900 mb-4">Planned Features</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            <div className="p-4 border border-gray-200 rounded-lg">
              <h3 className="font-semibold text-gray-900 mb-2">📝 Job Listings</h3>
              <p className="text-sm text-gray-800">Browse and search available job positions</p>
            </div>
            <div className="p-4 border border-gray-200 rounded-lg">
              <h3 className="font-semibold text-gray-900 mb-2">➕ Create Jobs</h3>
              <p className="text-sm text-gray-800">Add new job postings with detailed information</p>
            </div>
            <div className="p-4 border border-gray-200 rounded-lg">
              <h3 className="font-semibold text-gray-900 mb-2">🔍 Search & Filter</h3>
              <p className="text-sm text-gray-800">Find jobs by location, skills, and experience</p>
            </div>
            <div className="p-4 border border-gray-200 rounded-lg">
              <h3 className="font-semibold text-gray-900 mb-2">📊 Analytics</h3>
              <p className="text-sm text-gray-800">Track job posting performance and metrics</p>
            </div>
            <div className="p-4 border border-gray-200 rounded-lg">
              <h3 className="font-semibold text-gray-900 mb-2">🏢 Organizations</h3>
              <p className="text-sm text-gray-800">Manage multiple organization job postings</p>
            </div>
            <div className="p-4 border border-gray-200 rounded-lg">
              <h3 className="font-semibold text-gray-900 mb-2">📱 Responsive</h3>
              <p className="text-sm text-gray-800">Mobile-optimized job management interface</p>
            </div>
          </div>
        </section>

        {/* Technology Stack */}
        <section className="bg-gray-50 rounded-lg p-6">
          <h2 className="text-2xl font-bold text-gray-900 mb-4">Technology Stack</h2>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            <div className="text-center p-3 bg-white rounded-lg shadow-sm">
              <div className="font-semibold text-gray-900">React Query</div>
              <div className="text-sm text-gray-800">Data Fetching</div>
            </div>
            <div className="text-center p-3 bg-white rounded-lg shadow-sm">
              <div className="font-semibold text-gray-900">Zustand</div>
              <div className="text-sm text-gray-800">State Management</div>
            </div>
            <div className="text-center p-3 bg-white rounded-lg shadow-sm">
              <div className="font-semibold text-gray-900">TypeScript</div>
              <div className="text-sm text-gray-800">Type Safety</div>
            </div>
            <div className="text-center p-3 bg-white rounded-lg shadow-sm">
              <div className="font-semibold text-gray-900">Tailwind CSS</div>
              <div className="text-sm text-gray-800">Styling</div>
            </div>
          </div>
        </section>

        <div className="text-center pt-8 border-t">
          <p className="text-sm text-gray-500">
            Jobs feature demo will be available in the next update. Stay tuned!
          </p>
        </div>
      </div>
    </DashboardLayout>
  );
}