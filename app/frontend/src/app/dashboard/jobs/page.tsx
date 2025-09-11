'use client';

import { DashboardLayout } from '@/layouts';
import { Button, DataTable, Loading } from '@/components';
import { JobsList, CreateJobForm } from '@/features/jobs';
import { useJobs } from '@/features/jobs/api';
import { useState } from 'react';

export default function JobsDashboardPage() {
  const [isCreateModalOpen, setIsCreateModalOpen] = useState(false);
  const { data: jobs, isLoading, error } = useJobs();

  return (
    <DashboardLayout>
      <div className="space-y-6">
        {/* Header */}
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold text-gray-900">
              Jobs Dashboard
            </h1>
            <p className="text-gray-800">
              Manage your job postings and applications
            </p>
          </div>
          <Button 
            onClick={() => setIsCreateModalOpen(true)}
            icon={
              <svg className="h-4 w-4" fill="currentColor" viewBox="0 0 20 20">
                <path fillRule="evenodd" d="M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z" clipRule="evenodd" />
              </svg>
            }
          >
            Create Job
          </Button>
        </div>

        {/* Stats Cards */}
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
          <div className="bg-white p-4 rounded-lg border">
            <div className="flex items-center">
              <div className="p-2 bg-blue-100 rounded-lg">
                <svg className="h-6 w-6 text-blue-600" fill="currentColor" viewBox="0 0 20 20">
                  <path d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
                </svg>
              </div>
              <div className="ml-4">
                <p className="text-sm font-medium text-gray-800">Total Jobs</p>
                <p className="text-2xl font-bold text-gray-900">{jobs?.length || 0}</p>
              </div>
            </div>
          </div>

          <div className="bg-white p-4 rounded-lg border">
            <div className="flex items-center">
              <div className="p-2 bg-green-100 rounded-lg">
                <svg className="h-6 w-6 text-green-600" fill="currentColor" viewBox="0 0 20 20">
                  <path fillRule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-4">
                <p className="text-sm font-medium text-gray-800">Active Jobs</p>
                <p className="text-2xl font-bold text-gray-900">{jobs?.filter(job => job.status === 'active')?.length || 0}</p>
              </div>
            </div>
          </div>

          <div className="bg-white p-4 rounded-lg border">
            <div className="flex items-center">
              <div className="p-2 bg-yellow-100 rounded-lg">
                <svg className="h-6 w-6 text-yellow-600" fill="currentColor" viewBox="0 0 20 20">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-4">
                <p className="text-sm font-medium text-gray-800">Draft Jobs</p>
                <p className="text-2xl font-bold text-gray-900">{jobs?.filter(job => job.status === 'draft')?.length || 0}</p>
              </div>
            </div>
          </div>

          <div className="bg-white p-4 rounded-lg border">
            <div className="flex items-center">
              <div className="p-2 bg-red-100 rounded-lg">
                <svg className="h-6 w-6 text-red-600" fill="currentColor" viewBox="0 0 20 20">
                  <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-4">
                <p className="text-sm font-medium text-gray-800">Closed Jobs</p>
                <p className="text-2xl font-bold text-gray-900">{jobs?.filter(job => job.status === 'closed')?.length || 0}</p>
              </div>
            </div>
          </div>
        </div>

        {/* Jobs List */}
        <div className="bg-white rounded-lg border">
          <div className="p-6">
            <h2 className="text-lg font-semibold text-gray-900 mb-4">All Jobs</h2>
            {isLoading ? (
              <div className="flex justify-center py-8">
                <Loading text="Loading jobs..." />
              </div>
            ) : error ? (
              <div className="text-center py-8">
                <p className="text-red-600">Error loading jobs: {error.message}</p>
                <Button 
                  variant="outline" 
                  className="mt-4"
                  onClick={() => window.location.reload()}
                >
                  Retry
                </Button>
              </div>
            ) : (
              <JobsList jobs={jobs || []} />
            )}
          </div>
        </div>

        {/* Create Job Modal */}
        {isCreateModalOpen && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
            <div className="bg-white rounded-lg p-6 w-full max-w-2xl max-h-screen overflow-y-auto">
              <div className="flex justify-between items-center mb-4">
                <h2 className="text-xl font-bold text-gray-900">Create New Job</h2>
                <Button
                  variant="ghost"
                  onClick={() => setIsCreateModalOpen(false)}
                >
                  <svg className="h-6 w-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M6 18L18 6M6 6l12 12" />
                  </svg>
                </Button>
              </div>
              <CreateJobForm
                onSuccess={() => {
                  setIsCreateModalOpen(false);
                }}
                onCancel={() => setIsCreateModalOpen(false)}
              />
            </div>
          </div>
        )}
      </div>
    </DashboardLayout>
  );
}