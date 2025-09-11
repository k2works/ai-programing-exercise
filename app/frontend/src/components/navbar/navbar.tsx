'use client';

import { Button } from '@/components/button';
import { Link } from '@/components/link';

const InfoIcon = () => (
  <svg
    className="h-4 w-4"
    fill="none"
    stroke="currentColor"
    viewBox="0 0 24 24"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      strokeLinecap="round"
      strokeLinejoin="round"
      strokeWidth={2}
      d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
    />
  </svg>
);

export const Navbar = () => {
  const handleLogout = () => {
    console.log('Logging Out...');
    // TODO: 実際の認証システムを実装時にログアウト処理を追加
  };

  return (
    <nav className="bg-primary-600 text-white shadow-lg">
      <div className="max-w-6xl mx-auto px-4">
        <div className="flex justify-between items-center py-3">
          {/* Left section */}
          <div className="flex items-center space-x-4">
            {/* Brand */}
            <Link 
              href="/" 
              variant="solid"
              className="bg-primary-600 hover:bg-primary-700 border-none text-white font-bold"
            >
              Jobs App
            </Link>
            
            {/* Navigation Links */}
            <div className="hidden md:flex items-center space-x-1">
              <Link
                href="/dashboard/jobs"
                variant="ghost"
                icon={<InfoIcon />}
                className="text-white hover:bg-primary-700 hover:text-white"
              >
                Jobs
              </Link>
            </div>
          </div>

          {/* Right section */}
          <div className="flex items-center space-x-4">
            <Button
              variant="outline"
              className="bg-transparent text-white border-white hover:bg-white hover:text-primary-600"
              onClick={handleLogout}
            >
              Log Out
            </Button>
          </div>
        </div>
      </div>

      {/* Mobile navigation (hidden by default, would be toggled with state) */}
      <div className="md:hidden border-t border-primary-500">
        <div className="px-4 py-2 space-y-1">
          <Link
            href="/dashboard/jobs"
            variant="ghost"
            icon={<InfoIcon />}
            className="flex items-center w-full px-2 py-2 text-white hover:bg-primary-700 rounded"
          >
            Jobs
          </Link>
        </div>
      </div>
    </nav>
  );
};