'use client';

import dynamic from 'next/dynamic';
import { ReactNode } from 'react';

const NoSsr = ({ children }: { children: ReactNode }) => {
  return <>{children}</>;
};

export default dynamic(() => Promise.resolve(NoSsr), {
  ssr: false,
  loading: () => (
    <div className="min-h-screen bg-gray-50 flex items-center justify-center">
      <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
    </div>
  ),
});