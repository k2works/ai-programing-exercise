'use client';

import dynamic from 'next/dynamic';

// Dynamically import the actual component to avoid SSR issues with localStorage
const ClientStateDemo = dynamic(() => import('./client-state-demo'), {
  ssr: false,
  loading: () => <div className="flex justify-center items-center min-h-[400px]">Loading...</div>
});

export default function ClientStatePage() {
  return <ClientStateDemo />;
}