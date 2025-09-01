'use client';

import { useEffect } from 'react';
import { useRouter, usePathname } from 'next/navigation';
import { setAuthToken } from '@/lib/api-client';

const PUBLIC_PATHS = ['/login', '/'];

export default function AuthProvider({ children }: { children: React.ReactNode }) {
  const router = useRouter();
  const pathname = usePathname();

  useEffect(() => {
    const token = localStorage.getItem('auth_token');
    
    if (token) {
      setAuthToken(token);
    } else if (!PUBLIC_PATHS.includes(pathname)) {
      router.push('/login');
    }
  }, [pathname, router]);

  return <>{children}</>;
}