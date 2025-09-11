import { ReactNode } from 'react';
import { Link } from '@/components/link';

type PublicLayoutProps = {
  children: ReactNode;
};

export const PublicLayout = ({
  children,
}: PublicLayoutProps) => {
  return (
    <div className="max-w-6xl mx-auto min-h-screen flex flex-col">
      <main className="flex-1 mx-4 py-8">
        {children}
      </main>
      
      <footer className="py-8 text-center border-t border-gray-200">
        <p className="text-gray-600">
          Powered by{' '}
          <Link href="/" className="font-medium">
            Jobs App
          </Link>
        </p>
      </footer>
    </div>
  );
};