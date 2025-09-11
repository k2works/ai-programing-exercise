import { ReactNode } from 'react';
import { Navbar } from '@/components/navbar';
import { Link } from '@/components/link';

type DashboardLayoutProps = {
  children: ReactNode;
};

export const DashboardLayout = ({
  children,
}: DashboardLayoutProps) => {
  // TODO: 実際の認証システムが実装されたら useUser Hook を追加
  const mockOrganizationId = '1';

  return (
    <section className="min-h-screen flex flex-col">
      <Navbar />
      
      <main className="flex-1 max-w-6xl mx-auto w-full px-4 py-12">
        {children}
      </main>
      
      <footer className="py-8 text-center border-t border-gray-200 bg-gray-50">
        <Link
          href={`/organizations/${mockOrganizationId}`}
          className="text-primary-600 hover:text-primary-700"
        >
          View Public Organization Page
        </Link>
      </footer>
    </section>
  );
};