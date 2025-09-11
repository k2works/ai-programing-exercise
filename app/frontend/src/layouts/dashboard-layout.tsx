import { ReactNode } from 'react';
import { Navbar } from '@/components/navbar';
import { Link } from '@/components/link';
import { useUser } from '@/testing/test-data';

type DashboardLayoutProps = {
  children: ReactNode;
};

export const DashboardLayout = ({
  children,
}: DashboardLayoutProps) => {
  const user = useUser();

  return (
    <section className="min-h-screen flex flex-col">
      <Navbar />
      
      <main className="flex-1 max-w-6xl mx-auto w-full px-4 py-12">
        {children}
      </main>
      
      <footer className="py-8 text-center border-t border-gray-200 bg-gray-50">
        <Link
          href={`/organizations/${user.data?.organizationId}`}
          className="text-primary-600 hover:text-primary-700"
        >
          View Public Organization Page
        </Link>
      </footer>
    </section>
  );
};