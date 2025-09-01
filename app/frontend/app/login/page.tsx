import LoginForm from '@/components/login-form';

export default function LoginPage() {
  return (
    <div className="min-h-screen bg-gray-50 flex items-center justify-center py-12">
      <div className="container mx-auto px-4">
        <h1 className="text-3xl font-bold text-center mb-8">会議室予約システム</h1>
        <LoginForm />
      </div>
    </div>
  );
}