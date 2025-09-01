import LoginForm from '@/components/login-form';

export default function LoginPage() {
  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 flex items-center justify-center py-12">
      <div className="container mx-auto px-4">
        <h1 className="text-4xl font-bold text-center mb-12 text-gray-800">会議室予約システム</h1>
        <LoginForm />
      </div>
    </div>
  );
}