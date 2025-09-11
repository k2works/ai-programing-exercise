export default function Home() {
  return (
    <div className="min-h-screen flex items-center justify-center bg-gray-50">
      <div className="max-w-4xl mx-auto px-6 text-center">
        <div className="space-y-8">
          <div className="space-y-4">
            <div className="inline-flex items-center px-4 py-2 rounded-full bg-primary-100 text-primary-700 text-sm font-medium">
              Chapter 3 - UI Integration Complete
            </div>
            <h1 className="text-5xl font-bold text-primary-700 tracking-tight">
              React Job Board Application
            </h1>
            <p className="text-xl text-gray-600 max-w-2xl mx-auto">
              Tailwind CSS によるデザインシステム基盤構築完了
            </p>
          </div>
          
          <div className="space-y-6">
            <div className="text-lg text-gray-700 leading-relaxed">
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4 max-w-2xl mx-auto">
                <div className="flex items-center space-x-2">
                  <span className="text-success">✅</span>
                  <span>Next.js 14 + TypeScript 基盤</span>
                </div>
                <div className="flex items-center space-x-2">
                  <span className="text-success">✅</span>
                  <span>Tailwind CSS デザインシステム統合</span>
                </div>
                <div className="flex items-center space-x-2">
                  <span className="text-success">✅</span>
                  <span>カスタムテーマシステム実装</span>
                </div>
                <div className="flex items-center space-x-2">
                  <span className="text-success">✅</span>
                  <span>レスポンシブデザイン対応</span>
                </div>
              </div>
            </div>
            
            <button 
              className="inline-flex items-center px-8 py-4 rounded-lg bg-primary-600 text-white font-semibold 
                         hover:bg-primary-700 hover:-translate-y-1 hover:shadow-medium 
                         transition-all duration-200 text-lg"
            >
              求人検索を開始
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
