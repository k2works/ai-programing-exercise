import React from 'react';

const App: React.FC = () => {
  return (
    <div className="min-h-screen bg-gray-100 flex items-center justify-center">
      <div className="text-center">
        <h1 className="text-4xl font-bold text-gray-800 mb-4">
          ぷよぷよゲーム
        </h1>
        <p className="text-gray-600">
          TypeScript + React で実装されたぷよぷよゲーム
        </p>
        <p className="text-sm text-gray-500 mt-2">開発中...</p>
      </div>
    </div>
  );
};

export default App;
