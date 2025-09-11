import { Link } from '@/components/link';

export type NotFoundProps = {
  title?: string;
  message?: string;
  actionLabel?: string;
  actionHref?: string;
};

export const NotFound = ({
  title = 'ページが見つかりません',
  message = 'お探しのページは存在しないか、移動または削除された可能性があります。',
  actionLabel = 'ホームに戻る',
  actionHref = '/',
}: NotFoundProps) => {
  return (
    <div className="min-h-[400px] flex flex-col items-center justify-center text-center space-y-6">
      {/* 404 Icon */}
      <div className="relative">
        <div className="text-8xl font-bold text-gray-200 select-none">404</div>
        <div className="absolute inset-0 flex items-center justify-center">
          <svg
            className="h-16 w-16 text-primary-600"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
            xmlns="http://www.w3.org/2000/svg"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M9.172 16.172a4 4 0 015.656 0M9 12h6m-3-7a9 9 0 019 9 9 9 0 01-9 9 9 9 0 01-9-9 9 9 0 019-9z"
            />
          </svg>
        </div>
      </div>

      {/* Content */}
      <div className="space-y-3 max-w-md">
        <h1 className="text-2xl font-bold text-gray-900">{title}</h1>
        <p className="text-gray-600 leading-relaxed">{message}</p>
      </div>

      {/* Action Button */}
      <div className="space-y-4">
        <Link href={actionHref} variant="solid">
          {actionLabel}
        </Link>
        
        <div className="text-sm text-gray-500">
          または{' '}
          <button
            onClick={() => window.history.back()}
            className="text-primary-600 hover:text-primary-700 underline"
          >
            前のページに戻る
          </button>
        </div>
      </div>
    </div>
  );
};