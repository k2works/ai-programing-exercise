export type LoadingProps = {
  text?: string;
  size?: 'sm' | 'md' | 'lg' | 'xl';
};

const sizeClasses = {
  sm: 'h-4 w-4',
  md: 'h-8 w-8',
  lg: 'h-12 w-12',
  xl: 'h-16 w-16',
};

export const Loading = ({ text = 'Loading...', size = 'md' }: LoadingProps) => {
  return (
    <div className="flex flex-col items-center justify-center min-h-[200px] space-y-4">
      <div className="relative">
        <div
          className={`${sizeClasses[size]} border-4 border-gray-200 border-t-primary-600 rounded-full animate-spin`}
        />
      </div>
      <p className="text-gray-800 text-sm font-medium">{text}</p>
    </div>
  );
};