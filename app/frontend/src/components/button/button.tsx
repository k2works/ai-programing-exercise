import { MouseEventHandler, ReactNode } from 'react';

const variants = {
  solid: {
    base: 'bg-primary-600 text-white border-primary-600',
    hover: 'hover:bg-primary-700 hover:border-primary-700',
    disabled: 'disabled:bg-gray-300 disabled:border-gray-300 disabled:text-gray-500',
  },
  outline: {
    base: 'bg-white text-primary-600 border-primary-600',
    hover: 'hover:bg-primary-50 hover:text-primary-700',
    disabled: 'disabled:bg-gray-50 disabled:border-gray-300 disabled:text-gray-400',
  },
  ghost: {
    base: 'bg-transparent text-primary-600 border-transparent',
    hover: 'hover:bg-primary-50',
    disabled: 'disabled:text-gray-400',
  },
  danger: {
    base: 'bg-red-600 text-white border-red-600',
    hover: 'hover:bg-red-700 hover:border-red-700',
    disabled: 'disabled:bg-gray-300 disabled:border-gray-300 disabled:text-gray-500',
  },
};

const sizes = {
  sm: 'px-3 py-1.5 text-sm',
  md: 'px-4 py-2 text-base',
  lg: 'px-6 py-3 text-lg',
};

export type ButtonProps = {
  children: ReactNode;
  type?: 'button' | 'submit' | 'reset';
  variant?: keyof typeof variants;
  size?: keyof typeof sizes;
  isLoading?: boolean;
  isDisabled?: boolean;
  loadingText?: string;
  onClick?: MouseEventHandler<HTMLButtonElement>;
  icon?: JSX.Element;
  className?: string;
};

export const Button = ({
  variant = 'solid',
  size = 'md',
  type = 'button',
  children,
  icon,
  isLoading = false,
  isDisabled = false,
  loadingText,
  onClick,
  className = '',
  ...props
}: ButtonProps) => {
  const variantClasses = variants[variant];
  const sizeClasses = sizes[size];
  
  const baseClasses = [
    'inline-flex items-center justify-center',
    'border font-medium rounded-lg',
    'transition-all duration-200',
    'focus:outline-none focus:ring-2 focus:ring-primary-500 focus:ring-offset-2',
    sizeClasses,
    variantClasses.base,
    variantClasses.hover,
    variantClasses.disabled,
    className,
  ].join(' ');

  return (
    <button
      {...props}
      type={type}
      onClick={onClick}
      disabled={isDisabled || isLoading}
      className={baseClasses}
    >
      {isLoading && (
        <svg
          className="animate-spin -ml-1 mr-2 h-4 w-4"
          xmlns="http://www.w3.org/2000/svg"
          fill="none"
          viewBox="0 0 24 24"
        >
          <circle
            className="opacity-25"
            cx="12"
            cy="12"
            r="10"
            stroke="currentColor"
            strokeWidth="4"
          />
          <path
            className="opacity-75"
            fill="currentColor"
            d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
          />
        </svg>
      )}
      {icon && !isLoading && (
        <span className={`${children ? 'mr-2' : ''}`}>{icon}</span>
      )}
      {isLoading && loadingText ? loadingText : children}
    </button>
  );
};