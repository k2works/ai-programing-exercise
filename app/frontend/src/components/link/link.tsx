import NextLink from 'next/link';
import { ReactNode } from 'react';

const variants = {
  default: {
    base: 'text-primary-600',
    hover: 'hover:text-primary-700 hover:underline',
    focus: 'focus:outline-none focus:ring-2 focus:ring-primary-500 focus:ring-offset-2 focus:rounded',
  },
  solid: {
    base: 'inline-flex items-center px-4 py-2 bg-primary-600 text-white font-medium rounded-lg border border-primary-600',
    hover: 'hover:bg-primary-700 hover:border-primary-700',
    focus: 'focus:outline-none focus:ring-2 focus:ring-primary-500 focus:ring-offset-2',
  },
  outline: {
    base: 'inline-flex items-center px-4 py-2 bg-white text-primary-600 font-medium rounded-lg border border-primary-600',
    hover: 'hover:bg-primary-50 hover:text-primary-700',
    focus: 'focus:outline-none focus:ring-2 focus:ring-primary-500 focus:ring-offset-2',
  },
  ghost: {
    base: 'inline-flex items-center px-2 py-1 text-primary-600',
    hover: 'hover:bg-primary-50 hover:rounded',
    focus: 'focus:outline-none focus:ring-2 focus:ring-primary-500 focus:ring-offset-2 focus:rounded',
  },
};

export type LinkProps = {
  href: string;
  children: ReactNode;
  variant?: keyof typeof variants;
  icon?: JSX.Element;
  external?: boolean;
  className?: string;
};

export const Link = ({
  href,
  children,
  variant = 'default',
  icon,
  external = false,
  className = '',
  ...props
}: LinkProps) => {
  const variantClasses = variants[variant];
  
  const baseClasses = [
    'transition-all duration-200',
    'no-underline',
    variantClasses.base,
    variantClasses.hover,
    variantClasses.focus,
    className,
  ].join(' ');

  const linkContent = (
    <span className="inline-flex items-center">
      {icon && <span className={`${children ? 'mr-2' : ''}`}>{icon}</span>}
      {children}
      {external && (
        <svg
          className="ml-1 h-4 w-4"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
          xmlns="http://www.w3.org/2000/svg"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14"
          />
        </svg>
      )}
    </span>
  );

  if (external) {
    return (
      <a
        {...props}
        href={href}
        target="_blank"
        rel="noopener noreferrer"
        className={baseClasses}
      >
        {linkContent}
      </a>
    );
  }

  return (
    <NextLink href={href} passHref legacyBehavior>
      <a {...props} className={baseClasses}>
        {linkContent}
      </a>
    </NextLink>
  );
};