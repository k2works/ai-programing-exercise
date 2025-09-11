import React, { forwardRef } from 'react';
import { FieldError, UseFormRegister } from 'react-hook-form';

export type FormFieldProps = {
  type?: 'text' | 'email' | 'password' | 'textarea';
  label?: string;
  error?: FieldError;
  placeholder?: string;
  className?: string;
} & Partial<ReturnType<UseFormRegister<Record<string, unknown>>>>;

export const FormField = forwardRef<
  HTMLInputElement | HTMLTextAreaElement,
  FormFieldProps
>((props, ref) => {
  const {
    type = 'text',
    label,
    error,
    placeholder,
    className = '',
    ...inputProps
  } = props;

  const baseClasses = [
    'w-full px-3 py-2 border rounded-lg',
    'focus:outline-none focus:ring-2 focus:ring-primary-500 focus:border-primary-500',
    'transition-colors duration-200',
    error 
      ? 'border-red-500 bg-red-50' 
      : 'border-gray-300 bg-white hover:border-gray-400',
    className
  ].join(' ');

  return (
    <div className="space-y-1">
      {label && (
        <label className="block text-sm font-medium text-gray-700">
          {label}
        </label>
      )}
      
      {type === 'textarea' ? (
        <textarea
          className={`${baseClasses} resize-vertical`}
          rows={6}
          placeholder={placeholder}
          {...inputProps}
          ref={ref as React.Ref<HTMLTextAreaElement>}
        />
      ) : (
        <input
          type={type}
          className={baseClasses}
          placeholder={placeholder}
          {...inputProps}
          ref={ref as React.Ref<HTMLInputElement>}
        />
      )}
      
      {error && (
        <p className="text-sm text-red-600">
          {error.message}
        </p>
      )}
    </div>
  );
});

FormField.displayName = 'FormField';