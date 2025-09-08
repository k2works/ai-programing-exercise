// Mock for react-error-boundary
import React from 'react';

export const ErrorBoundary = ({ children, fallback, FallbackComponent, fallbackRender, onError }) => {
  return React.createElement('div', { 'data-testid': 'error-boundary' }, children);
};

export const useErrorHandler = () => {
  return (error) => {
    console.error('Error caught by useErrorHandler:', error);
  };
};

export const withErrorBoundary = (Component, errorBoundaryConfig) => {
  const WrappedComponent = (props) => {
    return React.createElement(ErrorBoundary, errorBoundaryConfig, 
      React.createElement(Component, props)
    );
  };
  WrappedComponent.displayName = `withErrorBoundary(${Component.displayName || Component.name || 'Component'})`;
  return WrappedComponent;
};