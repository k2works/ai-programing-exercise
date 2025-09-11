import { Loading } from '../loading';
import { renderSimple, screen } from '@/testing/test-utils-simple';

describe('Loading', () => {
  it('renders loading spinner with default text', () => {
    renderSimple(<Loading />);
    
    expect(screen.getByTestId('loading-spinner')).toBeInTheDocument();
    expect(screen.getByText('Loading...')).toBeInTheDocument();
  });

  it('renders with custom text', () => {
    renderSimple(<Loading text="Please wait..." />);
    
    expect(screen.getByTestId('loading-spinner')).toBeInTheDocument();
    expect(screen.getByText('Please wait...')).toBeInTheDocument();
  });

  it('renders without text when text is empty', () => {
    renderSimple(<Loading text="" />);
    
    expect(screen.getByTestId('loading-spinner')).toBeInTheDocument();
    expect(screen.queryByText('Loading...')).not.toBeInTheDocument();
  });
});