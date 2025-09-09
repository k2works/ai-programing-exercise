import HomePage from '../pages/index';
import {
  appRender,
  screen,
  waitFor,
  waitForLoadingToFinish,
} from '@/testing/test-utils';
import { testData } from '@/testing/test-data';

// Next.js router のモック
const mockPush = jest.fn();
jest.mock('next/router', () => ({
  useRouter: () => ({
    push: mockPush,
    pathname: '/',
    query: {},
    asPath: '/',
  }),
}));

describe('HomePage', () => {
  beforeEach(() => {
    mockPush.mockClear();
  });

  it('should render the home page with jobs list', async () => {
    appRender(<HomePage />);

    // ページタイトルが表示される
    expect(
      screen.getByRole('heading', { name: /理想の仕事を見つけよう/i })
    ).toBeInTheDocument();

    // ローディング完了を待つ
    await waitForLoadingToFinish();

    // ジョブリストが表示される
    await waitFor(() => {
      testData.jobs.forEach((job) => {
        expect(screen.getByText(job.position)).toBeInTheDocument();
      });
    });
  });

  it('should display job details in the list', async () => {
    appRender(<HomePage />);

    await waitForLoadingToFinish();

    // 各ジョブの詳細が表示される
    await waitFor(() => {
      testData.jobs.forEach((job) => {
        expect(screen.getByText(job.position)).toBeInTheDocument();
        expect(screen.getByText(job.department)).toBeInTheDocument();
        expect(screen.getByText(job.location)).toBeInTheDocument();
      });
    });
  });

  it('should have navigation links for each job', async () => {
    appRender(<HomePage />);

    await waitForLoadingToFinish();

    // 各ジョブへのリンクが存在する
    await waitFor(() => {
      const viewLinks = screen.getAllByRole('link', { name: /view/i });
      expect(viewLinks).toHaveLength(testData.jobs.length);
    });
  });

  it('should render in a responsive container', async () => {
    const { container } = appRender(<HomePage />);

    // Chakra UIのコンテナが存在する
    const mainContainer = container.querySelector('[class*="container"]');
    expect(mainContainer).toBeInTheDocument();
  });
});
