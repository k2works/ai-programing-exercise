import Head from 'next/head';

export type SEOProps = {
  title?: string;
  description?: string;
  image?: string;
  url?: string;
};

export const SEO = ({
  title = 'Jobs App',
  description = 'Find your next opportunity',
  image = '/default-og.jpg',
  url = 'https://jobs-app.vercel.app',
}: SEOProps) => {
  const fullTitle = title === 'Jobs App' ? title : `${title} - Jobs App`;

  return (
    <Head>
      <title>{fullTitle}</title>
      <meta name="description" content={description} />
      <meta property="og:title" content={fullTitle} />
      <meta property="og:description" content={description} />
      <meta property="og:image" content={image} />
      <meta property="og:url" content={url} />
      <meta name="twitter:card" content="summary_large_image" />
      <meta name="twitter:title" content={fullTitle} />
      <meta name="twitter:description" content={description} />
      <meta name="twitter:image" content={image} />
      <meta name="viewport" content="width=device-width, initial-scale=1" />
    </Head>
  );
};
