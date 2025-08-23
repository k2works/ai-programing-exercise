/** @type {import('tailwindcss').Config} */
export default {
  content: ['./index.html', './src/**/*.{js,ts,jsx,tsx}'],
  theme: {
    extend: {
      colors: {
        puyo: {
          red: '#ff4444',
          blue: '#4444ff',
          green: '#44ff44',
          yellow: '#ffff44',
          purple: '#ff44ff',
        },
      },
      gridTemplateColumns: {
        'puyo-field': 'repeat(6, minmax(0, 1fr))',
      },
      gridTemplateRows: {
        'puyo-field': 'repeat(12, minmax(0, 1fr))',
      },
    },
  },
  plugins: [],
};
