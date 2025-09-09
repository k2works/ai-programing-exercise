// Chakra UI テーマの型拡張
declare module '@chakra-ui/react' {
  interface Theme {
    colors: {
      primary: string;
      primaryAccent: string;
      [key: string]: any;
    };
  }
}

// カスタムコンポーネント Props の型定義
export interface CustomButtonProps {
  variant?: 'primary' | 'secondary' | 'outline';
  size?: 'sm' | 'md' | 'lg';
}

export interface CustomThemeConfig {
  colors: {
    primary: string;
    primaryAccent: string;
  };
  styles: {
    global: Record<string, any>;
  };
}
