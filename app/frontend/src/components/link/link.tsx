import { Link as ChakraLink, LinkProps as ChakraLinkProps } from '@chakra-ui/react';
import NextLink from 'next/link';
import { ReactNode } from 'react';

const variants = {
  solid: {
    color: 'primaryAccent',
    _hover: {
      textDecoration: 'none',
      opacity: '0.9',
    },
  },
  outline: {
    color: 'primary',
    _hover: {
      textDecoration: 'underline',
    },
  },
};

export type LinkProps = {
  href: string;
  children: ReactNode;
  variant?: keyof typeof variants;
  icon?: JSX.Element;
} & Omit<ChakraLinkProps, 'href'>;

export const Link = ({ 
  href, 
  children, 
  variant = 'outline', 
  icon, 
  ...props 
}: LinkProps) => {
  const variantStyle = variants[variant as keyof typeof variants];
  
  return (
    <NextLink href={href} passHref legacyBehavior>
      <ChakraLink 
        {...props} 
        {...variantStyle}
        display="inline-flex"
        alignItems="center"
        gap={icon ? 2 : 0}
      >
        {icon}
        {children}
      </ChakraLink>
    </NextLink>
  );
};