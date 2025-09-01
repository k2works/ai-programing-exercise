import type { Metadata } from "next";
import { Geist, Geist_Mono } from "next/font/google";
import DynamicAuthProvider from "@/components/dynamic-auth-provider";
import "./globals.css";

const geistSans = Geist({
  variable: "--font-geist-sans",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  title: "会議室予約システム",
  description: "会議室の予約管理を簡単に",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="ja" suppressHydrationWarning>
      <body
        className={`${geistSans.variable} ${geistMono.variable} antialiased`}
        suppressHydrationWarning
      >
        <div suppressHydrationWarning>
          <DynamicAuthProvider>{children}</DynamicAuthProvider>
        </div>
      </body>
    </html>
  );
}
