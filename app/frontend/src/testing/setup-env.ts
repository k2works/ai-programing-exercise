// Polyfills for Node.js test environment
import { TextEncoder, TextDecoder } from 'util';
import 'whatwg-fetch';

// MSW requires TextEncoder/TextDecoder in Node.js environment  
global.TextEncoder = TextEncoder;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
global.TextDecoder = TextDecoder as any;

// TransformStream polyfill for MSW
global.TransformStream = class TransformStream {
  constructor() {}
// eslint-disable-next-line @typescript-eslint/no-explicit-any
} as any;

// Mock BroadcastChannel for MSW WebSocket support
global.BroadcastChannel = class BroadcastChannel {
  constructor(public name: string) {}
  postMessage() {}
  addEventListener() {}
  removeEventListener() {}
  close() {}
// eslint-disable-next-line @typescript-eslint/no-explicit-any
} as any;

// Mock environment variables
process.env.NEXT_PUBLIC_API_URL = 'http://localhost:3001/api';
process.env.NODE_ENV = 'test';