import { StateCreator, StoreMutatorIdentifier } from 'zustand';

export interface DevtoolsOptions {
  name?: string;
  enabled?: boolean;
}

type DevtoolsImpl = <T>(
  storeInitializer: StateCreator<T, [], [], T>,
  options?: DevtoolsOptions
) => StateCreator<T, [], [], T>;

const devtoolsImpl: DevtoolsImpl = (config, options = {}) => (set, get, store) => {
  const { name = 'zustand', enabled = process.env.NODE_ENV === 'development' } = options;

  if (!enabled || typeof window === 'undefined' || !window.__REDUX_DEVTOOLS_EXTENSION__) {
    return config(set, get, store);
  }

  const devtools = window.__REDUX_DEVTOOLS_EXTENSION__.connect({
    name,
    trace: true,
  });

  const initialState = config(
    (partial, replace) => {
      if (replace) {
        set(partial as Parameters<typeof set>[0], true);
      } else {
        set(partial as Parameters<typeof set>[0], false);
      }
      
      // Get the updated state for DevTools
      const currentState = get();
      
      devtools.send(
        'setState',
        currentState
      );
    },
    get,
    store
  );

  devtools.init(initialState);

  // Listen for devtools actions (time travel)
  const unsubscribe = devtools.subscribe((message: { type: string; payload?: { type: string }; state?: string }) => {
    if (message.type === 'DISPATCH') {
      switch (message.payload?.type) {
        case 'RESET':
          devtools.init(initialState);
          set(initialState, true);
          break;
        case 'COMMIT':
          devtools.init(get());
          break;
        case 'ROLLBACK':
          devtools.init(initialState);
          set(initialState, true);
          break;
        case 'JUMP_TO_STATE':
        case 'JUMP_TO_ACTION':
          if (message.state) {
            set(JSON.parse(message.state), true);
          }
          break;
      }
    }
  });

  // Clean up on store destruction
  (store as typeof store & { destroy?: () => void }).destroy = () => {
    unsubscribe();
  };

  return initialState;
};

type Devtools = <
  T,
  Mps extends [StoreMutatorIdentifier, unknown][] = [],
  Mcs extends [StoreMutatorIdentifier, unknown][] = []
>(
  initializer: StateCreator<T, Mps, Mcs>,
  options?: DevtoolsOptions
) => StateCreator<T, Mps, Mcs>;

export const devtools = devtoolsImpl as unknown as Devtools;

// Type declaration for Redux DevTools Extension
declare global {
  interface Window {
    __REDUX_DEVTOOLS_EXTENSION__?: {
      connect: (options: { name: string; trace: boolean }) => {
        init: (state: unknown) => void;
        send: (action: string, state: unknown) => void;
        subscribe: (listener: (message: { type: string; payload?: { type: string }; state?: string }) => void) => () => void;
      };
    };
  }
}