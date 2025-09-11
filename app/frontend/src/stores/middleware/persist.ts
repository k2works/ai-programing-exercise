import { StateCreator, StoreMutatorIdentifier } from 'zustand';

export interface PersistOptions {
  name: string;
  storage?: 'localStorage' | 'sessionStorage';
}

type PersistImpl = <T>(
  storeInitializer: StateCreator<T, [], [], T>,
  options: PersistOptions
) => StateCreator<T, [], [], T>;

const persistImpl: PersistImpl = (config, options) => (set, get, store) => {
  const storage = options.storage === 'sessionStorage' 
    ? sessionStorage 
    : localStorage;

  let hasInitialized = false;
  let initialStoredState: Record<string, unknown> = {};

  // Load stored state first, before creating the store
  try {
    const stored = storage.getItem(options.name);
    if (stored) {
      initialStoredState = JSON.parse(stored);
    }
  } catch (error) {
    console.warn('Failed to parse stored state:', error);
  }

  const persistedSet = (partial: unknown, replace?: boolean) => {
    set(partial as Parameters<typeof set>[0], replace as Parameters<typeof set>[1]);
    
    if (hasInitialized) {
      try {
        const state = get();
        storage.setItem(options.name, JSON.stringify(state));
      } catch (error) {
        console.warn(`Failed to persist state to ${storage}:`, error);
      }
    }
  };

  // Create initial state with stored values merged in
  config(persistedSet, get, store);
  
  // Use set to properly initialize with stored state
  if (Object.keys(initialStoredState).length > 0) {
    // Filter out functions from stored state to avoid overriding actions
    const stateToRestore = Object.fromEntries(
      Object.entries(initialStoredState).filter(([, value]) => typeof value !== 'function')
    );
    set(stateToRestore as Parameters<typeof set>[0], false);
  }

  hasInitialized = true;
  return get(); // Return current state after initialization
};

type Persist = <
  T,
  Mps extends [StoreMutatorIdentifier, unknown][] = [],
  Mcs extends [StoreMutatorIdentifier, unknown][] = []
>(
  initializer: StateCreator<T, Mps, Mcs>,
  options: PersistOptions
) => StateCreator<T, Mps, Mcs>;

export const persist = persistImpl as unknown as Persist;