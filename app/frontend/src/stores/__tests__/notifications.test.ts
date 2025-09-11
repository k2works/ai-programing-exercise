/**
 * @jest-environment jsdom
 */
import { act, renderHook } from '@testing-library/react';
import { notificationsStore, useNotifications } from '../notifications';

describe('Notifications Store', () => {
  beforeEach(() => {
    // Reset store before each test
    notificationsStore.setState({ notifications: [] });
    jest.clearAllTimers();
    jest.useFakeTimers();
  });

  afterEach(() => {
    jest.runOnlyPendingTimers();
    jest.useRealTimers();
  });

  it('should add notification', () => {
    const { result } = renderHook(() => useNotifications());

    act(() => {
      result.current.showNotification({
        type: 'success',
        title: 'Test notification',
        message: 'This is a test message',
      });
    });

    expect(result.current.notifications).toHaveLength(1);
    expect(result.current.notifications[0].title).toBe('Test notification');
    expect(result.current.notifications[0].message).toBe('This is a test message');
    expect(result.current.notifications[0].type).toBe('success');
    expect(result.current.notifications[0].id).toBeDefined();
  });

  it('should dismiss notification', () => {
    const { result } = renderHook(() => useNotifications());

    act(() => {
      result.current.showNotification({
        type: 'info',
        title: 'Test',
      });
    });

    const notificationId = result.current.notifications[0].id;

    act(() => {
      result.current.dismissNotification(notificationId);
    });

    expect(result.current.notifications).toHaveLength(0);
  });

  it('should auto-dismiss notification with duration', () => {
    const { result } = renderHook(() => useNotifications());

    act(() => {
      result.current.showNotification({
        type: 'info',
        title: 'Auto dismiss',
        duration: 1000,
      });
    });

    expect(result.current.notifications).toHaveLength(1);

    act(() => {
      jest.advanceTimersByTime(1000);
    });

    expect(result.current.notifications).toHaveLength(0);
  });

  it('should handle multiple notifications', () => {
    const { result } = renderHook(() => useNotifications());

    act(() => {
      result.current.showNotification({
        type: 'success',
        title: 'First',
      });
      result.current.showNotification({
        type: 'error',
        title: 'Second',
      });
      result.current.showNotification({
        type: 'warning',
        title: 'Third',
      });
    });

    expect(result.current.notifications).toHaveLength(3);
    expect(result.current.notifications[0].title).toBe('First');
    expect(result.current.notifications[1].title).toBe('Second');
    expect(result.current.notifications[2].title).toBe('Third');
  });

  it('should maintain immutability when adding notifications', () => {
    const { result } = renderHook(() => useNotifications());

    act(() => {
      result.current.showNotification({
        type: 'info',
        title: 'First notification',
      });
    });

    const firstState = result.current.notifications;

    act(() => {
      result.current.showNotification({
        type: 'success',
        title: 'Second notification',
      });
    });

    const secondState = result.current.notifications;

    expect(firstState).not.toBe(secondState);
    expect(firstState).toHaveLength(1);
    expect(secondState).toHaveLength(2);
  });

  it('should not affect other notifications when dismissing one', () => {
    const { result } = renderHook(() => useNotifications());

    act(() => {
      result.current.showNotification({
        type: 'info',
        title: 'Keep me',
      });
      result.current.showNotification({
        type: 'error',
        title: 'Remove me',
      });
    });

    const removeId = result.current.notifications.find(
      n => n.title === 'Remove me'
    )?.id;

    act(() => {
      result.current.dismissNotification(removeId!);
    });

    expect(result.current.notifications).toHaveLength(1);
    expect(result.current.notifications[0].title).toBe('Keep me');
  });
});