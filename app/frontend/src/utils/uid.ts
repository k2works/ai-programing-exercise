export const generateId = (): string => {
  return Math.random().toString(36).substr(2, 9)
}

export const generateUniqueId = (prefix = 'id'): string => {
  const timestamp = Date.now().toString(36)
  const random = Math.random().toString(36).substr(2, 5)
  return `${prefix}-${timestamp}-${random}`
}

export const generateTestId = (): string => {
  return `test-${generateId()}`
}