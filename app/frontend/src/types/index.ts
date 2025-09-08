export interface User {
  id: string
  email: string
  name: string
  role: UserRole
  organizationId?: string
  createdAt: string
  updatedAt: string
}

export type UserRole = 'job_seeker' | 'hr_staff' | 'organization_admin' | 'system_admin'

export interface Job {
  id: string
  title: string
  description: string
  requirements: string[]
  location: string
  employmentType: EmploymentType
  salaryRange: SalaryRange
  organizationId: string
  status: JobStatus
  createdAt: string
  updatedAt: string
}

export type EmploymentType = 'full_time' | 'contract' | 'part_time' | 'internship' | 'remote'

export type JobStatus = 'draft' | 'published' | 'archived' | 'closed'

export interface SalaryRange {
  min: number
  max: number
  currency: string
}

export interface Organization {
  id: string
  name: string
  description: string
  logo?: string
  website?: string
  location: string
  createdAt: string
  updatedAt: string
}

export interface Application {
  id: string
  jobId: string
  applicantId: string
  status: ApplicationStatus
  appliedAt: string
  updatedAt: string
}

export type ApplicationStatus = 'pending' | 'reviewed' | 'interview' | 'rejected' | 'accepted'

export interface ApiResponse<T> {
  data: T
  message?: string
  success: boolean
}

export interface PaginatedResponse<T> {
  data: T[]
  pagination: {
    page: number
    limit: number
    total: number
    pages: number
  }
}

export interface ApiError {
  message: string
  code?: string
  details?: Record<string, string[]>
}