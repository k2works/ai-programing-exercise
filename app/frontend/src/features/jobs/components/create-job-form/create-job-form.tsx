'use client';

import { useForm } from 'react-hook-form';

import { Button } from '@/components/button';
import { FormField } from '@/components/form';

import { CreateJobData } from '../../types';

export type CreateJobFormProps = {
  onSubmit: (data: CreateJobData) => void;
  isLoading?: boolean;
};

export const CreateJobForm = ({
  onSubmit,
  isLoading = false,
}: CreateJobFormProps) => {
  const {
    register,
    handleSubmit,
    formState: { errors },
  } = useForm<CreateJobData>();

  return (
    <div className="max-w-2xl mx-auto">
      <form onSubmit={handleSubmit(onSubmit)} className="space-y-6">
        <FormField
          label="Position"
          placeholder="e.g. Senior Frontend Developer"
          error={errors.position}
          {...register('position', { 
            required: 'Position is required',
            minLength: {
              value: 2,
              message: 'Position must be at least 2 characters'
            }
          })}
        />
        
        <FormField
          label="Department"
          placeholder="e.g. Engineering"
          error={errors.department}
          {...register('department', { 
            required: 'Department is required',
            minLength: {
              value: 2,
              message: 'Department must be at least 2 characters'
            }
          })}
        />
        
        <FormField
          label="Location"
          placeholder="e.g. Remote, Tokyo, San Francisco"
          error={errors.location}
          {...register('location', { 
            required: 'Location is required',
            minLength: {
              value: 2,
              message: 'Location must be at least 2 characters'
            }
          })}
        />
        
        <FormField
          type="textarea"
          label="Job Information"
          placeholder="Describe the job responsibilities, requirements, and benefits..."
          error={errors.info}
          {...register('info', { 
            required: 'Job information is required',
            minLength: {
              value: 10,
              message: 'Job information must be at least 10 characters'
            }
          })}
        />
        
        <div className="flex justify-end space-x-4">
          <Button
            type="button"
            variant="outline"
            onClick={() => window.history.back()}
          >
            Cancel
          </Button>
          <Button
            type="submit"
            isLoading={isLoading}
            size="lg"
          >
            Create Job
          </Button>
        </div>
      </form>
    </div>
  );
};