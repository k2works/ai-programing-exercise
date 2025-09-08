import { rest } from 'msw';
import { API_URL } from '@/config/constants';
import { db } from '../seed-db';

export const jobsHandlers = [
  rest.get(`${API_URL}/jobs`, (req, res, ctx) => {
    const organizationId = req.url.searchParams.get('organizationId');
    
    const jobs = organizationId
      ? db.job.findMany({
          where: {
            organizationId: {
              equals: organizationId,
            },
          },
        })
      : db.job.getAll();

    return res(ctx.json(jobs));
  }),

  rest.get(`${API_URL}/jobs/:id`, (req, res, ctx) => {
    const { id } = req.params;
    
    const job = db.job.findFirst({
      where: {
        id: {
          equals: id as string,
        },
      },
    });

    if (!job) {
      return res(
        ctx.status(404),
        ctx.json({ message: 'Job not found' })
      );
    }

    return res(ctx.json(job));
  }),

  rest.post(`${API_URL}/jobs`, async (req, res, ctx) => {
    const jobData = await req.json() as any;
    
    const newJob = db.job.create({
      ...(jobData as object),
      id: String(Date.now()),
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    });

    return res(
      ctx.status(201),
      ctx.json(newJob)
    );
  }),

  rest.patch(`${API_URL}/jobs/:id`, async (req, res, ctx) => {
    const { id } = req.params;
    const updates = await req.json() as any;
    
    const job = db.job.findFirst({
      where: {
        id: {
          equals: id as string,
        },
      },
    });

    if (!job) {
      return res(
        ctx.status(404),
        ctx.json({ message: 'Job not found' })
      );
    }

    const updatedJob = db.job.update({
      where: {
        id: {
          equals: id as string,
        },
      },
      data: {
        ...(updates as object),
        updatedAt: new Date().toISOString(),
      },
    });

    return res(ctx.json(updatedJob));
  }),

  rest.delete(`${API_URL}/jobs/:id`, (req, res, ctx) => {
    const { id } = req.params;
    
    const job = db.job.findFirst({
      where: {
        id: {
          equals: id as string,
        },
      },
    });

    if (!job) {
      return res(
        ctx.status(404),
        ctx.json({ message: 'Job not found' })
      );
    }

    db.job.delete({
      where: {
        id: {
          equals: id as string,
        },
      },
    });

    return res(ctx.status(204));
  }),
];