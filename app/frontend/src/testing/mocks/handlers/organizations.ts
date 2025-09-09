import { rest } from 'msw';
import { API_URL } from '@/config/constants';
import { db } from '../seed-db';

export const organizationsHandlers = [
  rest.get(`${API_URL}/organizations`, (req, res, ctx) => {
    const organizations = db.organization.getAll();

    return res(ctx.json(organizations));
  }),

  rest.get(`${API_URL}/organizations/:id`, (req, res, ctx) => {
    const { id } = req.params;

    const organization = db.organization.findFirst({
      where: {
        id: {
          equals: id as string,
        },
      },
    });

    if (!organization) {
      return res(
        ctx.status(404),
        ctx.json({ message: 'Organization not found' })
      );
    }

    return res(ctx.json(organization));
  }),

  rest.post(`${API_URL}/organizations`, async (req, res, ctx) => {
    const orgData = (await req.json()) as any;

    const newOrg = db.organization.create({
      ...(orgData as object),
      id: String(Date.now()),
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    });

    return res(ctx.status(201), ctx.json(newOrg));
  }),

  rest.patch(`${API_URL}/organizations/:id`, async (req, res, ctx) => {
    const { id } = req.params;
    const updates = (await req.json()) as any;

    const organization = db.organization.findFirst({
      where: {
        id: {
          equals: id as string,
        },
      },
    });

    if (!organization) {
      return res(
        ctx.status(404),
        ctx.json({ message: 'Organization not found' })
      );
    }

    const updatedOrg = db.organization.update({
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

    return res(ctx.json(updatedOrg));
  }),

  rest.delete(`${API_URL}/organizations/:id`, (req, res, ctx) => {
    const { id } = req.params;

    const organization = db.organization.findFirst({
      where: {
        id: {
          equals: id as string,
        },
      },
    });

    if (!organization) {
      return res(
        ctx.status(404),
        ctx.json({ message: 'Organization not found' })
      );
    }

    db.organization.delete({
      where: {
        id: {
          equals: id as string,
        },
      },
    });

    return res(ctx.status(204));
  }),
];
