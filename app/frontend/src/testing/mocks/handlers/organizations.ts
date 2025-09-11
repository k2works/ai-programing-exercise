import { http, HttpResponse } from 'msw';
import { API_URL } from '@/config/constants';
import { db } from '../seed-db';

export const organizationsHandlers = [
  // GET /organizations - 組織一覧取得
  http.get(`${API_URL}/organizations`, () => {
    const organizations = db.organization.getAll();
    return HttpResponse.json(organizations);
  }),

  // POST /organizations - 組織作成
  http.post(`${API_URL}/organizations`, async ({ request }) => {
    const body = await request.json() as { name: string; description?: string };
    const newOrganization = db.organization.create(body);
    return HttpResponse.json(newOrganization, { status: 201 });
  }),

  // GET /organizations/:id - 組織詳細取得
  http.get(`${API_URL}/organizations/:id`, ({ params }) => {
    const { id } = params;
    const organization = db.organization.findFirst({
      where: { id: { equals: id as string } },
    });

    if (!organization) {
      return HttpResponse.json(
        { message: 'Organization not found' },
        { status: 404 }
      );
    }

    return HttpResponse.json(organization);
  }),

  // PUT /organizations/:id - 組織更新
  http.put(`${API_URL}/organizations/:id`, async ({ params, request }) => {
    const { id } = params;
    const body = await request.json() as { name?: string; description?: string };

    const existingOrganization = db.organization.findFirst({
      where: { id: { equals: id as string } },
    });

    if (!existingOrganization) {
      return HttpResponse.json(
        { message: 'Organization not found' },
        { status: 404 }
      );
    }

    const updatedOrganization = db.organization.update({
      where: { id: { equals: id as string } },
      data: body,
    });

    return HttpResponse.json(updatedOrganization);
  }),

  // DELETE /organizations/:id - 組織削除
  http.delete(`${API_URL}/organizations/:id`, ({ params }) => {
    const { id } = params;

    const existingOrganization = db.organization.findFirst({
      where: { id: { equals: id as string } },
    });

    if (!existingOrganization) {
      return HttpResponse.json(
        { message: 'Organization not found' },
        { status: 404 }
      );
    }

    db.organization.delete({
      where: { id: { equals: id as string } },
    });

    return HttpResponse.json(null, { status: 204 });
  }),
];