import { http, HttpResponse } from 'msw';
import { db } from '../seed-db';

export const jobsHandlers = [
  // GET /jobs - 求人一覧取得
  http.get('http://localhost:3001/api/jobs', ({ request }) => {
    const url = new URL(request.url);
    
    // エラーテスト用のクエリパラメータをチェック
    if (url.searchParams.get('error') === 'server') {
      return HttpResponse.json(
        { message: 'Internal Server Error' },
        { status: 500 }
      );
    }
    
    // 空のリストテスト用
    if (url.searchParams.get('empty') === 'true') {
      return HttpResponse.json([]);
    }
    
    const jobs = db.job.getAll();
    // MSW データからシンプルな JSON オブジェクトに変換
    const cleanJobs = jobs.map(job => ({
      id: job.id,
      position: job.position,
      department: job.department,
      location: job.location,
      info: job.info,
      organizationId: job.organizationId,
      status: job.status,
      createdAt: job.createdAt,
      updatedAt: job.updatedAt,
    }));
    
    return HttpResponse.json(cleanJobs);
  }),

  // POST /jobs - 求人作成
  http.post('http://localhost:3001/api/jobs', async ({ request }) => {
    const body = await request.json() as { position: string; department: string; location: string; info: string; organizationId?: string; status?: string; };
    
    // エラーテスト用のパラメータをチェック
    if (body.position === 'ERROR_TEST') {
      return HttpResponse.json(
        { message: 'Validation Error' },
        { status: 400 }
      );
    }
    
    if (body.position === 'SERVER_ERROR_TEST') {
      return HttpResponse.json(
        { message: 'Internal Server Error' },
        { status: 500 }
      );
    }
    
    const newJob = db.job.create({
      ...body,
      status: 'active',
    });
    
    // クリーンな JSON オブジェクトに変換
    const cleanJob = {
      id: newJob.id,
      position: newJob.position,
      department: newJob.department,
      location: newJob.location,
      info: newJob.info,
      organizationId: newJob.organizationId,
      status: newJob.status,
      createdAt: newJob.createdAt,
      updatedAt: newJob.updatedAt,
    };
    
    return HttpResponse.json(cleanJob, { status: 201 });
  }),

  // GET /jobs/:id - 求人詳細取得
  http.get('http://localhost:3001/api/jobs/:id', ({ params }) => {
    const { id } = params;
    const job = db.job.findFirst({
      where: { id: { equals: id as string } },
    });

    if (!job) {
      return HttpResponse.json(
        { message: 'Job not found' },
        { status: 404 }
      );
    }

    return HttpResponse.json(job);
  }),

  // PUT /jobs/:id - 求人更新
  http.put('http://localhost:3001/api/jobs/:id', async ({ params, request }) => {
    const { id } = params;
    const body = await request.json() as { position?: string; department?: string; location?: string; info?: string; status?: string };

    const existingJob = db.job.findFirst({
      where: { id: { equals: id as string } },
    });

    if (!existingJob) {
      return HttpResponse.json(
        { message: 'Job not found' },
        { status: 404 }
      );
    }

    const updatedJob = db.job.update({
      where: { id: { equals: id as string } },
      data: { ...body, updatedAt: new Date() },
    });

    return HttpResponse.json(updatedJob);
  }),

  // DELETE /jobs/:id - 求人削除
  http.delete('http://localhost:3001/api/jobs/:id', ({ params }) => {
    const { id } = params;

    const existingJob = db.job.findFirst({
      where: { id: { equals: id as string } },
    });

    if (!existingJob) {
      return HttpResponse.json(
        { message: 'Job not found' },
        { status: 404 }
      );
    }

    db.job.delete({
      where: { id: { equals: id as string } },
    });

    return HttpResponse.json(null, { status: 204 });
  }),
];