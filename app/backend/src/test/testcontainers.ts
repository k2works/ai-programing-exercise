import { GenericContainer, StartedTestContainer } from 'testcontainers';

let container: StartedTestContainer;

export async function startPostgresContainer() {
  container = await new GenericContainer('postgres:16-alpine')
    .withEnvironment({
      POSTGRES_USER: 'test',
      POSTGRES_PASSWORD: 'test',
      POSTGRES_DB: 'test_db',
    })
    .withExposedPorts(5432)
    .start();

  const host = container.getHost();
  const port = container.getMappedPort(5432);

  return {
    url: `postgresql://test:test@${host}:${port}/test_db`,
    container,
  };
}

export async function stopPostgresContainer() {
  if (container) {
    await container.stop();
  }
}
