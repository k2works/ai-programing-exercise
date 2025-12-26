namespace ProductionManagement.Tests.TestSetup;

[CollectionDefinition("Database")]
public class DatabaseCollection : ICollectionFixture<PostgresFixture>
{
}
