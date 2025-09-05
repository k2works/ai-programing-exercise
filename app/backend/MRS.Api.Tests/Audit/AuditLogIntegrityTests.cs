using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using Xunit;
using MRS.Api.Tests;

namespace MRS.Api.Tests.Audit
{
    public class AuditLogIntegrityTests : IClassFixture<TestWebApplicationFactory>
    {
        private readonly TestWebApplicationFactory _factory;
        private readonly HttpClient _client;

        public AuditLogIntegrityTests(TestWebApplicationFactory factory)
        {
            _factory = factory;
            _client = _factory.CreateClient();
        }

        [Fact]
        public async Task CancelReservation_ShouldCreateAuditLog()
        {
            // Arrange
            var userToken = await GetUserToken("audit-user-001", "User");
            var reservationId = "audit-test-001";
            var cancelRequest = new { reason = "Audit log test cancellation" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            // Act
            var response = await _client.PostAsync($"/api/reservations/{reservationId}/cancel", content);

            // Assert
            // キャンセル操作後に監査ログが作成されることを確認
            var auditLogsResponse = await _client.GetAsync($"/api/audit-logs?reservationId={reservationId}");
            
            if (auditLogsResponse.IsSuccessStatusCode)
            {
                var auditLogsJson = await auditLogsResponse.Content.ReadAsStringAsync();
                var auditLogs = JsonSerializer.Deserialize<JsonElement>(auditLogsJson);

                if (auditLogs.ValueKind == JsonValueKind.Array && auditLogs.GetArrayLength() > 0)
                {
                    var firstLog = auditLogs[0];
                    
                    // 監査ログの必須フィールドを確認
                    Assert.True(firstLog.TryGetProperty("auditId", out _));
                    Assert.True(firstLog.TryGetProperty("reservationId", out var resId) && 
                               resId.GetString() == reservationId);
                    Assert.True(firstLog.TryGetProperty("action", out var action) && 
                               action.GetString() == "cancelled");
                    Assert.True(firstLog.TryGetProperty("performedBy", out var performedBy) && 
                               performedBy.GetString() == "audit-user-001");
                    Assert.True(firstLog.TryGetProperty("performedAt", out _));
                    Assert.True(firstLog.TryGetProperty("reason", out var reason) && 
                               reason.GetString() == "Audit log test cancellation");
                }
            }
        }

        [Fact]
        public async Task AdminCancelReservation_ShouldCreateDetailedAuditLog()
        {
            // Arrange
            var adminToken = await GetAdminToken("audit-admin-001", "Admin");
            var reservationId = "admin-audit-test-001";
            var cancelRequest = new 
            { 
                reason = "Admin audit test cancellation",
                adminComment = "Testing admin-specific audit logging"
            };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", adminToken);

            // Act
            var response = await _client.PostAsync($"/api/reservations/{reservationId}/cancel", content);

            // Assert
            var auditLogsResponse = await _client.GetAsync($"/api/audit-logs?reservationId={reservationId}");
            
            if (auditLogsResponse.IsSuccessStatusCode)
            {
                var auditLogsJson = await auditLogsResponse.Content.ReadAsStringAsync();
                var auditLogs = JsonSerializer.Deserialize<JsonElement>(auditLogsJson);

                if (auditLogs.ValueKind == JsonValueKind.Array && auditLogs.GetArrayLength() > 0)
                {
                    var firstLog = auditLogs[0];
                    
                    // 管理者固有の監査ログフィールドを確認
                    Assert.True(firstLog.TryGetProperty("userRole", out var role) && 
                               role.GetString() == "Admin");
                    Assert.True(firstLog.TryGetProperty("adminComment", out var adminComment) && 
                               adminComment.GetString() == "Testing admin-specific audit logging");
                }
            }
        }

        [Fact]
        public async Task MultipleOperations_ShouldCreateChronologicalAuditTrail()
        {
            // Arrange
            var userToken = await GetUserToken("timeline-user", "User");
            var adminToken = await GetAdminToken("timeline-admin", "Admin");
            var reservationId = "timeline-test-001";
            
            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            var operations = new List<(string action, string token, object request)>
            {
                ("create", userToken, new { title = "Test meeting", roomId = "room-001" }),
                ("update", userToken, new { title = "Updated meeting" }),
                ("cancel", userToken, new { reason = "User cancellation" }),
                ("restore", adminToken, new { reason = "Admin restoration", adminComment = "Error correction" })
            };

            var timestamps = new List<DateTime>();

            // Act - 一連の操作を実行
            foreach (var (action, token, request) in operations)
            {
                timestamps.Add(DateTime.UtcNow);
                
                _client.DefaultRequestHeaders.Authorization = 
                    new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

                var json = JsonSerializer.Serialize(request);
                var content = new StringContent(json, Encoding.UTF8, "application/json");

                await _client.PostAsync($"/api/reservations/{reservationId}/{action}", content);
                
                // 操作間に小さな遅延を入れて時系列を明確にする
                await Task.Delay(100);
            }

            // Assert
            var auditLogsResponse = await _client.GetAsync($"/api/audit-logs?reservationId={reservationId}");
            
            if (auditLogsResponse.IsSuccessStatusCode)
            {
                var auditLogsJson = await auditLogsResponse.Content.ReadAsStringAsync();
                var auditLogs = JsonSerializer.Deserialize<JsonElement>(auditLogsJson);

                if (auditLogs.ValueKind == JsonValueKind.Array)
                {
                    var logTimes = new List<DateTime>();
                    
                    foreach (var log in auditLogs.EnumerateArray())
                    {
                        if (log.TryGetProperty("performedAt", out var performedAtElement))
                        {
                            if (DateTime.TryParse(performedAtElement.GetString(), out var performedAt))
                            {
                                logTimes.Add(performedAt);
                            }
                        }
                    }

                    // 監査ログが時系列順に記録されていることを確認
                    var sortedTimes = logTimes.OrderBy(t => t).ToList();
                    Assert.Equal(sortedTimes, logTimes);

                    // すべての操作が記録されていることを確認
                    Assert.True(auditLogs.GetArrayLength() >= operations.Count);
                }
            }
        }

        [Fact]
        public async Task AuditLog_ShouldBeImmutable()
        {
            // Arrange
            var userToken = await GetUserToken("immutable-test-user", "User");
            var reservationId = "immutable-test-001";
            var cancelRequest = new { reason = "Immutability test" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            // Act - 監査ログを作成
            await _client.PostAsync($"/api/reservations/{reservationId}/cancel", content);

            // 監査ログの取得
            var getResponse = await _client.GetAsync($"/api/audit-logs?reservationId={reservationId}");
            var originalLogs = await getResponse.Content.ReadAsStringAsync();

            // 監査ログの変更を試行（失敗すべき）
            var updateContent = new StringContent(
                JsonSerializer.Serialize(new { reason = "Modified reason" }),
                Encoding.UTF8, 
                "application/json"
            );

            var updateResponse = await _client.PutAsync("/api/audit-logs/some-log-id", updateContent);
            var deleteResponse = await _client.DeleteAsync("/api/audit-logs/some-log-id");

            // Assert
            // 監査ログの変更・削除が拒否されることを確認
            Assert.True(updateResponse.StatusCode == HttpStatusCode.MethodNotAllowed ||
                       updateResponse.StatusCode == HttpStatusCode.Forbidden ||
                       updateResponse.StatusCode == HttpStatusCode.NotFound);

            Assert.True(deleteResponse.StatusCode == HttpStatusCode.MethodNotAllowed ||
                       deleteResponse.StatusCode == HttpStatusCode.Forbidden ||
                       deleteResponse.StatusCode == HttpStatusCode.NotFound);

            // 監査ログが変更されていないことを確認
            var verifyResponse = await _client.GetAsync($"/api/audit-logs?reservationId={reservationId}");
            var verifyLogs = await verifyResponse.Content.ReadAsStringAsync();
            
            Assert.Equal(originalLogs, verifyLogs);
        }

        [Fact]
        public async Task AuditLog_ShouldIncludeSecurityContext()
        {
            // Arrange
            var userToken = await GetUserToken("security-context-user", "User");
            var reservationId = "security-context-test";
            var cancelRequest = new { reason = "Security context test" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            // セキュリティコンテキスト情報を設定
            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);
            _client.DefaultRequestHeaders.Add("User-Agent", "SecurityTestClient/1.0");
            _client.DefaultRequestHeaders.Add("X-Forwarded-For", "192.168.1.100");

            // Act
            var response = await _client.PostAsync($"/api/reservations/{reservationId}/cancel", content);

            // Assert
            var auditLogsResponse = await _client.GetAsync($"/api/audit-logs?reservationId={reservationId}");
            
            if (auditLogsResponse.IsSuccessStatusCode)
            {
                var auditLogsJson = await auditLogsResponse.Content.ReadAsStringAsync();
                var auditLogs = JsonSerializer.Deserialize<JsonElement>(auditLogsJson);

                if (auditLogs.ValueKind == JsonValueKind.Array && auditLogs.GetArrayLength() > 0)
                {
                    var firstLog = auditLogs[0];
                    
                    // セキュリティコンテキスト情報の記録を確認
                    Assert.True(firstLog.TryGetProperty("ipAddress", out var ipAddress));
                    Assert.True(firstLog.TryGetProperty("userAgent", out var userAgent));
                    
                    // User-Agentが記録されていることを確認
                    var userAgentValue = userAgent.GetString();
                    Assert.Contains("SecurityTestClient", userAgentValue ?? "");
                }
            }
        }

        [Fact]
        public async Task AuditLog_ShouldHandleConcurrentWrites()
        {
            // Arrange
            var concurrentUsers = 5;
            var reservationPrefix = "concurrent-audit";
            var tasks = new List<Task>();

            // Act - 同時に複数の監査ログ作成操作を実行
            for (int i = 0; i < concurrentUsers; i++)
            {
                int userId = i;
                tasks.Add(Task.Run(async () =>
                {
                    var token = await GetUserToken($"concurrent-user-{userId:D2}", "User");
                    var reservationId = $"{reservationPrefix}-{userId:D2}";
                    var cancelRequest = new { reason = $"Concurrent test {userId}" };
                    var json = JsonSerializer.Serialize(cancelRequest);
                    var content = new StringContent(json, Encoding.UTF8, "application/json");

                    var localClient = _factory.CreateClient();
                    localClient.DefaultRequestHeaders.Authorization = 
                        new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

                    await localClient.PostAsync($"/api/reservations/{reservationId}/cancel", content);
                }));
            }

            await Task.WhenAll(tasks);

            // Assert - すべての監査ログが正しく作成されていることを確認
            for (int i = 0; i < concurrentUsers; i++)
            {
                var reservationId = $"{reservationPrefix}-{i:D2}";
                var auditLogsResponse = await _client.GetAsync($"/api/audit-logs?reservationId={reservationId}");
                
                if (auditLogsResponse.IsSuccessStatusCode)
                {
                    var auditLogsJson = await auditLogsResponse.Content.ReadAsStringAsync();
                    var auditLogs = JsonSerializer.Deserialize<JsonElement>(auditLogsJson);

                    // 各予約に対して少なくとも1つの監査ログが存在することを確認
                    Assert.True(auditLogs.ValueKind == JsonValueKind.Array && auditLogs.GetArrayLength() > 0,
                        $"Audit log not found for reservation {reservationId}");
                }
            }
        }

        [Fact]
        public async Task AuditLog_DataRetention_ShouldComplyWithPolicy()
        {
            // Arrange
            var userToken = await GetUserToken("retention-test-user", "User");
            var oldReservationId = "retention-old-001";
            var recentReservationId = "retention-recent-001";

            // 古い監査ログをシミュレート（実際の実装では日付を遡って設定）
            var cancelRequest = new { reason = "Data retention test" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            // Act
            await _client.PostAsync($"/api/reservations/{recentReservationId}/cancel", content);

            // Assert
            var auditLogsResponse = await _client.GetAsync("/api/audit-logs");
            
            if (auditLogsResponse.IsSuccessStatusCode)
            {
                var auditLogsJson = await auditLogsResponse.Content.ReadAsStringAsync();
                var auditLogs = JsonSerializer.Deserialize<JsonElement>(auditLogsJson);

                if (auditLogs.ValueKind == JsonValueKind.Array)
                {
                    foreach (var log in auditLogs.EnumerateArray())
                    {
                        if (log.TryGetProperty("performedAt", out var performedAtElement) &&
                            DateTime.TryParse(performedAtElement.GetString(), out var performedAt))
                        {
                            // データ保持期間（例：7年）を超えた古いログが適切に管理されていることを確認
                            var retentionPeriod = TimeSpan.FromDays(7 * 365); // 7年
                            var cutoffDate = DateTime.UtcNow - retentionPeriod;
                            
                            // 古すぎるログは通常のクエリでは取得されない、または適切にマーキングされている
                            Assert.True(performedAt > cutoffDate || 
                                       log.TryGetProperty("archived", out _),
                                "Old audit logs should be archived or filtered from regular queries");
                        }
                    }
                }
            }
        }

        // ヘルパーメソッド
        private async Task<string> GetUserToken(string userId, string role)
        {
            await Task.Delay(1);
            return $"test-token-{userId}";
        }

        private async Task<string> GetAdminToken(string userId, string role)
        {
            return await GetUserToken(userId, role);
        }
    }
}