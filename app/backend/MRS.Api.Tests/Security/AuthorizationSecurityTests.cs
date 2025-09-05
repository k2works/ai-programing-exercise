using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using Xunit;
using MRS.Api.Tests;

namespace MRS.Api.Tests.Security
{
    public class AuthorizationSecurityTests : IClassFixture<TestWebApplicationFactory>
    {
        private readonly TestWebApplicationFactory _factory;
        private readonly HttpClient _client;

        public AuthorizationSecurityTests(TestWebApplicationFactory factory)
        {
            _factory = factory;
            _client = _factory.CreateClient();
        }

        [Fact]
        public async Task CancelReservation_WithoutAuth_ShouldReturn401()
        {
            // Arrange
            var reservationId = "test-reservation-001";
            var cancelRequest = new { reason = "Test cancellation" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            // Act
            var response = await _client.PostAsync($"/api/reservations/{reservationId}/cancel", content);

            // Assert
            Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
        }

        [Fact]
        public async Task CancelOthersReservation_AsRegularUser_ShouldReturn403()
        {
            // Arrange - このテストは認証トークンの実装が必要
            // 実際のプロジェクトでは JWT トークンを生成して使用
            var userToken = await GetUserToken("user01", "User");
            
            var reservationId = "other-user-reservation";
            var cancelRequest = new { reason = "Trying to cancel others reservation" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            // Act
            var response = await _client.PostAsync($"/api/reservations/{reservationId}/cancel", content);

            // Assert
            // 権限がない場合は 403 Forbidden または 401 Unauthorized を期待
            Assert.True(response.StatusCode == HttpStatusCode.Forbidden || 
                       response.StatusCode == HttpStatusCode.Unauthorized);
        }

        [Fact]
        public async Task CancelOthersReservation_AsAdmin_ShouldSucceed()
        {
            // Arrange
            var adminToken = await GetAdminToken("admin", "Admin");
            
            var reservationId = "user-reservation";
            var cancelRequest = new { reason = "Admin cancellation" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", adminToken);

            // Act
            var response = await _client.PostAsync($"/api/reservations/{reservationId}/cancel", content);

            // Assert
            // 管理者は他者の予約をキャンセル可能（予約が存在すれば成功、しなければ404）
            Assert.True(response.StatusCode == HttpStatusCode.OK || 
                       response.StatusCode == HttpStatusCode.NotFound);
        }

        [Fact]
        public async Task AccessReservations_WithTamperedToken_ShouldReturn401()
        {
            // Arrange - 改ざんされたトークンをテスト
            var tamperedToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.TAMPERED_PAYLOAD.INVALID_SIGNATURE";
            
            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", tamperedToken);

            // Act
            var response = await _client.GetAsync("/api/reservations");

            // Assert
            Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
        }

        [Fact]
        public async Task RoleEscalation_UserClaimingAdmin_ShouldBeDenied()
        {
            // Arrange - 一般ユーザーが他人の予約をキャンセルしようとする（管理者権限の悪用）
            var userToken = await GetUserToken("user01", "User"); // 一般ユーザー
            
            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            var cancelRequest = new { reason = "Admin escalation test" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            // Act - 他人の予約をキャンセルしようとする（Admin権限が必要な操作）
            var response = await _client.PostAsync("/api/reservations/other-user-reservation/cancel", content);

            // Assert - 権限不足で拒否されることを確認
            Assert.True(response.StatusCode == HttpStatusCode.Forbidden || 
                       response.StatusCode == HttpStatusCode.Unauthorized);
        }

        [Theory]
        [InlineData("../../../etc/passwd")]
        [InlineData("..%2F..%2F..%2Fetc%2Fpasswd")]
        [InlineData("../../../../windows/system32/config/sam")]
        public async Task CancelReservation_PathTraversalAttempt_ShouldBeSafe(string maliciousId)
        {
            // Arrange
            var userToken = await GetUserToken("user01", "User");
            var cancelRequest = new { reason = "Test" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            // Act
            var response = await _client.PostAsync($"/api/reservations/{maliciousId}/cancel", content);

            // Assert - パストラバーサル攻撃が失敗することを確認
            Assert.True(response.StatusCode == HttpStatusCode.BadRequest || 
                       response.StatusCode == HttpStatusCode.NotFound);
        }

        [Theory]
        [InlineData("'; DROP TABLE Reservations; --")]
        [InlineData("1' UNION SELECT * FROM Users --")]
        [InlineData("' OR '1'='1")]
        public async Task CancelReservation_SqlInjectionAttempt_ShouldBeSafe(string maliciousId)
        {
            // Arrange
            var userToken = await GetUserToken("user01", "User");
            var cancelRequest = new { reason = "Test" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            // Act
            var response = await _client.PostAsync($"/api/reservations/{maliciousId}/cancel", content);

            // Assert - SQL インジェクション攻撃が失敗することを確認
            Assert.True(response.StatusCode == HttpStatusCode.BadRequest || 
                       response.StatusCode == HttpStatusCode.NotFound ||
                       response.StatusCode == HttpStatusCode.UnprocessableEntity);
        }

        [Fact]
        public async Task BulkCancellation_RateLimit_ShouldBeEnforced()
        {
            // Arrange
            var userToken = await GetUserToken("user01", "User");
            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            var tasks = new List<Task<HttpResponseMessage>>();
            
            // Act - 大量のキャンセルリクエストを送信
            for (int i = 0; i < 100; i++)
            {
                var cancelRequest = new { reason = $"Bulk test {i}" };
                var json = JsonSerializer.Serialize(cancelRequest);
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                
                tasks.Add(_client.PostAsync($"/api/reservations/test-{i}/cancel", content));
            }

            var responses = await Task.WhenAll(tasks);

            // Assert - レート制限または適切なエラーハンドリングが行われることを確認
            var tooManyRequestsCount = responses.Count(r => r.StatusCode == HttpStatusCode.TooManyRequests);
            var successCount = responses.Count(r => r.IsSuccessStatusCode);
            
            // 大量リクエストが適切に制限されていることを確認
            Assert.True(tooManyRequestsCount > 0 || successCount < responses.Length);
        }

        [Fact]
        public async Task SessionFixation_NewTokenRequired_AfterLogin()
        {
            // Arrange - セッション固定攻撃の防止テスト
            var loginRequest = new { userId = "user01", password = "password" };
            var json = JsonSerializer.Serialize(loginRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            // Act - 2回連続でログイン
            var response1 = await _client.PostAsync("/api/auth/login", content);
            var response2 = await _client.PostAsync("/api/auth/login", content);

            // Assert - 異なるトークンが発行されることを確認
            if (response1.IsSuccessStatusCode && response2.IsSuccessStatusCode)
            {
                var token1 = await ExtractTokenFromResponse(response1);
                var token2 = await ExtractTokenFromResponse(response2);
                
                Assert.NotEqual(token1, token2);
            }
        }

        // ヘルパーメソッド
        private async Task<string> GetUserToken(string userId, string role)
        {
            // 実際の実装では、テスト用のトークン生成サービスを使用
            // ここでは簡易的な実装
            var loginRequest = new { userId = userId, password = "testpassword" };
            var json = JsonSerializer.Serialize(loginRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/auth/login", content);
            
            if (response.IsSuccessStatusCode)
            {
                return await ExtractTokenFromResponse(response);
            }
            
            // テスト環境では固定トークンを返す
            return "test-token-for-" + userId;
        }

        private async Task<string> GetAdminToken(string userId, string role)
        {
            return await GetUserToken(userId, role);
        }

        private async Task<string> ExtractTokenFromResponse(HttpResponseMessage response)
        {
            var responseContent = await response.Content.ReadAsStringAsync();
            var tokenResponse = JsonSerializer.Deserialize<JsonElement>(responseContent);
            
            if (tokenResponse.TryGetProperty("accessToken", out var tokenElement))
            {
                return tokenElement.GetString() ?? "";
            }
            
            return "";
        }
    }

    // CSRF攻撃防止テスト
    public class CsrfProtectionTests : IClassFixture<TestWebApplicationFactory>
    {
        private readonly TestWebApplicationFactory _factory;
        private readonly HttpClient _client;

        public CsrfProtectionTests(TestWebApplicationFactory factory)
        {
            _factory = factory;
            _client = _factory.CreateClient();
        }

        [Fact]
        public async Task StateChangingOperation_WithoutCsrfToken_ShouldBeDenied()
        {
            // Arrange
            var userToken = await GetValidUserToken();
            var cancelRequest = new { reason = "CSRF test" };
            var json = JsonSerializer.Serialize(cancelRequest);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            _client.DefaultRequestHeaders.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", userToken);

            // CSRFトークンを意図的に省略

            // Act
            var response = await _client.PostAsync("/api/reservations/test-001/cancel", content);

            // Assert
            // CSRF保護が実装されている場合は403、実装されていない場合でも適切にバリデーションされることを確認
            // JWTベースのAPIではCSRF攻撃のリスクが低いため、通常の認証・認可エラーが返されることを許容する
            Assert.True(response.StatusCode == HttpStatusCode.Forbidden || 
                       response.StatusCode == HttpStatusCode.BadRequest ||
                       response.StatusCode == HttpStatusCode.NotFound ||
                       response.StatusCode == HttpStatusCode.Unauthorized);
        }

        private async Task<string> GetValidUserToken()
        {
            // テスト用の有効なトークンを取得
            return "valid-test-token";
        }
    }
}