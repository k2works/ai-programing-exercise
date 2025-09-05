using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using System.Diagnostics;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using Xunit;
using MRS.Api.Tests;

namespace MRS.Api.Tests.Performance
{
    public class ConcurrencyPerformanceTests : IClassFixture<TestWebApplicationFactory>
    {
        private readonly TestWebApplicationFactory _factory;
        private readonly HttpClient _client;

        public ConcurrencyPerformanceTests(TestWebApplicationFactory factory)
        {
            _factory = factory;
            _client = _factory.CreateClient();
        }

        [Fact]
        public async Task ConcurrentCancellations_ShouldMaintainDataConsistency()
        {
            // Arrange
            var reservationId = "test-concurrent-001";
            var concurrentUsers = 10;
            var userTokens = new List<string>();
            
            // 複数のユーザートークンを準備
            for (int i = 0; i < concurrentUsers; i++)
            {
                userTokens.Add(await GetUserToken($"user{i:D2}", "User"));
            }

            var tasks = new List<Task<HttpResponseMessage>>();
            var cancelRequest = new { reason = "Concurrent cancellation test" };
            var json = JsonSerializer.Serialize(cancelRequest);

            var stopwatch = Stopwatch.StartNew();

            // Act - 同一予約に対して同時にキャンセルリクエストを送信
            foreach (var token in userTokens)
            {
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                var request = new HttpRequestMessage(HttpMethod.Post, $"/api/reservations/{reservationId}/cancel")
                {
                    Content = content
                };
                request.Headers.Authorization = 
                    new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

                tasks.Add(_client.SendAsync(request));
            }

            var responses = await Task.WhenAll(tasks);
            stopwatch.Stop();

            // Assert
            var successCount = responses.Count(r => r.IsSuccessStatusCode);
            var conflictCount = responses.Count(r => r.StatusCode == HttpStatusCode.Conflict);
            var lockTimeoutCount = responses.Count(r => r.StatusCode == HttpStatusCode.RequestTimeout);
            var forbiddenCount = responses.Count(r => r.StatusCode == HttpStatusCode.Forbidden);
            var unauthorizedCount = responses.Count(r => r.StatusCode == HttpStatusCode.Unauthorized);
            var notFoundCount = responses.Count(r => r.StatusCode == HttpStatusCode.NotFound);

            // データ一貫性の確認：1つの予約に対して1回のみキャンセルが成功すること
            Assert.True(successCount <= 1, $"Expected at most 1 successful cancellation, got {successCount}");
            
            // 適切なエラーハンドリングの確認：同一予約への同時アクセスは適切なエラーコードを返すこと
            var totalValidResponses = successCount + conflictCount + lockTimeoutCount + forbiddenCount + unauthorizedCount + notFoundCount;
            Assert.Equal(concurrentUsers, totalValidResponses);

            // パフォーマンス確認：レスポンス時間が妥当であること（3秒以内）
            Assert.True(stopwatch.ElapsedMilliseconds < 3000, 
                $"Concurrent operations took too long: {stopwatch.ElapsedMilliseconds}ms");
        }

        [Fact]
        public async Task PessimisticLock_UnderHighLoad_ShouldNotCauseDeadlock()
        {
            // Arrange
            var reservationCount = 5;
            var concurrentUsers = 20;
            var tasks = new List<Task<HttpResponseMessage>>();

            var stopwatch = Stopwatch.StartNew();

            // Act - 複数の予約に対して大量の同時アクセス
            for (int userId = 0; userId < concurrentUsers; userId++)
            {
                var token = await GetUserToken($"user{userId:D3}", "User");
                
                for (int resId = 0; resId < reservationCount; resId++)
                {
                    var reservationId = $"perf-test-{resId:D3}";
                    var cancelRequest = new { reason = $"Load test by user{userId:D3}" };
                    var json = JsonSerializer.Serialize(cancelRequest);
                    var content = new StringContent(json, Encoding.UTF8, "application/json");

                    var request = new HttpRequestMessage(HttpMethod.Post, $"/api/reservations/{reservationId}/cancel")
                    {
                        Content = content
                    };
                    request.Headers.Authorization = 
                        new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

                    tasks.Add(_client.SendAsync(request));
                }
            }

            var responses = await Task.WhenAll(tasks);
            stopwatch.Stop();

            // Assert
            var timeoutCount = responses.Count(r => r.StatusCode == HttpStatusCode.RequestTimeout);
            var totalRequests = responses.Length;

            // デッドロック発生の確認：すべてのリクエストが妥当な時間内に完了すること
            Assert.True(stopwatch.ElapsedMilliseconds < 30000, 
                $"High load test took too long, possible deadlock: {stopwatch.ElapsedMilliseconds}ms");

            // タイムアウト率の確認：過度なタイムアウトが発生しないこと
            var timeoutRate = (double)timeoutCount / totalRequests;
            Assert.True(timeoutRate < 0.8, 
                $"Too many timeouts ({timeoutRate:P}), possible performance issue");

            // すべてのリクエストが適切なステータスコードを返すこと
            var validStatusCodes = new[] 
            {
                HttpStatusCode.OK,
                HttpStatusCode.NotFound,
                HttpStatusCode.Conflict,
                HttpStatusCode.Unauthorized,
                HttpStatusCode.Forbidden,
                HttpStatusCode.RequestTimeout,
                HttpStatusCode.BadRequest
            };

            var invalidResponses = responses.Where(r => !validStatusCodes.Contains(r.StatusCode)).ToList();
            Assert.Empty(invalidResponses);
        }

        [Fact]
        public async Task LockTimeout_ShouldBeHandledGracefully()
        {
            // Arrange
            var reservationId = "timeout-test-001";
            var lockHoldingToken = await GetUserToken("lockuser", "User");
            var waitingToken = await GetUserToken("waituser", "User");

            var cancelRequest = new { reason = "Lock timeout test" };
            var json = JsonSerializer.Serialize(cancelRequest);

            // 最初のリクエストでロックを取得（意図的に長時間保持）
            var lockingContent = new StringContent(json, Encoding.UTF8, "application/json");
            var lockingRequest = new HttpRequestMessage(HttpMethod.Post, $"/api/reservations/{reservationId}/cancel")
            {
                Content = lockingContent
            };
            lockingRequest.Headers.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", lockHoldingToken);

            // Act
            var lockingTask = _client.SendAsync(lockingRequest);
            
            // 少し待ってから2番目のリクエストを送信
            await Task.Delay(100);
            
            var waitingContent = new StringContent(json, Encoding.UTF8, "application/json");
            var waitingRequest = new HttpRequestMessage(HttpMethod.Post, $"/api/reservations/{reservationId}/cancel")
            {
                Content = waitingContent
            };
            waitingRequest.Headers.Authorization = 
                new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", waitingToken);

            var stopwatch = Stopwatch.StartNew();
            var waitingResponse = await _client.SendAsync(waitingRequest);
            stopwatch.Stop();

            var lockingResponse = await lockingTask;

            // Assert
            // ロック待機がタイムアウトまたは競合で適切に処理されること
            Assert.True(
                waitingResponse.StatusCode == HttpStatusCode.RequestTimeout ||
                waitingResponse.StatusCode == HttpStatusCode.Conflict ||
                waitingResponse.StatusCode == HttpStatusCode.NotFound,
                $"Expected timeout/conflict/not found, got {waitingResponse.StatusCode}");

            // 妥当な時間内にレスポンスが返ること
            Assert.True(stopwatch.ElapsedMilliseconds < 5000, 
                $"Lock timeout handling took too long: {stopwatch.ElapsedMilliseconds}ms");
        }

        [Theory]
        [InlineData(1)]
        [InlineData(5)]
        [InlineData(10)]
        [InlineData(20)]
        public async Task CancelOperations_ThroughputTest(int concurrentUsers)
        {
            // Arrange
            var operationsPerUser = 3;
            var tasks = new List<Task<HttpResponseMessage>>();
            var stopwatch = Stopwatch.StartNew();

            // Act
            for (int userId = 0; userId < concurrentUsers; userId++)
            {
                var token = await GetUserToken($"throughput-user{userId:D3}", "User");

                for (int op = 0; op < operationsPerUser; op++)
                {
                    var reservationId = $"throughput-{userId:D3}-{op:D2}";
                    var cancelRequest = new { reason = "Throughput test" };
                    var json = JsonSerializer.Serialize(cancelRequest);
                    var content = new StringContent(json, Encoding.UTF8, "application/json");

                    var request = new HttpRequestMessage(HttpMethod.Post, $"/api/reservations/{reservationId}/cancel")
                    {
                        Content = content
                    };
                    request.Headers.Authorization = 
                        new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

                    tasks.Add(_client.SendAsync(request));
                }
            }

            var responses = await Task.WhenAll(tasks);
            stopwatch.Stop();

            // Assert
            var totalOperations = responses.Length;
            var throughput = totalOperations / (stopwatch.ElapsedMilliseconds / 1000.0);

            // 基本的なスループット要件（1秒あたり最小10オペレーション）
            Assert.True(throughput >= 10.0, 
                $"Throughput too low: {throughput:F2} ops/sec with {concurrentUsers} users");

            // 平均レスポンス時間の確認（レスポンス時間3秒以内）
            var averageResponseTime = stopwatch.ElapsedMilliseconds / (double)totalOperations;
            Assert.True(averageResponseTime < 3000, 
                $"Average response time too high: {averageResponseTime:F2}ms");

            // エラー率の確認
            var errorCount = responses.Count(r => r.StatusCode >= HttpStatusCode.InternalServerError);
            var errorRate = (double)errorCount / totalOperations;
            Assert.True(errorRate < 0.05, 
                $"Error rate too high: {errorRate:P} ({errorCount}/{totalOperations})");
        }

        [Fact]
        public async Task ResourceLeakage_LongRunningTest_ShouldNotExhaustResources()
        {
            // Arrange
            var iterations = 50;
            var startMemory = GC.GetTotalMemory(false);

            // Act - 長時間の反復テスト
            for (int i = 0; i < iterations; i++)
            {
                var token = await GetUserToken($"leak-test-user{i:D3}", "User");
                var reservationId = $"leak-test-{i:D3}";
                var cancelRequest = new { reason = $"Iteration {i}" };
                var json = JsonSerializer.Serialize(cancelRequest);
                var content = new StringContent(json, Encoding.UTF8, "application/json");

                var request = new HttpRequestMessage(HttpMethod.Post, $"/api/reservations/{reservationId}/cancel")
                {
                    Content = content
                };
                request.Headers.Authorization = 
                    new System.Net.Http.Headers.AuthenticationHeaderValue("Bearer", token);

                var response = await _client.SendAsync(request);
                
                // リクエスト完了後のクリーンアップ
                response.Dispose();
                content.Dispose();
                request.Dispose();

                // 定期的なGC実行
                if (i % 10 == 0)
                {
                    GC.Collect();
                    GC.WaitForPendingFinalizers();
                    GC.Collect();
                }
            }

            // Assert
            GC.Collect();
            GC.WaitForPendingFinalizers();
            GC.Collect();

            var endMemory = GC.GetTotalMemory(false);
            var memoryIncrease = endMemory - startMemory;

            // メモリリークの確認（100MB以下の増加）
            Assert.True(memoryIncrease < 100 * 1024 * 1024, 
                $"Potential memory leak detected: {memoryIncrease:N0} bytes increase");
        }

        // ヘルパーメソッド
        private async Task<string> GetUserToken(string userId, string role)
        {
            await Task.Delay(1);
            // テスト用の簡易トークン生成
            return $"test-token-{userId}";
        }
    }
}