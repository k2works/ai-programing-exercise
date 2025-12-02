package com.example.accounting.eventsourcing

import com.example.accounting.domain.eventsourcing.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDate}

/**
 * JournalEntryAggregate のユニットテスト
 */
class JournalEntryAggregateSpec extends AnyFlatSpec with Matchers:

  val testDetails = List(
    JournalDetailData(1, "1000", "D", BigDecimal(10000), Some("借方明細")),
    JournalDetailData(2, "4000", "C", BigDecimal(10000), Some("貸方明細")),
  )

  behavior of "JournalEntryAggregate"

  it should "空の Aggregate を作成できる" in {
    val aggregate = JournalEntryAggregate.empty("test-id")

    aggregate.id shouldBe "test-id"
    aggregate.version shouldBe 0
    aggregate.status shouldBe JournalStatus.Draft
    aggregate.journalDate shouldBe None
    aggregate.description shouldBe None
    aggregate.details shouldBe empty
  }

  it should "JournalEntryCreatedEvent を適用できる" in {
    val aggregate = JournalEntryAggregate.empty("test-id")
    val now = Instant.now()

    val event = JournalEntryCreatedEvent(
      aggregateId = "test-id",
      version = 1,
      timestamp = now,
      journalDate = LocalDate.of(2024, 1, 15),
      description = "テスト仕訳",
      details = testDetails,
      createdBy = "user1",
    )

    val updated = aggregate.apply(event)

    updated.version shouldBe 1
    updated.journalDate shouldBe Some(LocalDate.of(2024, 1, 15))
    updated.description shouldBe Some("テスト仕訳")
    updated.details shouldBe testDetails
    updated.status shouldBe JournalStatus.Draft
    updated.createdBy shouldBe Some("user1")
    updated.createdAt shouldBe Some(now)
  }

  it should "JournalEntryApprovedEvent を適用できる" in {
    val now = Instant.now()
    val aggregate = createTestAggregate(now)

    val approvedAt = now.plusSeconds(3600)
    val event = JournalEntryApprovedEvent(
      aggregateId = "test-id",
      version = 2,
      timestamp = approvedAt,
      approvedBy = "approver1",
      approvedAt = approvedAt,
    )

    val updated = aggregate.apply(event)

    updated.version shouldBe 2
    updated.status shouldBe JournalStatus.Approved
    updated.approvedBy shouldBe Some("approver1")
    updated.approvedAt shouldBe Some(approvedAt)
  }

  it should "JournalEntryRejectedEvent を適用できる" in {
    val now = Instant.now()
    val aggregate = createTestAggregate(now)

    val event = JournalEntryRejectedEvent(
      aggregateId = "test-id",
      version = 2,
      timestamp = now.plusSeconds(3600),
      rejectedBy = "reviewer1",
      reason = "金額が不正",
    )

    val updated = aggregate.apply(event)

    updated.version shouldBe 2
    updated.status shouldBe JournalStatus.Rejected
  }

  it should "JournalEntryDeletedEvent を適用できる" in {
    val now = Instant.now()
    val aggregate = createTestAggregate(now)

    val event = JournalEntryDeletedEvent(
      aggregateId = "test-id",
      version = 2,
      timestamp = now.plusSeconds(3600),
      deletedBy = "admin1",
      reason = "重複仕訳",
    )

    val updated = aggregate.apply(event)

    updated.version shouldBe 2
    updated.status shouldBe JournalStatus.Deleted
  }

  behavior of "Aggregate 状態遷移"

  it should "Draft 状態から Approved に遷移できる" in {
    val aggregate = createTestAggregate(Instant.now())

    aggregate.status shouldBe JournalStatus.Draft
    aggregate.canApprove shouldBe true
    aggregate.canReject shouldBe true
    aggregate.canDelete shouldBe true
  }

  it should "Approved 状態からは承認・却下できない" in {
    val now = Instant.now()
    val aggregate = createApprovedAggregate(now)

    aggregate.status shouldBe JournalStatus.Approved
    aggregate.canApprove shouldBe false
    aggregate.canReject shouldBe false
    aggregate.canDelete shouldBe true
  }

  it should "Rejected 状態からは何もできない" in {
    val now = Instant.now()
    val aggregate = createRejectedAggregate(now)

    aggregate.status shouldBe JournalStatus.Rejected
    aggregate.canApprove shouldBe false
    aggregate.canReject shouldBe false
    aggregate.canDelete shouldBe false
  }

  behavior of "イベント再生"

  it should "イベントリストから状態を復元できる" in {
    val now = Instant.now()
    val events = List(
      JournalEntryCreatedEvent(
        "test-id", 1, now, LocalDate.of(2024, 1, 15), "テスト仕訳", testDetails, "user1"
      ),
      JournalEntryApprovedEvent(
        "test-id", 2, now.plusSeconds(3600), "approver1", now.plusSeconds(3600)
      ),
    )

    val result = JournalEntryAggregate.replay(events)

    result.isRight shouldBe true
    val aggregate = result.toOption.get
    aggregate.id shouldBe "test-id"
    aggregate.version shouldBe 2
    aggregate.status shouldBe JournalStatus.Approved
    aggregate.approvedBy shouldBe Some("approver1")
  }

  it should "空のイベントリストではエラーを返す" in {
    val result = JournalEntryAggregate.replay(List.empty)

    result.isLeft shouldBe true
    result.left.toOption.get shouldBe "イベントが空です"
  }

  it should "スナップショットから差分イベントを適用して復元できる" in {
    val now = Instant.now()
    val snapshot = createTestAggregate(now)

    val newEvents = List(
      JournalEntryApprovedEvent(
        "test-id", 2, now.plusSeconds(3600), "approver1", now.plusSeconds(3600)
      ),
    )

    val result = JournalEntryAggregate.replayFromSnapshot(snapshot, newEvents)

    result.version shouldBe 2
    result.status shouldBe JournalStatus.Approved
  }

  behavior of "貸借計算"

  it should "借方合計を正しく計算できる" in {
    val aggregate = createTestAggregate(Instant.now())

    aggregate.totalDebit shouldBe BigDecimal(10000)
  }

  it should "貸方合計を正しく計算できる" in {
    val aggregate = createTestAggregate(Instant.now())

    aggregate.totalCredit shouldBe BigDecimal(10000)
  }

  it should "貸借一致を判定できる" in {
    val aggregate = createTestAggregate(Instant.now())

    aggregate.isBalanced shouldBe true
  }

  it should "貸借不一致を判定できる" in {
    val unbalancedDetails = List(
      JournalDetailData(1, "1000", "D", BigDecimal(10000), Some("借方明細")),
      JournalDetailData(2, "4000", "C", BigDecimal(5000), Some("貸方明細")),
    )

    val now = Instant.now()
    val event = JournalEntryCreatedEvent(
      "test-id", 1, now, LocalDate.of(2024, 1, 15), "不均衡仕訳", unbalancedDetails, "user1"
    )
    val aggregate = JournalEntryAggregate.empty("test-id").apply(event)

    aggregate.isBalanced shouldBe false
    aggregate.totalDebit shouldBe BigDecimal(10000)
    aggregate.totalCredit shouldBe BigDecimal(5000)
  }

  private def createTestAggregate(now: Instant): JournalEntryAggregate =
    val event = JournalEntryCreatedEvent(
      "test-id", 1, now, LocalDate.of(2024, 1, 15), "テスト仕訳", testDetails, "user1"
    )
    JournalEntryAggregate.empty("test-id").apply(event)

  private def createApprovedAggregate(now: Instant): JournalEntryAggregate =
    val aggregate = createTestAggregate(now)
    val event = JournalEntryApprovedEvent(
      "test-id", 2, now.plusSeconds(3600), "approver1", now.plusSeconds(3600)
    )
    aggregate.apply(event)

  private def createRejectedAggregate(now: Instant): JournalEntryAggregate =
    val aggregate = createTestAggregate(now)
    val event = JournalEntryRejectedEvent(
      "test-id", 2, now.plusSeconds(3600), "reviewer1", "却下理由"
    )
    aggregate.apply(event)
