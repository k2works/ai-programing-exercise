package com.example.accounting.eventsourcing

import com.example.accounting.domain.eventsourcing.*
import com.example.accounting.infrastructure.out.persistence.eventsourcing.EventStoreRepositoryImpl
import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.{Instant, LocalDate}

/**
 * EventStoreRepository の統合テスト
 */
class EventStoreRepositorySpec extends DatabaseSpec with BeforeAndAfterEach:

  val repository = new EventStoreRepositoryImpl()

  val testDetails = List(
    JournalDetailData(1, "1000", "D", BigDecimal(10000), Some("借方明細")),
    JournalDetailData(2, "4000", "C", BigDecimal(10000), Some("貸方明細")),
  )

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "EventStoreRepository イベント操作"

  it should "イベントを追加できる" in withContainers { container =>
    setupWithMigrations(container)

    val aggregateId = java.util.UUID.randomUUID().toString
    val now = Instant.now()
    val event = JournalEntryCreatedEvent(
      aggregateId = aggregateId,
      version = 1,
      timestamp = now,
      journalDate = LocalDate.of(2024, 1, 15),
      description = "テスト仕訳",
      details = testDetails,
      createdBy = "user1",
    )

    DB.localTx { implicit session =>
      repository.append(aggregateId, event)

      val events = repository.getEvents(aggregateId)
      events should have size 1
      events.head shouldBe a[JournalEntryCreatedEvent]
    }
  }

  it should "複数のイベントを順序どおりに取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val aggregateId = java.util.UUID.randomUUID().toString
    val now = Instant.now()

    val createEvent = JournalEntryCreatedEvent(
      aggregateId, 1, now, LocalDate.of(2024, 1, 15), "テスト仕訳", testDetails, "user1"
    )
    val approveEvent = JournalEntryApprovedEvent(
      aggregateId, 2, now.plusSeconds(3600), "approver1", now.plusSeconds(3600)
    )

    DB.localTx { implicit session =>
      repository.append(aggregateId, createEvent)
      repository.append(aggregateId, approveEvent)

      val events = repository.getEvents(aggregateId)
      events should have size 2
      events(0) shouldBe a[JournalEntryCreatedEvent]
      events(1) shouldBe a[JournalEntryApprovedEvent]
    }
  }

  it should "指定日時までのイベントを取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val aggregateId = java.util.UUID.randomUUID().toString
    val now = Instant.now()

    val createEvent = JournalEntryCreatedEvent(
      aggregateId, 1, now, LocalDate.of(2024, 1, 15), "テスト仕訳", testDetails, "user1"
    )
    val approveEvent = JournalEntryApprovedEvent(
      aggregateId, 2, now.plusSeconds(3600), "approver1", now.plusSeconds(3600)
    )

    DB.localTx { implicit session =>
      repository.append(aggregateId, createEvent)
      repository.append(aggregateId, approveEvent)

      // 最初のイベントの直後までの時点
      val events = repository.getEventsUntil(aggregateId, now.plusSeconds(1))
      events should have size 1
      events.head shouldBe a[JournalEntryCreatedEvent]
    }
  }

  it should "指定バージョン以降のイベントを取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val aggregateId = java.util.UUID.randomUUID().toString
    val now = Instant.now()

    val createEvent = JournalEntryCreatedEvent(
      aggregateId, 1, now, LocalDate.of(2024, 1, 15), "テスト仕訳", testDetails, "user1"
    )
    val approveEvent = JournalEntryApprovedEvent(
      aggregateId, 2, now.plusSeconds(3600), "approver1", now.plusSeconds(3600)
    )

    DB.localTx { implicit session =>
      repository.append(aggregateId, createEvent)
      repository.append(aggregateId, approveEvent)

      val events = repository.getEventsAfterVersion(aggregateId, 1)
      events should have size 1
      events.head shouldBe a[JournalEntryApprovedEvent]
    }
  }

  behavior of "EventStoreRepository Aggregate 復元"

  it should "イベントから Aggregate を復元できる" in withContainers { container =>
    setupWithMigrations(container)

    val aggregateId = java.util.UUID.randomUUID().toString
    val now = Instant.now()

    val createEvent = JournalEntryCreatedEvent(
      aggregateId, 1, now, LocalDate.of(2024, 1, 15), "テスト仕訳", testDetails, "user1"
    )

    DB.localTx { implicit session =>
      repository.append(aggregateId, createEvent)

      val aggregate = repository.loadAggregate(aggregateId)
      aggregate should not be None
      aggregate.get.id shouldBe aggregateId
      aggregate.get.version shouldBe 1
      aggregate.get.status shouldBe JournalStatus.Draft
    }
  }

  it should "存在しない Aggregate は None を返す" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val aggregate = repository.loadAggregate("non-existent-id")
      aggregate shouldBe None
    }
  }

  behavior of "EventStoreRepository スナップショット"

  it should "スナップショットを保存できる" in withContainers { container =>
    setupWithMigrations(container)

    val aggregateId = java.util.UUID.randomUUID().toString
    val now = Instant.now()

    val aggregate = JournalEntryAggregate(
      id = aggregateId,
      version = 5,
      journalDate = Some(LocalDate.of(2024, 1, 15)),
      description = Some("テスト仕訳"),
      details = testDetails,
      status = JournalStatus.Approved,
      createdBy = Some("user1"),
      createdAt = Some(now),
      approvedBy = Some("approver1"),
      approvedAt = Some(now.plusSeconds(3600)),
    )

    DB.localTx { implicit session =>
      repository.saveSnapshot(aggregate)

      val loaded = repository.loadSnapshot(aggregateId)
      loaded should not be None
      loaded.get.id shouldBe aggregateId
      loaded.get.version shouldBe 5
      loaded.get.status shouldBe JournalStatus.Approved
    }
  }

  it should "スナップショットと差分イベントから Aggregate を復元できる" in withContainers { container =>
    setupWithMigrations(container)

    val aggregateId = java.util.UUID.randomUUID().toString
    val now = Instant.now()

    // まずイベントを追加
    val createEvent = JournalEntryCreatedEvent(
      aggregateId, 1, now, LocalDate.of(2024, 1, 15), "テスト仕訳", testDetails, "user1"
    )

    DB.localTx { implicit session =>
      repository.append(aggregateId, createEvent)

      // スナップショットを保存
      val snapshot = JournalEntryAggregate.empty(aggregateId).apply(createEvent)
      repository.saveSnapshot(snapshot)

      // 追加イベントを保存
      val approveEvent = JournalEntryApprovedEvent(
        aggregateId, 2, now.plusSeconds(3600), "approver1", now.plusSeconds(3600)
      )
      repository.append(aggregateId, approveEvent)

      // Aggregate を復元（スナップショット + 差分イベント）
      val aggregate = repository.loadAggregate(aggregateId)
      aggregate should not be None
      aggregate.get.version shouldBe 2
      aggregate.get.status shouldBe JournalStatus.Approved
    }
  }

  behavior of "EventStoreRepository 集約ID取得"

  it should "集約種別ごとの全集約IDを取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val aggregateId1 = java.util.UUID.randomUUID().toString
    val aggregateId2 = java.util.UUID.randomUUID().toString
    val now = Instant.now()

    DB.localTx { implicit session =>
      val event1 = JournalEntryCreatedEvent(
        aggregateId1, 1, now, LocalDate.of(2024, 1, 15), "仕訳1", testDetails, "user1"
      )
      val event2 = JournalEntryCreatedEvent(
        aggregateId2, 1, now, LocalDate.of(2024, 1, 16), "仕訳2", testDetails, "user1"
      )

      repository.append(aggregateId1, event1)
      repository.append(aggregateId2, event2)

      val ids = repository.getAllAggregateIds("JournalEntry")
      ids should contain allOf(aggregateId1, aggregateId2)
    }
  }
