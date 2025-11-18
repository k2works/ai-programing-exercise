-- CreateTable
CREATE TABLE "監査ログ" (
    "ID" SERIAL NOT NULL,
    "エンティティタイプ" VARCHAR(50) NOT NULL,
    "エンティティID" VARCHAR(50) NOT NULL,
    "アクション" VARCHAR(20) NOT NULL,
    "ユーザーID" VARCHAR(50) NOT NULL,
    "ユーザー名" VARCHAR(100) NOT NULL,
    "タイムスタンプ" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "変更前の値" JSONB,
    "変更後の値" JSONB,
    "変更内容" JSONB,
    "理由" VARCHAR(500),
    "IPアドレス" VARCHAR(45),
    "ユーザーエージェント" VARCHAR(500),

    CONSTRAINT "監査ログ_pkey" PRIMARY KEY ("ID")
);

-- CreateIndex
CREATE INDEX "idx_audit_entity" ON "監査ログ"("エンティティタイプ", "エンティティID");

-- CreateIndex
CREATE INDEX "idx_audit_user" ON "監査ログ"("ユーザーID");

-- CreateIndex
CREATE INDEX "idx_audit_timestamp" ON "監査ログ"("タイムスタンプ");

-- CreateIndex
CREATE INDEX "idx_audit_action" ON "監査ログ"("アクション");
