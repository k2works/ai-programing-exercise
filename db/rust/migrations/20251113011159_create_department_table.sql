-- 部門マスタテーブルを作成
CREATE TABLE "部門マスタ" (
    "部門コード" VARCHAR(10) NOT NULL,
    "開始日" TIMESTAMP NOT NULL,
    "終了日" TIMESTAMP,
    "部門名" VARCHAR(100),
    "組織階層" INTEGER NOT NULL,
    "部門パス" VARCHAR(500) NOT NULL,
    "最下層区分" INTEGER NOT NULL,
    "伝票入力可否" INTEGER NOT NULL,
    "作成日時" TIMESTAMP NOT NULL,
    "作成者名" VARCHAR(50),
    "更新日時" TIMESTAMP NOT NULL,
    "更新者名" VARCHAR(50),
    PRIMARY KEY ("部門コード", "開始日")
);
