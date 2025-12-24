-- 計画ステータス
CREATE TYPE 計画ステータス AS ENUM ('草案', '確定', '展開済', '取消');

-- オーダ種別
CREATE TYPE オーダ種別 AS ENUM ('購買', '製造');

-- 引当区分
CREATE TYPE 引当区分 AS ENUM ('在庫', '発注残', '製造残');

-- 基準生産計画
CREATE TABLE "基準生産計画" (
    "ID" SERIAL PRIMARY KEY,
    "MPS番号" VARCHAR(20) UNIQUE NOT NULL,
    "計画日" DATE NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "計画数量" DECIMAL(15, 2) NOT NULL,
    "納期" DATE NOT NULL,
    "ステータス" 計画ステータス DEFAULT '草案' NOT NULL,
    "場所コード" VARCHAR(20),
    "備考" TEXT,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50)
);

-- オーダ情報（購買・製造共通）
CREATE TABLE "オーダ情報" (
    "ID" SERIAL PRIMARY KEY,
    "オーダNO" VARCHAR(20) UNIQUE NOT NULL,
    "オーダ種別" オーダ種別 NOT NULL,
    "品目コード" VARCHAR(20) NOT NULL,
    "着手予定日" DATE NOT NULL,
    "納期" DATE NOT NULL,
    "有効期限" DATE,
    "計画数量" DECIMAL(15, 2) NOT NULL,
    "場所コード" VARCHAR(20) NOT NULL,
    "ステータス" 計画ステータス DEFAULT '草案' NOT NULL,
    "MPS_ID" INTEGER REFERENCES "基準生産計画"("ID"),
    "親オーダID" INTEGER REFERENCES "オーダ情報"("ID"),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "作成者" VARCHAR(50),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(50)
);

-- 所要情報
CREATE TABLE "所要情報" (
    "ID" SERIAL PRIMARY KEY,
    "所要NO" VARCHAR(20) UNIQUE NOT NULL,
    "オーダID" INTEGER NOT NULL REFERENCES "オーダ情報"("ID"),
    "品目コード" VARCHAR(20) NOT NULL,
    "納期" DATE NOT NULL,
    "必要数量" DECIMAL(15, 2) NOT NULL,
    "引当済数量" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "不足数量" DECIMAL(15, 2) DEFAULT 0 NOT NULL,
    "場所コード" VARCHAR(20) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 引当情報
CREATE TABLE "引当情報" (
    "ID" SERIAL PRIMARY KEY,
    "所要ID" INTEGER NOT NULL REFERENCES "所要情報"("ID"),
    "引当区分" 引当区分 NOT NULL,
    "オーダID" INTEGER REFERENCES "オーダ情報"("ID"),
    "引当日" DATE NOT NULL,
    "引当数量" DECIMAL(15, 2) NOT NULL,
    "場所コード" VARCHAR(20) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- インデックス
CREATE INDEX "idx_基準生産計画_品目コード" ON "基準生産計画"("品目コード");
CREATE INDEX "idx_基準生産計画_納期" ON "基準生産計画"("納期");
CREATE INDEX "idx_オーダ情報_品目コード" ON "オーダ情報"("品目コード");
CREATE INDEX "idx_オーダ情報_納期" ON "オーダ情報"("納期");
CREATE INDEX "idx_オーダ情報_MPS_ID" ON "オーダ情報"("MPS_ID");
CREATE INDEX "idx_所要情報_オーダID" ON "所要情報"("オーダID");
CREATE INDEX "idx_所要情報_品目コード" ON "所要情報"("品目コード");
CREATE INDEX "idx_引当情報_所要ID" ON "引当情報"("所要ID");
