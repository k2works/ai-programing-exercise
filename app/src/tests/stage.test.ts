import {describe, beforeEach, it, expect} from "vitest";
import { Config } from "../config";
import { Stage } from "../stage";
import { PuyoImage } from "../puyoimage";

describe("ステージ", () => {
    let stage: Stage;
    let puyoImage: PuyoImage;
    let config: Config;

    beforeEach(() => {
        document.body.innerHTML = `
      <body style="margin: 0;">
        <!-- <div id="stage" style="position:absolute; left: 0; top: 0; overflow: hidden;"></div> -->
        <div
          id="stage"
          style="
            position: relative;
            margin: 0 auto;
            overflow: hidden;
          "
        ></div>
        <!-- <div id="score" style="position:absolute; left: 0; top: 0; overflow: hidden; text-align: right;"></div> -->
        <div
          id="score"
          style="margin: 0 auto; overflow: hidden; text-align: right;"
        ></div>
        <div style="display: none;">
          <img src="/img/puyo_1.png" alt="緑ぷよ" id="puyo_1" />
          <img src="/img/puyo_2.png" alt="青ぷよ" id="puyo_2" />
          <img src="/img/puyo_3.png" alt="紫ぷよ" id="puyo_3" />
          <img src="/img/puyo_4.png" alt="赤ぷよ" id="puyo_4" />
          <img src="/img/puyo_5.png" alt="黄ぷよ" id="puyo_5" />
          <img src="/img/batankyu.png" id="batankyu" />
          <img src="/img/zenkeshi.png" id="zenkeshi" />
          <img src="/img/0.png" id="font0" />
          <img src="/img/1.png" id="font1" />
          <img src="/img/2.png" id="font2" />
          <img src="/img/3.png" id="font3" />
          <img src="/img/4.png" id="font4" />
          <img src="/img/5.png" id="font5" />
          <img src="/img/6.png" id="font6" />
          <img src="/img/7.png" id="font7" />
          <img src="/img/8.png" id="font8" />
          <img src="/img/9.png" id="font9" />
        </div>
      </body>
    `;

        config = new Config();
        puyoImage = new PuyoImage(config);
        stage = new Stage(config, puyoImage);
    });

    describe("ステージの準備をする", () => {
        it("HTMLからステージの元となる要素を取得し、大きさを設定する", () => {
            expect(stage.stageElement.id).toEqual("stage");
            expect(stage.stageElement.style.width).toEqual("240px");
            expect(stage.stageElement.style.height).toEqual("480px");
            expect(stage.stageElement.style.backgroundColor).toEqual(
                "rgb(255, 255, 255)"
            );
            expect(stage.stageElement.getElementsByTagName("img")[0].id).toEqual(
                "zenkeshi"
            );
            expect(stage.stageElement.getElementsByTagName("img")[0].width).toEqual(
                240
            );
            expect(
                stage.stageElement.getElementsByTagName("img")[0].style.position
            ).toEqual("absolute");
            expect(
                stage.stageElement.getElementsByTagName("img")[0].style.display
            ).toEqual("none");

            expect(stage.scoreElement.id).toEqual("score");
            expect(stage.scoreElement.style.backgroundColor).toEqual(
                "rgb(36, 192, 187)"
            );
            expect(stage.scoreElement.style.width).toEqual("240px");
            expect(stage.scoreElement.style.height).toEqual("33px");
        });

        it("メモリを準備する", () => {
            expect(stage.puyoCount).toEqual(0);
        });
    });

    describe("自由落下をチェックする", () => {
        describe("落ちるものがあったことを記録しておく", () => {
            it("一番下の行の左から1列目に緑ぷよ画像が存在する", () => {
                stage.setPuyo(0, 10, 1);
                const result = stage.checkFall();
                const fallingPuyo = stage.fallingPuyoList[0];

                expect(result).toBeTruthy();
                expect(fallingPuyo.element.src).toEqual(
                    "http://localhost:3000/img/puyo_1.png"
                );
                expect(fallingPuyo.element.alt).toEqual("緑ぷよ");
                expect(fallingPuyo.position).toEqual(400);
                expect(fallingPuyo.destination).toEqual(440);
                expect(fallingPuyo.falling).toBeTruthy();
            });

            it("一番下の行の左から6列目に黄ぷよ画像が存在する", () => {
                stage.setPuyo(5, 0, 5);
                const result = stage.checkFall();
                const fallingPuyo = stage.fallingPuyoList[0];

                expect(result).toBeTruthy();
                expect(fallingPuyo.element.src).toEqual(
                    "http://localhost:3000/img/puyo_5.png"
                );
                expect(fallingPuyo.element.alt).toEqual("黄ぷよ");
                expect(fallingPuyo.position).toEqual(0);
                expect(fallingPuyo.destination).toEqual(440);
                expect(fallingPuyo.falling).toBeTruthy();
            });
        });

        it("自由落下中", () => {
            stage.setPuyo(0, 10, 1);
            stage.checkFall();
            const result = stage.fall();

            expect(result).toBeTruthy();
        });

        it("自由落下終了", () => {
            stage.setPuyo(0, 11, 1);
            stage.checkFall();
            const result = stage.fall();

            expect(result).toBeFalsy();
        });
    });

    describe("消せるかどうか判定する", () => {
        it("ぷよがあるか確認する", () => {
            const result = stage.checkErase(0);

            expect(result).toBe(null);
        });

        

        describe("四方向周囲ぷよを確認する", () => {
            it("ステージの外にはみ出た", () => {
                stage.setPuyo(5, 11, 1);
                stage.checkErase(0);

                expect(stage.board[11][5]!.puyo).toEqual(1);
            });

            it("ぷよの色が違う", () => {
                stage.setPuyo(0, 0, 1);
                stage.setPuyo(1, 0, 2);
                stage.checkErase(0);

                expect(stage.board[0][0]!.puyo).toEqual(1);
                expect(stage.board[0][1]!.puyo).toEqual(2);
            });

            it("そのぷよのまわりのぷよも消せるか判定する", () => {
                stage.setPuyo(0, 0, 1);
                stage.setPuyo(1, 0, 1);
                stage.setPuyo(0, 1, 1);
                stage.checkErase(0);

                expect(stage.board[0][0]!.puyo).toEqual(1);
                expect(stage.board[0][1]!.puyo).toEqual(1);
                expect(stage.board[1][0]!.puyo).toEqual(1);
            });
        });

        describe("もし消せるならば、消えるぷよの個数と色の情報をまとめて返す", () => {
            it("緑ぷよが横に4個並ぶ", () => {
                stage.setPuyo(0, 0, 1);
                stage.setPuyo(1, 0, 1);
                stage.setPuyo(2, 0, 1);
                stage.setPuyo(3, 0, 1);
                const result = stage.checkErase(0);

                expect(result!.piece).toEqual(4);
                expect(result!.color).toEqual(1);
            });

            it("黄ぷよが縦に4個並ぶ", () => {
                stage.setPuyo(0, 0, 5);
                stage.setPuyo(0, 1, 5);
                stage.setPuyo(0, 2, 5);
                stage.setPuyo(0, 3, 5);
                const result = stage.checkErase(0);

                expect(result!.piece).toEqual(4);
                expect(result!.color).toEqual(5);
            });
        });
    });

    describe("消すアニメーションをする", () => {
        beforeEach(() => {
            stage.setPuyo(0, 0, 1);
            stage.setPuyo(1, 0, 1);
            stage.setPuyo(2, 0, 1);
            stage.setPuyo(3, 0, 1);
        });

        it("アニメーションを終了する", () => {
            stage.checkErase(0);

            expect(stage.erasing(31)).toBeFalsy();
        });

        it("レシオが1未満かつ0.75より多い場合", () => {
            stage.checkErase(0);

            expect(stage.erasing(23.0)).toBeTruthy();
            expect(stage.erasingPuyoInfoList[0].cell.element.style.display).toEqual(
                "block"
            );
            expect(stage.erasingPuyoInfoList[1].cell.element.style.display).toEqual(
                "block"
            );
            expect(stage.erasingPuyoInfoList[2].cell.element.style.display).toEqual(
                "block"
            );
            expect(stage.erasingPuyoInfoList[3].cell.element.style.display).toEqual(
                "block"
            );
        });

        it("レシオが0.75未満かつ0.5より多い場合", () => {
            stage.checkErase(0);

            expect(stage.erasing(16.0)).toBeTruthy();
            expect(stage.erasingPuyoInfoList[0].cell.element.style.display).toEqual(
                "none"
            );
            expect(stage.erasingPuyoInfoList[1].cell.element.style.display).toEqual(
                "none"
            );
            expect(stage.erasingPuyoInfoList[2].cell.element.style.display).toEqual(
                "none"
            );
            expect(stage.erasingPuyoInfoList[3].cell.element.style.display).toEqual(
                "none"
            );
        });

        it("レシオが0.5未満かつ0.25より多い場合", () => {
            stage.checkErase(0);

            expect(stage.erasing(7.6)).toBeTruthy();
            expect(stage.erasingPuyoInfoList[0].cell.element.style.display).toEqual(
                "block"
            );
            expect(stage.erasingPuyoInfoList[1].cell.element.style.display).toEqual(
                "block"
            );
            expect(stage.erasingPuyoInfoList[2].cell.element.style.display).toEqual(
                "block"
            );
            expect(stage.erasingPuyoInfoList[3].cell.element.style.display).toEqual(
                "block"
            );
        });

        it("レシオが0.25未満の場合", () => {
            stage.checkErase(0);

            expect(stage.erasing(7)).toBeTruthy();
            expect(stage.erasingPuyoInfoList[0].cell.element.style.display).toEqual(
                "none"
            );
            expect(stage.erasingPuyoInfoList[1].cell.element.style.display).toEqual(
                "none"
            );
            expect(stage.erasingPuyoInfoList[2].cell.element.style.display).toEqual(
                "none"
            );
            expect(stage.erasingPuyoInfoList[3].cell.element.style.display).toEqual(
                "none"
            );
        });
    });

    describe("全消し消去する", () => {
        describe("全消しを消去する", () => {
            it("全消しイメージのプロパティを更新する", () => {
                stage.hideZenkeshi();

                expect(stage.zenkeshiImage.style.display).toEqual("none");
            });
        });

        describe("全消しを表示する", () => {
            it("全消しイメージのプロパティを更新する", () => {
                stage.showZenkeshi();

                expect(stage.zenkeshiImage.style.display).toEqual("block");
                expect(stage.zenkeshiImage.style.opacity).toEqual("1");
                expect(stage.zenkeshiImage.style.top).toEqual("735px");
            });
        });
    })
});
