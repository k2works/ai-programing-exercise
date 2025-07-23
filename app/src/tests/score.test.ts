import {describe, beforeEach, it, expect} from "vitest";
import { Config } from "../config";
import { Score } from "../score";
import { Stage } from "../stage";
import { PuyoImage } from "../puyoimage";

describe("スコア", () => {
    let score: Score;
    let puyoImage: PuyoImage;
    let config: Config;
    let stage: Stage;

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
        score = new Score(config, stage);
    });

    it("スコア表示の準備をする", () => {
        expect(score.score).toEqual(0);
        expect(score.fontLength).toEqual(9);
        expect(score.fontTemplateList[0].id).toEqual("font0");
        expect(score.fontTemplateList[8].id).toEqual("font8");
    });

    describe("得点の計算をする", () => {
        it("1連鎖4個緑ぷよの得点", () => {
            score.calculateScore(1, 4, 1);

            expect(score.score).toEqual(400);
        });

        it("1連鎖4個青ぷよの得点", () => {
            score.calculateScore(1, 4, 2);

            expect(score.score).toEqual(520);
        });

        it("1連鎖4個紫ぷよの得点", () => {
            score.calculateScore(1, 4, 3);

            expect(score.score).toEqual(640);
        });

        it("1連鎖4個赤ぷよの得点", () => {
            score.calculateScore(1, 4, 4);

            expect(score.score).toEqual(880);
        });

        it("1連鎖4個黄ぷよの得点", () => {
            score.calculateScore(1, 4, 5);

            expect(score.score).toEqual(1360);
        });

        it("23連鎖11個黄ぷよの得点", () => {
            score.calculateScore(23, 11, 5);

            expect(score.score).toEqual(77660);
        });
    });
});
