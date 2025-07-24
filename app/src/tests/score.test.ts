import {describe, beforeEach, it, expect} from "vitest";
import { Config } from "../config";
import { Score } from "../score";
import { Stage } from "../stage";
import { PuyoImage } from "../puyoimage";

describe("スコア", () => {
    let score: Score;
    let stage: Stage;

    beforeEach(() => {
        document.body.innerHTML = `
      <div id="stage"></div>
      <div id="score"></div>
    `;
        const config = new Config();
        const puyoImage = new PuyoImage(config);
        stage = new Stage(config, puyoImage);
        score = new Score(config, stage);
    });

    it("スコア表示の準備をする", () => {
        expect(score.score).toEqual(0);
        expect(stage.scoreElement.textContent).toEqual("000000000");
    });

    describe("得点の計算をする", () => {
        it("1連鎖4個緑ぷよの得点", () => {
            score.calculateScore(1, 4, 1);
            expect(score.score).toEqual(400);
            expect(stage.scoreElement.textContent).toEqual("000000400");
        });

        it("23連鎖11個黄ぷよの得点", () => {
            score.calculateScore(23, 11, 5);
            expect(score.score).toEqual(77660);
            expect(stage.scoreElement.textContent).toEqual("000077660");
        });
    });
});
