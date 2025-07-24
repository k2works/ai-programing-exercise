import {describe, beforeEach, it, expect, vi} from "vitest";
import {Config} from "../config";
import {Stage} from "../stage";
import {PuyoImage} from "../puyoimage";

describe("ステージ", () => {
    vi.useFakeTimers();
    let stage: Stage;
    let config: Config;

    beforeEach(() => {
        document.body.innerHTML = `
      <div id="stage"></div>
      <div id="score"></div>
    `;
        config = new Config();
        const puyoImage = new PuyoImage(config);
        stage = new Stage(config, puyoImage);
    });

    describe("ステージの準備をする", () => {
        it("HTMLからステージの元となる要素を取得し、大きさを設定する", () => {
            expect(stage.stageElement.id).toEqual("stage");
            expect(stage.stageElement.style.width).toEqual("240px");
            expect(stage.stageElement.style.height).toEqual("480px");
            expect(stage.stageElement.style.backgroundColor).toEqual("rgb(255, 255, 255)");
            
            const zenkeshiElement = stage.stageElement.children[0] as HTMLElement;
            expect(zenkeshiElement.textContent).toEqual('全消し！');
            expect(zenkeshiElement.style.display).toEqual("none");

            expect(stage.scoreElement.id).toEqual("score");
            expect(stage.scoreElement.style.backgroundColor).toEqual("rgb(36, 192, 187)");
        });

        it("メモリを準備する", () => {
            expect(stage.puyoCount).toEqual(0);
        });
    });

    describe("自由落下をチェックする", () => {
        it("落ちるぷよをfallingPuyoListに追加する", () => {
            stage.setPuyo(0, 10, 1); // Green puyo
            const result = stage.checkFall();
            const fallingPuyo = stage.fallingPuyoList[0];

            expect(result).toBeTruthy();
            expect(fallingPuyo.element.style.backgroundColor).toEqual("green");
            expect(fallingPuyo.position).toEqual(400);
            expect(fallingPuyo.destination).toEqual(440);
            expect(fallingPuyo.falling).toBeTruthy();
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
        it("消せるぷよがない場合はnullを返す", () => {
            const result = stage.checkErase(0);
            expect(result).toBe(null);
        });

        it("ぷよの接続判定ができる", () => {
            stage.setPuyo(0, 0, 1);
            stage.setPuyo(0, 1, 1);
            stage.setPuyo(1, 0, 2);
            const connectedPuyos = stage.checkConnections(0, 0);
            expect(connectedPuyos.length).toBe(2);
            expect(connectedPuyos).toEqual(expect.arrayContaining([{ x: 0, y: 0 }, { x: 0, y: 1 }]));
        });

        it("4つ繋がったぷよを消去対象として検出する", () => {
            stage.setPuyo(0, 0, 1);
            stage.setPuyo(1, 0, 1);
            stage.setPuyo(2, 0, 1);
            stage.setPuyo(3, 0, 1);
            const result = stage.checkErase(0);
            expect(result!.piece).toEqual(4);
            expect(result!.color).toEqual(1);
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

        it("アニメーション中はtrueを返す", () => {
            stage.checkErase(0);
            expect(stage.erasing(15)).toBeTruthy();
        });
    });

    describe("全消し", () => {
        it("全消しを表示する", () => {
            stage.showZenkeshi();
            vi.runAllTimers();
            expect(stage.zenkeshiImage.style.display).toEqual("block");
            expect(stage.zenkeshiImage.style.opacity).toEqual("1");
        });

        it("全消しを非表示にする", () => {
            stage.hideZenkeshi();
            vi.runAllTimers();
            expect(stage.zenkeshiImage.style.opacity).toEqual("0");
        });
    });
});

