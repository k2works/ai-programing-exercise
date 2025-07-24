import { describe, beforeEach, it, expect, vi } from "vitest";
import { Config } from "../config";
import { PuyoImage } from "../puyoimage";
import { Stage } from "../stage";

describe("ぷよ画像", () => {
    let config: Config;
    let puyoImage: PuyoImage;
    let stage: Stage;

    beforeEach(() => {
        document.body.innerHTML = `
      <div id="stage"></div>
      <div id="score"></div>
    `;
        config = new Config();
        puyoImage = new PuyoImage(config);
        stage = new Stage(config, puyoImage);
    });

    it("ぷよのDOMを生成する", () => {
        const puyoElement = puyoImage.getPuyo(1);
        expect(puyoElement.tagName).toBe("DIV");
        expect(puyoElement.style.width).toBe(`${config.puyoImageWidth}px`);
        expect(puyoElement.style.height).toBe(`${config.puyoImageHeight}px`);
        expect(puyoElement.style.backgroundColor).toBe("green");
        expect(puyoElement.style.borderRadius).toBe("50%");
    });

    it("ばたんきゅーの準備をする", () => {
        puyoImage.prepareBatankyu(0, stage);
        expect(puyoImage.batankyuElement.style.display).toBe("block");
        expect(stage.stageElement.contains(puyoImage.batankyuElement)).toBeTruthy();
    });

    it("ばたんきゅーのアニメーションをする", () => {
        puyoImage.prepareBatankyu(0, stage);
        puyoImage.batankyu(1500);
        expect(puyoImage.batankyuElement.style.left).not.toBe("");
        expect(puyoImage.batankyuElement.style.top).not.toBe("");
    });
});

