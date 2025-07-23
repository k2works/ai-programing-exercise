import {describe, beforeEach, it, expect} from "vitest";
import { Config } from "../config";
import { PuyoImage } from "../puyoimage";
import {Stage} from "../stage";

describe("画像", () => {
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
        <img src="/img/puyo_1.png" id="puyo_1" />
        <img src="/img/puyo_2.png" id="puyo_2" />
        <img src="/img/puyo_3.png" id="puyo_3" />
        <img src="/img/puyo_4.png" id="puyo_4" />
        <img src="/img/puyo_5.png" id="puyo_5" />
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

    it("ぷよぷよの画像を準備する", () => {
        expect(puyoImage.puyoImages.length).toEqual(5);
        expect(puyoImage.puyoImages[0].width).toEqual(40);
        expect(puyoImage.puyoImages[0].height).toEqual(40);
        expect(puyoImage.puyoImages[0].style.position).toEqual("absolute");
    });

    it("ばたんきゅーの画像を準備する", () => {
        expect(puyoImage.batankyuImage.id).toEqual("batankyu");
        expect(puyoImage.batankyuImage.width).toEqual(240);
        expect(puyoImage.batankyuImage.style.position).toEqual("absolute");
    });

    it("ばたんきゅーの準備をする", () => {
        puyoImage.prepareBatankyu(1, stage);

        expect(puyoImage.batankyuImage.style.top).toEqual("0px");
    });

    it.skip("ばたんきゅー", () => {
        puyoImage.batankyu(1);

        expect(parseFloat(puyoImage.batankyuImage.style.left)).toBeCloseTo(0);
        expect(parseFloat(puyoImage.batankyuImage.style.top)).toBeCloseTo(183.75);
    });
});
