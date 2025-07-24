import {describe, beforeEach, it, expect} from "vitest";
import {Config} from "../config";
import {Player} from "../player";
import {Stage} from "../stage";
import {PuyoImage} from "../puyoimage";
import {Score} from "../score";

describe("プレイヤー", () => {
    let player: Player;
    let config: Config;
    let stage: Stage;
    let puyoImage: PuyoImage;
    let score: Score;

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
        player = new Player(config);
    });

    it("プレイヤーのインスタンスを作成する", () => {
        expect(player).toBeDefined();
    });

    describe("ブラウザのキーボードの入力を取得するイベントリスナを登録する", () => {
        it("キーボード入力を確認する", () => {
            expect(player.keyStatus.right).toBeFalsy();
            expect(player.keyStatus.left).toBeFalsy();
            expect(player.keyStatus.up).toBeFalsy();
            expect(player.keyStatus.down).toBeFalsy();
        });

        describe("キーボードが押された場合", () => {
            it("左向きのキーを押す", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 37 }));

                expect(player.keyStatus.right).toBeFalsy();
                expect(player.keyStatus.left).toBeTruthy();
                expect(player.keyStatus.up).toBeFalsy();
                expect(player.keyStatus.down).toBeFalsy();
            });

            it("上向きのキーを押す", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 }));

                expect(player.keyStatus.right).toBeFalsy();
                expect(player.keyStatus.left).toBeFalsy();
                expect(player.keyStatus.up).toBeTruthy();
                expect(player.keyStatus.down).toBeFalsy();
            });

            it("右向きのキーを押す", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 39 }));

                expect(player.keyStatus.right).toBeTruthy();
                expect(player.keyStatus.left).toBeFalsy();
                expect(player.keyStatus.up).toBeFalsy();
                expect(player.keyStatus.down).toBeFalsy();
            });

            it("下向きのキーを押す", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 40 }));

                expect(player.keyStatus.right).toBeFalsy();
                expect(player.keyStatus.left).toBeFalsy();
                expect(player.keyStatus.up).toBeFalsy();
                expect(player.keyStatus.down).toBeTruthy();
            });
        });

        describe("キーボードが離された場合", () => {
            it("左向きのキーを押して離す", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 37 }));
                document.dispatchEvent(new KeyboardEvent("keyup", { keyCode: 37 }));

                expect(player.keyStatus.left).toBeFalsy();
            });

            it("上向きのキーを離す", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 }));
                document.dispatchEvent(new KeyboardEvent("keyup", { keyCode: 38 }));

                expect(player.keyStatus.up).toBeFalsy();
            });

            it("右向きのキーを離す", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 39 }));
                document.dispatchEvent(new KeyboardEvent("keyup", { keyCode: 39 }));

                expect(player.keyStatus.right).toBeFalsy();
            });

            it("下向きのキーを離す", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 40 }));
                document.dispatchEvent(new KeyboardEvent("keyup", { keyCode: 40 }));

                expect(player.keyStatus.down).toBeFalsy();
            });
        });

        describe("タッチ操作追加", () => {
            it("タッチイベントが発火する", () => {
                const touchStartEvent = new TouchEvent("touchstart", {
                    touches: [{ clientX: 0, clientY: 0 } as Touch]
                });
                document.dispatchEvent(touchStartEvent);

                expect(player.touchPoint.xs).toEqual(0);
                expect(player.touchPoint.ys).toEqual(0);
            });

            it("指が少し動いた時は無視", () => {
                const touchMoveEvent = new TouchEvent("touchmove", {
                    touches: [{ clientX: 0, clientY: 0 } as Touch]
                });
                document.dispatchEvent(touchMoveEvent);

                expect(player.keyStatus.right).toBeFalsy();
                expect(player.keyStatus.left).toBeFalsy();
                expect(player.keyStatus.up).toBeFalsy();
                expect(player.keyStatus.down).toBeFalsy();
            });

            describe("指の動きからジェスチャーによるkeyStatusプロパティを更新", () => {
                it("縦方向 up", () => {
                    const touchMoveEvent = new TouchEvent("touchmove", {
                        touches: [{ clientX: -21, clientY: -22 } as Touch]
                    });
                    document.dispatchEvent(touchMoveEvent);

                    expect(player.keyStatus.right).toBeFalsy();
                    expect(player.keyStatus.left).toBeFalsy();
                    expect(player.keyStatus.up).toBeTruthy();
                    expect(player.keyStatus.down).toBeFalsy();
                });

                it("縦方向 down", () => {
                    const touchMoveEvent = new TouchEvent("touchmove", {
                        touches: [{ clientX: 21, clientY: 22 } as Touch]
                    });
                    document.dispatchEvent(touchMoveEvent);

                    expect(player.keyStatus.right).toBeFalsy();
                    expect(player.keyStatus.left).toBeFalsy();
                    expect(player.keyStatus.up).toBeFalsy();
                    expect(player.keyStatus.down).toBeTruthy();
                });

                it("横方向 left", () => {
                    const touchMoveEvent = new TouchEvent("touchmove", {
                        touches: [{ clientX: -22, clientY: -21 } as Touch]
                    });
                    document.dispatchEvent(touchMoveEvent);

                    expect(player.keyStatus.right).toBeFalsy();
                    expect(player.keyStatus.left).toBeTruthy();
                    expect(player.keyStatus.up).toBeFalsy();
                    expect(player.keyStatus.down).toBeFalsy();
                });

                it("横方向 right", () => {
                    const touchMoveEvent = new TouchEvent("touchmove", {
                        touches: [{ clientX: 22, clientY: 21 } as Touch]
                    });
                    document.dispatchEvent(touchMoveEvent);

                    expect(player.keyStatus.right).toBeTruthy();
                    expect(player.keyStatus.left).toBeFalsy();
                    expect(player.keyStatus.up).toBeFalsy();
                    expect(player.keyStatus.down).toBeFalsy();
                });
            });

            it("タッチイベントが終了する", () => {
                const touchMoveEvent = new TouchEvent("touchmove", {
                    touches: [{ clientX: 21, clientY: 21 } as Touch]
                });
                document.dispatchEvent(touchMoveEvent);
                document.dispatchEvent(new TouchEvent("touchend"));

                expect(player.keyStatus.right).toBeFalsy();
                expect(player.keyStatus.left).toBeFalsy();
                expect(player.keyStatus.up).toBeFalsy();
                expect(player.keyStatus.down).toBeFalsy();
            });
        });
    });

    describe("プレイヤーが操作する", () => {
        beforeEach(() => {
            player.createNewPuyo(stage, puyoImage, score);
        });

        describe("まず自由落下を確認する", () => {
            describe("現状の場所の下にブロックがあるかどうか確認する", () => {
                describe("ブロックがある", () => {
                    it("ぷよを固定する", () => {
                        player.puyoStatus.y = 12;
                        // 接地させる
                        player.fall();
                        // 接地フレームがplayerGroundFrameを超えるまでfallを呼ぶ
                        let result = false;
                        for (let i = 0; i < config.playerGroundFrame + 1; i++) {
                            result = player.fall() as boolean;
                        }

                        expect(result).toBeTruthy();
                    });
                });

                describe("ブロックがない", () => {
                    describe("下にブロックがないなら自由落下してよい。プレイヤー操作中の自由落下処理をする", () => {
                        it("下キーが押されているならもっと加速する", () => {
                            player.fall(false);
                            const normalFallTop = player.puyoStatus.top;

                            player.fall(true);
                            const downFallTop = player.puyoStatus.top;

                            expect(downFallTop).toBeGreaterThan(normalFallTop);
                        });
                    });
                });
            });

            describe("ブロックの境を超えたので、再チェックする", () => {
                it("下キーが押されていたら、得点を計算する", () => {
                    const initialY = player.puyoStatus.y;
                    while(player.puyoStatus.y === initialY) {
                        player.fall(true);
                    }
                    // 落下してyが変わったのでスコアが加算されているはず
                    expect(score.score).toBeGreaterThan(0);
                });

                it("境を超えたが特に問題はなかった。次回も自由落下を続ける", () => {
                    [...Array(3).keys()].forEach((frame) => {
                        player.playing(frame);
                    });
                    const result = player.groundFrame;

                    expect(result).toEqual(0);
                });

                it("境を超えたらブロックにぶつかった。位置を調整して、接地を開始する", () => {
                    [...Array(96).keys()].forEach((frame) => {
                        player.playing(frame);
                    });

                    expect(player.groundFrame).toEqual(0);
                });

                it("自由落下で特に問題はなかった。次回も自由落下を続ける", () => {
                    [...Array(3).keys()].forEach((frame) => {
                        player.playing(frame);
                    });
                    const result = player.groundFrame;

                    expect(result).toEqual(0);
                });

                describe("下にブロックがある", () => {
                    it("初接地である。接地を開始する", () => {
                        [...Array(96).keys()].forEach((frame) => {
                            player.playing(frame);
                        });
                        const result = player.groundFrame;

                        expect(result).toEqual(0);
                    });


                    it("ぷよを固定する", () => {
                        let result;
                        [...Array(96).keys()].forEach((frame) => {
                            player.playing(frame);
                        });
                        [...Array(40).keys()].forEach((frame) => {
                            result = player.playing(frame);
                        });

                        expect(player.groundFrame).toEqual(0);
                        expect(result).toBeTruthy();
                    });
                });
            });
        });

        describe("自由落下中", () => {
            describe("左右を確認する", () => {
                it("左に移動できる", () => {
                    let result;
                    document.dispatchEvent(new KeyboardEvent("keydown", {keyCode: 37}));
                    [...Array(1).keys()].forEach((frame) => {
                        result = player.playing(frame);
                    });

                    expect(result).toEqual("moving");
                });

                it("左に移動できない", () => {
                    let result;
                    document.dispatchEvent(new KeyboardEvent("keydown", {keyCode: 37}));
                    [...Array(90).keys()].forEach((frame) => {
                        result = player.playing(frame);
                    });

                    expect(result).toEqual("playing");
                });

                it("右に移動できる", () => {
                    let result;
                    document.dispatchEvent(new KeyboardEvent("keydown", {keyCode: 39}));
                    [...Array(1).keys()].forEach((frame) => {
                        result = player.playing(frame);
                    });

                    expect(result).toEqual("moving");
                });

                it("右に移動できない", () => {
                    let result;
                    document.dispatchEvent(new KeyboardEvent("keydown", {keyCode: 39}));
                    [...Array(90).keys()].forEach((frame) => {
                        result = player.playing(frame);
                    });

                    expect(result).toEqual("playing");
                });
            });

            describe("設置していない場合は、さらに１個下のブロックの左右も確認する", () => {
                it("左を確認する", () => {
                    let result;
                    [...Array(3).keys()].forEach((frame) => {
                        document.dispatchEvent(
                            new KeyboardEvent("keydown", {keyCode: 37})
                        );
                        result = player.playing(frame);
                    });

                    expect(result).toEqual("playing");
                });

                it("右を確認する", () => {
                    let result;
                    [...Array(5).keys()].forEach((frame) => {
                        document.dispatchEvent(
                            new KeyboardEvent("keydown", {keyCode: 39})
                        );
                        result = player.playing(frame);
                    });

                    expect(result).toEqual("playing");
                });
            });

            it("動かすことができるので、移動先情報をセットして移動状態にする", () => {
                let result;
                document.dispatchEvent(new KeyboardEvent("keydown", {keyCode: 37}));
                [...Array(1).keys()].forEach((frame) => {
                    result = player.playing(frame);
                });

                expect(result).toEqual("moving");
            });

            it("移動中も自由落下はさせる", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 }));
                [...Array(1).keys()].forEach((frame) => {
                    player.playing(frame);
                });

                expect(player.moving(1)).toBeTruthy();
            });

            it("回転中も自然落下はさせる", () => {
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 }));
                [...Array(1).keys()].forEach((frame) => {
                    player.playing(frame);
                });

                expect(player.rotating(1)).toBeTruthy();
            });

            it("現在のぷよをステージ上に配置する", () => {
                stage.setPuyo(1, 0, 1);
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 }));
                [...Array(1).keys()].forEach((frame) => {
                    player.playing(frame);
                });

                expect(player.fix()).toEqual(undefined);
            });
        });

        describe("回転を確認する", () => {
            it("右から上には100%確実に回せる。何もしない", () => {
                let result;
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 }));
                [...Array(1).keys()].forEach((frame) => {
                    result = player.playing(frame);
                });

                expect(result).toEqual("rotating");
            });
            it("上から左に回すときに、左にブロックがあれば右に移動する必要があるのでまず確認する", () => {
                let result;
                stage.setPuyo(1, 0, 1);
                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 }));
                [...Array(1).keys()].forEach((frame) => {
                    result = player.playing(frame);
                });

                expect(result).toEqual("rotating");
            });
            it("左から下に回す時には,自分の下か左下にブロックがあれば1個上に引き上げる。まず下を確認する", () => {
                // rotationを180(左向き)に設定
                player.puyoStatus.rotation = 180;
                player.puyoStatus.dx = -1;
                player.puyoStatus.dy = 0;

                stage.board[2][2] = 1; // (x, y) = (2, 2) にぷよを置く

                player.puyoStatus.x = 2;
                player.puyoStatus.y = 0;

                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 })); // 上キーで時計回り回転
                const result = player.playing(0);

                expect(result).toEqual("rotating");
                expect(player.puyoStatus.y).toEqual(-1); // 1段上に引き上げられる
            });
            it("左下も確認する", () => {
                // rotationを180(左向き)に設定
                player.puyoStatus.rotation = 180;
                player.puyoStatus.dx = -1;
                player.puyoStatus.dy = 0;

                stage.board[2][1] = 1; // (x, y) = (1, 2) にぷよを置く

                player.puyoStatus.x = 2;
                player.puyoStatus.y = 0;

                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 })); // 上キーで時計回り回転
                const result = player.playing(0);

                expect(result).toEqual("rotating");
                expect(player.puyoStatus.y).toEqual(-1); // 1段上に引き上げられる
            });
            it("下から右に回すときは,右にブロックがあれば左に移動する必要がるのでまず確認する", () => {
                // rotationを270(下向き)に設定
                player.puyoStatus.rotation = 270;
                player.puyoStatus.dx = 0;
                player.puyoStatus.dy = 1;

                stage.board[1][3] = 1; // (x, y) = (3, 1) にぷよを置く

                player.puyoStatus.x = 2;
                player.puyoStatus.y = 0;

                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 })); // 上キーで時計回り回転
                const result = player.playing(0);

                expect(result).toEqual("rotating");
                expect(player.puyoStatus.x).toEqual(1); // 1マス左にずれる
            });
            it("左にずれる必要がある時,左にもブロックがあれば回転出来ないので確認する", () => {
                // rotationを270(下向き)に設定
                player.puyoStatus.rotation = 270;
                player.puyoStatus.dx = 0;
                player.puyoStatus.dy = 1;

                stage.board[1][3] = 1; // (x, y) = (3, 1) にぷよを置く
                stage.board[1][1] = 1; // (x, y) = (1, 1) にぷよを置く

                player.puyoStatus.x = 2;
                player.puyoStatus.y = 0;

                document.dispatchEvent(new KeyboardEvent("keydown", { keyCode: 38 })); // 上キーで時計回り回転
                const result = player.playing(0);

                expect(result).toEqual("playing");
            });
        });
    });
});
