import {Config} from "./config";
import type {GameMode} from "./game";
import type {PuyoImage} from "./puyoimage";
import type {Score} from "./score";
import type {Stage} from "./stage";

type PuyoStatus = {
    x: number;
    y: number;
    left: number;
    top: number;
    dx: number;
    dy: number;
    rotation: number;
}

type KeyStatus = {
    right: boolean;
    left: boolean;
    up: boolean;
    down: boolean;
}

type TouchPoint = {
    xs: number;
    ys: number;
    xe: number;
    ye: number;
}

export class Player {
    private readonly config: Config;
    private stage: Stage | undefined;
    private score: Score | undefined;
    centerPuyo: number = 0;
    movablePuyo: number = 0;
    private centerPuyoElement: HTMLImageElement | null | undefined;
    private movablePuyoElement: HTMLImageElement | null | undefined;
    puyoStatus: PuyoStatus = {x: 0, y: 0, left: 0, top: 0, dx: 0, dy: 0, rotation: 0}
    groundFrame: number = 0;
    keyStatus: KeyStatus;
    touchPoint: TouchPoint;
    private actionStartFrame: number = 0;
    private moveSource: number = 0;
    private moveDestination: number = 0;
    private rotateFromRotation: number = 0;
    private rotateAfterLeft: number = 0;
    private rotateBeforeLeft: number = 0;

    constructor(config: Config) {
        this.config = config;

        // キーボードの入力を確認する
        this.keyStatus = {
            right: false,
            left: false,
            up: false,
            down: false,
        };
        // ブラウザのキーボードの入力を取得するイベントリスナを登録する
        document.addEventListener("keydown", (e: KeyboardEvent) => {
            // キーボードが押された場合
            switch (e.keyCode) {
                case 37: // 左向きのキー
                    this.keyStatus.left = true;
                    e.preventDefault();
                    return false;
                case 38: // 上向きのキー
                    this.keyStatus.up = true;
                    e.preventDefault();
                    return false;
                case 39: // 右向きのキー
                    this.keyStatus.right = true;
                    e.preventDefault();
                    return false;
                case 40: // 下向きのキー
                    this.keyStatus.down = true;
                    e.preventDefault();
                    return false;
            }
            return undefined;
        });
        document.addEventListener("keyup", (e: KeyboardEvent) => {
            // キーボードが離された場合
            switch (e.keyCode) {
                case 37: // 左向きキー
                    this.keyStatus.left = false;
                    e.preventDefault();
                    return false;
                case 38: // 上向きキー
                    this.keyStatus.up = false;
                    e.preventDefault();
                    return false;
                case 39: // 右向きキー
                    this.keyStatus.right = false;
                    e.preventDefault();
                    return false;
                case 40: // 下向きキー
                    this.keyStatus.down = false;
                    e.preventDefault();
                    return false;
            }
            return undefined;
        });
        // タッチ操作追加
        this.touchPoint = {
            xs: 0,
            ys: 0,
            xe: 0,
            ye: 0,
        };
        document.addEventListener("touchstart", (e: TouchEvent) => {
            this.touchPoint.xs = e.touches[0].clientX;
            this.touchPoint.ys = e.touches[0].clientY;
        });
        document.addEventListener("touchmove", (e: TouchEvent) => {
            // 指が少し動いた時は無視
            if (
                Math.abs(e.touches[0].clientX - this.touchPoint.xs) < 20 &&
                Math.abs(e.touches[0].clientY - this.touchPoint.ys) < 20
            ) {
                return;
            }

            // 指の動きからジェスチャーによるkeyStatusプロパティを更新
            this.touchPoint.xe = e.touches[0].clientX;
            this.touchPoint.ye = e.touches[0].clientY;
            const {xs, ys, xe, ye} = this.touchPoint;
            gesture(xs, ys, xe, ye);

            this.touchPoint.xs = this.touchPoint.xe;
            this.touchPoint.ys = this.touchPoint.ye;
        });
        document.addEventListener("touchend", () => {
            this.keyStatus.up = false;
            this.keyStatus.down = false;
            this.keyStatus.left = false;
            this.keyStatus.right = false;
        });

        // ジェスチャーを判定して、keyStatusプロパティを更新する関数
        const gesture = (xs: number, ys: number, xe: number, ye: number): void => {
            const horizonDirection = xe - xs;
            const verticalDirection = ye - ys;

            if (Math.abs(horizonDirection) < Math.abs(verticalDirection)) {
                // 縦方向
                if (verticalDirection < 0) {
                    // up
                    this.keyStatus.up = true;
                    this.keyStatus.down = false;
                    this.keyStatus.left = false;
                    this.keyStatus.right = false;
                } else if (0 <= verticalDirection) {
                    // down
                    this.keyStatus.up = false;
                    this.keyStatus.down = true;
                    this.keyStatus.left = false;
                    this.keyStatus.right = false;
                }
            } else {
                // 横方向
                if (horizonDirection < 0) {
                    // left
                    this.keyStatus.up = false;
                    this.keyStatus.down = false;
                    this.keyStatus.left = true;
                    this.keyStatus.right = false;
                } else if (0 <= horizonDirection) {
                    // right
                    this.keyStatus.up = false;
                    this.keyStatus.down = false;
                    this.keyStatus.left = false;
                    this.keyStatus.right = true;
                }
            }
        };
    }

    public createNewPuyo(stage: Stage, puyoImage: PuyoImage, score: Score): boolean {
        this.stage = stage;
        this.score = score;

        // ぷよぷよが置けるかどうか、１番上の段の左から３つ目を確認する
        if (this.stage.board[0][2] !== null) {
            // 置けない
            return false;
        }

        // 新しいぷよを２つ作成する
        this.centerPuyo = Math.floor(Math.random() * this.config.puyoColors) + 1;
        this.movablePuyo = Math.floor(Math.random() * this.config.puyoColors) + 1;

        // ぷよのDOMを作成する
        this.centerPuyoElement = puyoImage.getPuyo(this.centerPuyo);
        this.movablePuyoElement = puyoImage.getPuyo(this.movablePuyo);
        this.stage.stageElement.appendChild(this.centerPuyoElement);
        this.stage.stageElement.appendChild(this.movablePuyoElement);

        // ぷよの初期位置をきめる
        this.puyoStatus.x = 2;
        this.puyoStatus.y = -1;
        this.puyoStatus.left = this.puyoStatus.x * this.config.puyoImageWidth;
        this.puyoStatus.top = this.puyoStatus.y * this.config.puyoImageHeight;
        this.puyoStatus.dx = 0;
        this.puyoStatus.dy = 1;
        this.puyoStatus.rotation = 0;
        this.groundFrame = 0;

        this.setPuyoPosition();

        return true;
    }

    public playing(frame: number): GameMode {
        if (this.stage === undefined) throw new Error("stage is undefined");

        // まず自由落下を確認する
        // 下キーが押されていたら、それ込みで自由落下させる
        if (this.falling(this.keyStatus.down)) {
            // 落下が終わっていたら、ぷよを固定する
            this.setPuyoPosition();
            return "fix";
        }
        this.setPuyoPosition();
        if (this.keyStatus.right || this.keyStatus.left) {
            // 左右を確認する
            const cx = this.keyStatus.right ? 1 : -1;
            const x = this.puyoStatus.x;
            const y = this.puyoStatus.y;
            const mx = x + this.puyoStatus.dx;
            const my = y + this.puyoStatus.dy;
            // その方向にブロックがないことを確認する
            // まずは自分の左右を確認
            let canMove = true;
            if (
                y < 0 ||
                x + cx < 0 ||
                x + cx >= this.config.stageCols ||
                this.stage.board[y][x + cx]
            ) {
                if (y >= 0) {
                    canMove = false;
                }
            }
            if (
                my < 0 ||
                mx + cx < 0 ||
                mx + cx >= this.config.stageCols ||
                this.stage.board[my][mx + cx]
            ) {
                if (my >= 0) {
                    canMove = false;
                }
            }
            // 設置していない場合は、さらに１個下のブロックの左右も確認する
            if (this.groundFrame === 0) {
                if (
                    y + 1 < 0 ||
                    x + cx < 0 ||
                    x + cx >= this.config.stageCols ||
                    this.stage.board[y + 1][x + cx]
                ) {
                    if (y + 1 >= 0) {
                        canMove = false;
                    }
                }
                if (
                    my + 1 < 0 ||
                    mx + cx < 0 ||
                    mx + cx >= this.config.stageCols ||
                    this.stage.board[my + 1][mx + cx]
                ) {
                    if (my + 1 >= 0) {
                        canMove = false;
                    }
                }
            }

            if (canMove) {
                // 動かすことが出来るので、移動先情報をセットして移動状態にする
                this.actionStartFrame = frame;
                this.moveSource = x * this.config.puyoImageWidth;
                this.moveDestination = (x + cx) * this.config.puyoImageWidth;
                this.puyoStatus.x += cx;
                return "moving";
            }
        } else if (this.keyStatus.up) {
            // 回転を確認する
            // 回せるかどうかは後で確認。まわすぞ
            const x = this.puyoStatus.x;
            const y = this.puyoStatus.y;
            const rotation = this.puyoStatus.rotation;
            let canRotate = true;

            let cx = 0;
            let cy = 0;

            if (rotation === 0) {
                // 右から上には100%確実に回せる。何もしない
            } else if (rotation === 90) {
                // 上から左に回すときに,左にブロックがあれば右に移動する必要があるのでまず確認をする
                if (
                    y + 1 < 0 ||
                    x - 1 < 0 ||
                    x - 1 >= this.config.stageCols ||
                    this.stage.board[y + 1][x - 1]
                ) {
                    if (y + 1 >= 0) {
                        // ブロックがある。右に1個ずれる
                        cx = 1;
                    }
                }
                // 右にずれる必要がある時,右にもブロックがあれば回転出来ないので確認する
                if (cx === 1) {
                    if (
                        y + 1 < 0 ||
                        x + 1 < 0 ||
                        y + 1 >= this.config.stageRows ||
                        x + 1 >= this.config.stageCols ||
                        this.stage.board[y + 1][x + 1]
                    ) {
                        if (y + 1 >= 0) {
                            // ブロックがある。回転出来なかった
                            canRotate = false;
                        }
                    }
                }
            } else if (rotation === 180) {
                // 左から下に回す時には、自分の下か左下にブロックがあれば1個上に引き上げる。まず下を確認する
                if (y + 2 < 0 || y + 2 >= this.config.stageRows || this.stage.board[y + 2][x]) {
                    if (y + 2 >= 0) {
                        // ブロックがある。上に引き上げる
                        cy = -1;
                    }
                }
                // 左下も確認する
                if (
                    y + 2 < 0 ||
                    y + 2 >= this.config.stageRows ||
                    x - 1 < 0 || this.stage.board[y + 2][x - 1])
                {
                    if (y + 2 >= 0) {
                        // ブロックがある。上に引き上げる
                        cy = -1;
                    }
                }
            } else if (rotation === 270) {
                // 下から右に回すときは、右にブロックがあれば左に移動する必要があるのでまず確認する
                if (
                    y + 1 < 0 ||
                    x + 1 < 0 ||
                    x + 1 >= this.config.stageCols ||
                    this.stage.board[y + 1][x + 1]
                ) {
                    if (y + 1 >= 0) {
                        // ブロックがある。左に1個ずれる
                        cx = -1;
                    }
                }
                // 左にずれる必要がある時、左にもブロックがあれば回転出来ないので確認する
                if (cx === -1) {
                    if (
                        y + 1 < 0 ||
                        x - 1 < 0 ||
                        x - 1 >= this.config.stageCols ||
                        this.stage.board[y + 1][x - 1]
                    ) {
                        if (y + 1 >= 0) {
                            // ブロックがある。回転出来なかった
                            canRotate = false;
                        }
                    }
                }
            }

            if (canRotate) {
                // 上に移動する必要があるときは、一気にあげてしまう
                if (cy === -1) {
                    if (this.groundFrame > 0) {
                        // 接地しているなら１段引き上げる
                        this.puyoStatus.y -= 1;
                        this.groundFrame = 0;
                    }
                    this.puyoStatus.top = this.puyoStatus.y * this.config.puyoImageHeight;
                }
                // 回すことが出来るので、回転後の情報をセットして回転状態にする
                this.actionStartFrame = frame;
                this.rotateBeforeLeft = x * this.config.puyoImageWidth;
                this.rotateAfterLeft = (x + cx) * this.config.puyoImageWidth;
                this.rotateFromRotation = this.puyoStatus.rotation;
                // 次の状態を先に設定しておく
                this.puyoStatus.x += cx;
                const distRotation = (this.puyoStatus.rotation + 90) % 360;
                const dCombi = [
                    [1, 0],
                    [0, -1],
                    [-1, 0],
                    [0, 1],
                ][distRotation / 90];
                this.puyoStatus.dx = dCombi[0];
                this.puyoStatus.dy = dCombi[1];
                return "rotating";
            }
        }

        return "playing";
    }

    public moving(frame: number) {
        // 移動中も自由落下はさせる
        this.falling();
        const ratio = Math.min(
            1,
            (frame - this.actionStartFrame) / this.config.playerMoveFrame
        );
        this.puyoStatus.left =
            ratio * (this.moveDestination - this.moveSource) + this.moveSource;
        this.setPuyoPosition();
        if (ratio === 1) {
            return false;
        }
        return true;
    }

    public rotating(frame: number) {
        // 回転中も自然落下はさせる
        this.falling();
        const ratio = Math.min(
            1,
            (frame - this.actionStartFrame) / this.config.playerRotateFrame);
        this.puyoStatus.left =
            (this.rotateAfterLeft - this.rotateBeforeLeft) * ratio +
            this.rotateBeforeLeft;
        this.puyoStatus.rotation = this.rotateFromRotation + ratio * 90;
        this.setPuyoPosition();
        if (ratio === 1) {
            this.puyoStatus.rotation = (this.rotateFromRotation + 90) % 360;
            return false;
        }
        return true;
    }
    
    public fix() {
        if (this.stage === undefined) throw new Error("stage is undefined");

        // 現在のぷよをステージ上に配置する
        const x = this.puyoStatus.x;
        const y = this.puyoStatus.y;
        const dx = this.puyoStatus.dx;
        const dy = this.puyoStatus.dy;
        if (y >= 0) {
            // 画面外のぷよは消してしまう
            this.stage.setPuyo(x, y, this.centerPuyo);
            this.stage.puyoCount++;
        }
        if (y + dy >= 0) {
            // 画面外のぷよは消してしまう
            this.stage.setPuyo(x + dx, y + dy, this.movablePuyo);
            this.stage.puyoCount++;
        }
        // 操作用に作成したぷよ画像を消す
        if (this.centerPuyoElement && this.movablePuyoElement) {
            this.stage.stageElement.removeChild(this.centerPuyoElement);
            this.stage.stageElement.removeChild(this.movablePuyoElement);
            this.centerPuyoElement = null;
            this.movablePuyoElement = null;
        }
    }

    public batankyu(): void {
        if (this.keyStatus.up) {
            location.reload();
        }
    }

    private falling(isDownPressed?: boolean): boolean | void {
        if (this.stage === undefined) throw new Error("stage is undefined");
        if (this.score === undefined) throw new Error("score is undefined");

        // 現状の場所の下にブロックがあるかどうか確認する
        let isBlocked = false;
        let x = this.puyoStatus.x;
        let y = this.puyoStatus.y;
        let dx = this.puyoStatus.dx;
        let dy = this.puyoStatus.dy;
        if (
            y + 1 >= this.config.stageRows ||
            this.stage.board[y + 1][x] ||
            (y + dy + 1 >= 0 &&
                (y + dy + 1 >= this.config.stageRows || this.stage.board[y + dy + 1][x + dx]))
        ) {
            isBlocked = true;
        }
        if (!isBlocked) {
            // 下にブロックがないなら自由落下してよい。プレイヤー操作中の自由落下処理をする
            this.puyoStatus.top += this.config.playerFallingSpeed;
            if (isDownPressed) {
                // 下キーが押されているならもっと加速する
                this.puyoStatus.top += this.config.playerDownSpeed;
            }
            if (Math.floor(this.puyoStatus.top / this.config.puyoImageHeight) != y) {
                // ブロックの境を超えたので、再チェックする
                // 下キーが押されていたら、得点を計算する
                if (isDownPressed) {
                    this.score.addScore(1);
                }
                y += 1;
                this.puyoStatus.y = y;
                if (
                    y + 1 >= this.config.stageRows ||
                    this.stage.board[y + 1][x] ||
                    (y + dy + 1 >= 0 &&
                        (y + dy + 1 >= this.config.stageRows || this.stage.board[y + dy + 1][x + dx]))
                ) {
                    isBlocked = true;
                }
                if (!isBlocked) {
                    // 境を超えたが特に問題はなかった。次回も自由落下を続ける
                    this.groundFrame = 0;
                    return;
                } else {
                    // 境を超えたらブロックにぶつかった。位置を調整して、接地を開始する
                    this.puyoStatus.top = y * this.config.puyoImageHeight;
                    this.groundFrame = 1;
                    return;
                }
            } else {
                // 自由落下で特に問題がなかった。次回も自由落下を続ける
                this.groundFrame = 0;
                return;
            }
        }
        if (this.groundFrame === 0) {
            // 初接地である。接地を開始する
            this.groundFrame = 1;
        } else {
            this.groundFrame++;
            if (this.groundFrame > this.config.playerGroundFrame) {
                return true;
            }
        }
    }

    private setPuyoPosition() {
        if (!this.centerPuyoElement || !this.movablePuyoElement) return;

        // ぷよの表示を更新する
        this.centerPuyoElement.style.left = this.puyoStatus.left + "px";
        this.centerPuyoElement.style.top = this.puyoStatus.top + "px";
        let x = this.puyoStatus.left;
        let y = this.puyoStatus.top;
        if (this.puyoStatus.rotation === 0) {
            // 上
            y -= this.config.puyoImageHeight;
        } else if (this.puyoStatus.rotation === 90) {
            // 右
            x += this.config.puyoImageWidth;
        } else if (this.puyoStatus.rotation === 180) {
            // 下
            y += this.config.puyoImageHeight;
        } else if (this.puyoStatus.rotation === 270) {
            // 左
            x -= this.config.puyoImageWidth;
        }
        this.movablePuyoElement.style.left = x + "px";
        this.movablePuyoElement.style.top = y + "px";
    }
}
