import {Config} from "./config";
import type {PuyoImage} from "./puyoimage";

type PuyoCell = {
    puyo: number;
    element: HTMLImageElement;
}

type FallingPuyo = {
    element: HTMLImageElement;
    position: number;
    destination: number;
    falling: boolean;
}

type PuyoInfo = {
    x: number;
    y: number;
    cell: PuyoCell;
}

type EraseResult = {
    piece: number;
    color: number;
}

export class Stage {
    public stageElement: HTMLElement;
    public scoreElement: HTMLElement;
    public puyoCount: number;
    public board: (PuyoCell | null)[][];
    public zenkeshiImage: HTMLImageElement;
    public fallingPuyoList: FallingPuyo[];
    public erasingPuyoInfoList: PuyoInfo[];
    private readonly config: Config;
    private readonly puyoImage: PuyoImage;
    private eraseStartFrame: number = 0;


    constructor(config: Config, puyoImage: PuyoImage) {
        // HTMLからステージの元となる要素を取得し、大きさを設定する
        const stageElement = document.getElementById("stage") as HTMLElement;
        stageElement.style.width = config.puyoImageWidth * config.stageCols + "px";
        stageElement.style.height =
            config.puyoImageHeight * config.stageRows + "px";
        stageElement.style.backgroundColor = config.stageBackgroundColor;
        this.stageElement = stageElement;

        const zenkeshiImage = document.getElementById("zenkeshi") as HTMLImageElement;
        zenkeshiImage.width = config.puyoImageWidth * 6;
        zenkeshiImage.style.position = "absolute";
        zenkeshiImage.style.display = "none";
        this.zenkeshiImage = zenkeshiImage;
        stageElement.appendChild(zenkeshiImage);

        const scoreElement = document.getElementById("score") as HTMLElement;
        scoreElement.style.backgroundColor = config.scoreBackgroundColor;
        scoreElement.style.top = config.puyoImageHeight * config.stageRows + "px";
        scoreElement.style.width = config.puyoImageWidth * config.stageCols + "px";
        scoreElement.style.height = config.fontHeight + "px";
        this.scoreElement = scoreElement;

        // メモリを取得する
        this.board = [
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
        ].map(row => row.map(_ => null));

        let puyoCount = 0;
        for (let y = 0; y < config.stageRows; y++) {
            const line = this.board[y] || (this.board[y] = []);
            for (let x = 0; x < config.stageCols; x++) {
                const puyo = line[x];
                if (puyo && typeof puyo === 'number' && puyo >= 1 && puyo < 5) {
                    line[x] = { puyo: puyo, element: this.setPuyo(x, y, puyo) };
                    puyoCount++;
                } else {
                    line[x] = null;
                }
            }
        }
        this.puyoCount = puyoCount;
        this.puyoImage = puyoImage;
        this.fallingPuyoList = [];
        this.erasingPuyoInfoList = [];
        this.config = config;
    }

    // 画面とメモリ両方にpuyoをセットする
    public setPuyo(x: number, y: number, puyo: number): HTMLImageElement {
        // 画像を作成し配置する
        const puyoImage = this.puyoImage.getPuyo(puyo);
        puyoImage.style.left = x * this.config.puyoImageWidth + "px";
        puyoImage.style.top = y * this.config.puyoImageHeight + "px";
        this.stageElement.appendChild(puyoImage);
        // メモリにセットする
        this.board[y][x] = {
            puyo: puyo,
            element: puyoImage,
        };

        return puyoImage;
    }

    // 自由落下をチェックする
    public checkFall(): boolean {
        this.fallingPuyoList.length = 0;
        let isFalling = false;
        // 下の行から上の行を見ていく
        for (let y = this.config.stageRows - 2; y >= 0; y--) {
            const line = this.board[y];
            for (let x = 0; x < line.length; x++) {
                if (!this.board[y][x]) {
                    // このマスにぷよがなければ次
                    continue;
                }
                if (!this.board[y + 1][x]) {
                    // このぷよは落ちるので、取り除く
                    let cell = this.board[y][x] as PuyoCell;
                    this.board[y][x] = null;
                    let dst = y;
                    while (
                        dst + 1 < this.config.stageRows &&
                        this.board[dst + 1][x] == null
                        ) {
                        dst++;
                    }
                    // 最終目的地に置く
                    this.board[dst][x] = cell;
                    // 落ちるリストに入れる
                    this.fallingPuyoList.push({
                        element: cell.element,
                        position: y * this.config.puyoImageHeight,
                        destination: dst * this.config.puyoImageHeight,
                        falling: true,
                    });
                    // 落ちるものがあったことを記録しておく
                    isFalling = true;
                }
            }
        }
        return isFalling;
    }

    // 自由落下させる
    public fall(): boolean {
        let isFalling = false;
        for (const fallingPuyo of this.fallingPuyoList) {
            if (!fallingPuyo.falling) {
                // すでに自由落下が終わっている
                continue;
            }
            let position = fallingPuyo.position;
            position += this.config.freeFallingSpeed;
            if (position >= fallingPuyo.destination) {
                // 自由落下終了
                position = fallingPuyo.destination;
                fallingPuyo.falling = false;
            } else {
                // まだ落下しているぷよがあることを記録する
                isFalling = true;
            }
            // 新しい位置を保存する
            fallingPuyo.position = position;
            // ぷよを動かす
            fallingPuyo.element.style.top = position + "px";
        }
        return isFalling;
    }


    // 消せるかどうか判定する
    public checkErase(startFrame: number): EraseResult | null {
        this.eraseStartFrame = startFrame;
        this.erasingPuyoInfoList.length = 0;

        // 何色のぷよを消したかを記録する
        const erasedPuyoColor: Record<string, boolean> = {};

        // 隣接ぷよを確認する関数内関数を作成
        const sequencePuyoInfoList: PuyoInfo[] = [];
        const existingPuyoInfoList: PuyoInfo[] = [];
        const checkSequentialPuyo = (x: number, y: number): void => {
            // ぷよがあるか確認する
            const orig = this.board[y][x];
            if (!orig) {
                // ないなら何もしない
                return;
            }
            // あるなら一旦退避して、メモリ上から消す
            const puyo = this.board[y][x]!.puyo;
            sequencePuyoInfoList.push({
                x: x,
                y: y,
                cell: this.board[y][x] as PuyoCell,
            });
            this.board[y][x] = null;

            // 四方向の周囲ぷよを確認する
            const direction = [
                [0, 1],
                [1, 0],
                [0, -1],
                [-1, 0],
            ];
            for (let i = 0; i < direction.length; i++) {
                const dx = x + direction[i][0];
                const dy = y + direction[i][1];
                if (
                    dx < 0 ||
                    dy < 0 ||
                    dx >= this.config.stageCols ||
                    dy >= this.config.stageRows
                ) {
                    // ステージの外にはみ出た
                    continue;
                }
                const cell = this.board[dy][dx];
                if (!cell || cell.puyo !== puyo) {
                    // ぷよの色が違う
                    continue;
                }
                // そのまわりのぷよも消せるか確認する
                checkSequentialPuyo(dx, dy);
            }
        };

        // 実際に削除できるかの確認を行う
        for (let y = 0; y < this.config.stageRows; y++) {
            for (let x = 0; x < this.config.stageCols; x++) {
                sequencePuyoInfoList.length = 0;
                const puyoColor = this.board[y][x] && this.board[y][x]!.puyo;
                checkSequentialPuyo(x, y);
                if (
                    sequencePuyoInfoList.length == 0 ||
                    sequencePuyoInfoList.length < this.config.erasePuyoCount
                ) {
                    // 連続して並んでいる数が足りなかったので消さない
                    if (sequencePuyoInfoList.length) {
                        // 退避していたぷよを消さないリストに追加する
                        existingPuyoInfoList.push(...sequencePuyoInfoList);
                    }
                } else {
                    // これらは消して良いので消すリストに追加する
                    this.erasingPuyoInfoList.push(...sequencePuyoInfoList);
                    if (puyoColor !== null && puyoColor !== undefined) {
                        erasedPuyoColor[puyoColor] = true;
                    }
                }
            }
        }
        this.puyoCount -= this.erasingPuyoInfoList.length;

        // 消さないリストに入っていたぷよをメモリに復帰させる
        for (const info of existingPuyoInfoList) {
            this.board[info.y][info.x] = info.cell;
        }

        if (this.erasingPuyoInfoList.length) {
            // もし消せるならば、消えるぷよの個数と色の情報をまとめて返す
            return {
                piece: this.erasingPuyoInfoList.length,
                color: parseInt(Object.keys(erasedPuyoColor)[0], 10),
            };
        }
        return null;
    }

    // 消すアニメーションをする
    public erasing(frame: number): boolean {
        const elapsedFrame = frame - this.eraseStartFrame;
        const ratio = elapsedFrame / this.config.eraseAnimationDuration;
        if (ratio > 1) {
            // アニメーションを終了する
            for (const info of this.erasingPuyoInfoList) {
                const element = info.cell.element;
                this.stageElement.removeChild(element);
            }
            return false;
        } else if (ratio > 0.75) {
            for (const info of this.erasingPuyoInfoList) {
                const element = info.cell.element;
                element.style.display = "block";
            }
            return true;
        } else if (ratio > 0.5) {
            for (const info of this.erasingPuyoInfoList) {
                const element = info.cell.element;
                element.style.display = "none";
            }
            return true;
        } else if (ratio > 0.25) {
            for (const info of this.erasingPuyoInfoList) {
                const element = info.cell.element;
                element.style.display = "block";
            }
            return true;
        } else {
            for (const info of this.erasingPuyoInfoList) {
                const element = info.cell.element;
                element.style.display = "none";
            }
            return true;
        }
    }

    public showZenkeshi(): void {
        // 全消しを表示する
        this.zenkeshiImage.style.display = "block";
        this.zenkeshiImage.style.opacity = "1";
        const startTime = Date.now();
        const startTop = this.config.puyoImageHeight * this.config.stageRows;
        const endTop = (this.config.puyoImageHeight * this.config.stageRows) / 3;
        const animation = () => {
            const ratio = Math.min(
                (Date.now() - startTime) / this.config.zenkeshiDuration,
                1
            );
            this.zenkeshiImage.style.top =
                (endTop - startTop) * ratio + startTop + "px";
            if (ratio !== 1) {
                requestAnimationFrame(animation);
            }
        };
        animation();
    }

    public hideZenkeshi(): void {
        // 全消しを消去する
        const startTime = Date.now();
        const animation = () => {
            const ratio = Math.min(
                (Date.now() - startTime) / this.config.zenkeshiDuration,
                1
            );
            this.zenkeshiImage.style.opacity = String(1 - ratio);
            if (ratio !== 1) {
                requestAnimationFrame(animation);
            } else {
                this.zenkeshiImage.style.display = "none";
            }
        };
        animation();
    }
}
