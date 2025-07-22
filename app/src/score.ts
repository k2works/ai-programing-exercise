import {Config} from "./config";
import type {Stage} from "./stage";

export class Score {
    public score: number = 0;
    public fontLength: number = 0;
    public fontTemplateList: HTMLImageElement[] = [];
    private readonly stage: Stage;

    static readonly rensaBonus: number[] = [
        0,
        8,
        16,
        32,
        64,
        96,
        128,
        160,
        192,
        224,
        256,
        288,
        320,
        352,
        384,
        416,
        448,
        480,
        512,
        544,
        576,
        608,
        640,
        672,
    ];
    static readonly pieceBonus: number[] = [0, 0, 0, 0, 2, 3, 4, 5, 6, 7, 10, 10];
    static readonly colorBonus: number[] = [0, 0, 3, 6, 12, 24];

    constructor(config: Config, stage: Stage) {
        this.stage = stage;

        let fontWidth = 0;
        for (let i = 0; i < 10; i++) {
            const fontImage = document.getElementById(`font${i}`) as HTMLImageElement;
            if (fontWidth === 0) {
                fontWidth = (fontImage.width / fontImage.height) * config.fontHeight;
            }
            fontImage.height = config.fontHeight;
            fontImage.width = fontWidth;
            this.fontTemplateList.push(fontImage);
        }

        this.fontLength = Math.floor(
            (config.stageCols * config.puyoImageWidth) /
            this.fontTemplateList[0].width
        );

        // テストではイメージサイズの幅がマイナスになりフォント長が無限大になるので調整する
        if (this.fontLength === Infinity) {
            this.fontLength = 9;
        }

        this.showScore();
    }

    public showScore(): void {
        let score = this.score;
        const scoreElement = this.stage.scoreElement;
        // まず最初に、scoreElementの中身を空っぽにする
        while (scoreElement.firstChild) {
            scoreElement.removeChild(scoreElement.firstChild);
        }
        // スコアを下の桁から埋めていく
        for (let i = 0; i < this.fontLength; i++) {
            // 10で割ったあまりを求めて、一番下の桁を取り出す
            const number = score % 10;
            // 一番うしろに追加するのではなく、一番前に追加することで、スコアの並びを数字と同じようにする
            scoreElement.insertBefore(
                this.fontTemplateList[number].cloneNode(true),
                scoreElement.firstChild
            );
            // 10で割って次の桁の準備をしておく
            score = Math.floor(score / 10);
        }
    }

    public addScore(score: number): void {
        this.score += score;
        this.showScore();
    }

    public calculateScore(rensa: number, piece: number, color: number): void {
        rensa = Math.min(rensa, Score.rensaBonus.length - 1);
        piece = Math.min(piece, Score.pieceBonus.length - 1);
        color = Math.min(color, Score.colorBonus.length - 1);
        let scale = Score.rensaBonus[rensa] + Score.pieceBonus[piece] + Score.colorBonus[color];

        if (scale === 0) {
            scale = 1;
        }
        this.addScore(scale * piece * 10);
    }
}
