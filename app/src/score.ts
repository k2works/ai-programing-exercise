import {Config} from "./config";
import type {Stage} from "./stage";

export class Score {
    public score: number = 0;
    private readonly stage: Stage;
    private readonly config: Config;

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
        this.config = config;
        this.showScore();
    }

    public showScore(): void {
        const scoreElement = this.stage.scoreElement;
        scoreElement.textContent = this.score.toString().padStart(9, '0');
        scoreElement.style.fontSize = this.config.fontHeight + 'px';
        scoreElement.style.fontFamily = 'monospace';
        scoreElement.style.color = 'white';
        scoreElement.style.textAlign = 'right';
        scoreElement.style.paddingRight = '10px';
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
