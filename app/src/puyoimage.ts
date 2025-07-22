import {Config} from "./config";
import type {Stage} from "./stage";

export class PuyoImage {
    private readonly config: Config;
    puyoImages: HTMLImageElement[] = [];
    batankyuImage: HTMLImageElement;
    private gameOverFrame: number = 0;

    constructor(config: Config) {
        this.config = config;

        for (let i = 0; i < 5; i++) {
            const image = document.getElementById(`puyo_${i + 1}`) as HTMLImageElement;
            image.removeAttribute("id");
            image.width = this.config.puyoImageWidth;
            image.height = this.config.puyoImageHeight;
            image.style.position = "absolute";
            this.puyoImages[i] = image;
        }
        this.batankyuImage = document.getElementById("batankyu") as HTMLImageElement;
        this.batankyuImage.width = this.config.puyoImageWidth * 6;
        this.batankyuImage.style.position = "absolute";
    }

    public getPuyo(index: number): HTMLImageElement {
        const image = this.puyoImages[index - 1].cloneNode(true) as HTMLImageElement;
        return image;
    }

    public prepareBatankyu(frame: number, stage: Stage): void {
        this.gameOverFrame = frame;
        stage.stageElement.appendChild(this.batankyuImage);
        this.batankyuImage.style.top = -this.batankyuImage.height + "px";
    }

    public batankyu(frame: number): void {
        const ratio = (frame - this.gameOverFrame) / this.config.gameOverFrame;
        const x =
            Math.cos(Math.PI / 2 + ratio * Math.PI * 2 * 10) * this.config.puyoImageWidth;
        const y =
            (Math.cos(Math.PI + ratio * Math.PI * 2) *
                this.config.puyoImageHeight *
                this.config.stageRows) /
            4 +
            (this.config.puyoImageHeight * this.config.stageRows) / 2;
        this.batankyuImage.style.left = x + "px";
        this.batankyuImage.style.top = y + "px";
    }
}
