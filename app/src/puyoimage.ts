import {Config} from "./config";
import type {Stage} from "./stage";

const puyoColorMap: { [key: number]: string } = {
    1: 'green',
    2: 'blue',
    3: 'purple',
    4: 'red',
    5: '#f0f000', // yellow
};

export class PuyoImage {
    private readonly config: Config;
    batankyuElement: HTMLElement;
    private gameOverFrame: number = 0;

    constructor(config: Config) {
        this.config = config;

        // Create batankyu element programmatically
        this.batankyuElement = document.createElement('div');
        this.batankyuElement.textContent = 'GAME OVER';
        this.batankyuElement.style.position = "absolute";
        this.batankyuElement.style.width = (this.config.puyoImageWidth * 6) + 'px';
        this.batankyuElement.style.color = 'red';
        this.batankyuElement.style.fontSize = '48px';
        this.batankyuElement.style.fontWeight = 'bold';
        this.batankyuElement.style.textAlign = 'center';
        this.batankyuElement.style.display = 'none'; // Initially hidden
    }

    public getPuyo(puyoColor: number): HTMLElement {
        const puyoElement = document.createElement('div');
        puyoElement.style.width = this.config.puyoImageWidth + 'px';
        puyoElement.style.height = this.config.puyoImageHeight + 'px';
        puyoElement.style.backgroundColor = puyoColorMap[puyoColor] || 'black';
        puyoElement.style.borderRadius = '50%';
        puyoElement.style.position = 'absolute';
        puyoElement.style.border = '1px solid black';
        puyoElement.style.boxSizing = 'border-box';
        return puyoElement;
    }

    public prepareBatankyu(frame: number, stage: Stage): void {
        this.gameOverFrame = frame;
        this.batankyuElement.style.display = 'block';
        stage.stageElement.appendChild(this.batankyuElement);
        this.batankyuElement.style.top = -this.batankyuElement.offsetHeight + "px";
    }

    public batankyu(frame: number): void {
        const ratio = (frame - this.gameOverFrame) / this.config.gameOverFrame;
        const x =
            Math.cos(Math.PI / 2 + ratio * Math.PI * 2 * 10) * this.config.puyoImageWidth;
        const y = (this.config.puyoImageHeight * this.config.stageRows) / 2 * 1.5;
        this.batankyuElement.style.left = x.toFixed(2) + "px";
        this.batankyuElement.style.top = y.toFixed(2) + "px";
    }
}
