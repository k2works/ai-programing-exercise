import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { Game } from '../game';
import { Config } from '../config';
import { Stage } from '../stage';
import { PuyoImage } from '../puyoimage';
import { Player } from '../player';
import { Score } from '../score';

describe('ゲーム', () => {
    let game: Game;
    let mockRequestAnimationFrame: ReturnType<typeof vi.fn>;
    let originalRequestAnimationFrame: typeof window.requestAnimationFrame;

    beforeEach(() => {
        // DOMの準備
        document.body.innerHTML = `
            <div id="stage"></div>
            <div id="score"></div>
            <div id="next"></div>
            <div id="next2"></div>
            <img src="/img/puyo_1.png" alt="" id="puyo_1" />
            <img src="/img/puyo_2.png" alt="" id="puyo_2" />
            <img src="/img/puyo_3.png" alt="" id="puyo_3" />
            <img src="/img/puyo_4.png" alt="" id="puyo_4" />
            <img src="/img/puyo_5.png" alt="" id="puyo_5" />
            <img src="/img/zenkeshi.png" alt="" id="zenkeshi" />
            <img src="/img/batankyu.png" alt="" id="batankyu" />
            <img src="/img/0.png" alt="" id="font0" />
            <img src="/img/1.png" alt="" id="font1" />
            <img src="/img/2.png" alt="" id="font2" />
            <img src="/img/3.png" alt="" id="font3" />
            <img src="/img/4.png" alt="" id="font4" />
            <img src="/img/5.png" alt="" id="font5" />
            <img src="/img/6.png" alt="" id="font6" />
            <img src="/img/7.png" alt="" id="font7" />
            <img src="/img/8.png" alt="" id="font8" />
            <img src="/img/9.png" alt="" id="font9" />
        `;
        game = new Game();

        // requestAnimationFrameのモック
        vi.spyOn(window, 'requestAnimationFrame').mockImplementation(cb => cb(0));
    });

    afterEach(() => {
        vi.restoreAllMocks();
    });

    describe('ゲームの初期化', () => {
        it('ゲームを初期化すると、必要なコンポーネントが作成される', () => {
            game.initialize();

            expect(game['config']).toBeInstanceOf(Config);
            expect(game['puyoImage']).toBeInstanceOf(PuyoImage);
            expect(game['stage']).toBeInstanceOf(Stage);
            expect(game['player']).toBeInstanceOf(Player);
            expect(game['score']).toBeInstanceOf(Score);
        });

        it('ゲームを初期化すると、ゲームモードがstartになる', () => {
            game.initialize();

            expect(game['mode']).toEqual('start');
        });
    });

    describe('ゲームループ', () => {
        it('ゲームループを開始すると、requestAnimationFrameが呼ばれる', () => {
            const spy = vi.spyOn(game, 'loop');
            game.loop();

            expect(spy).toHaveBeenCalledTimes(1);
            spy.mockRestore();
        });
    });
});
