import { describe, it, expect, beforeEach, vi } from 'vitest'
import { DialogueBox } from './DialogueBox'
import * as Phaser from 'phaser'

// Phaserオブジェクトをモック化
const mockScene = {
  scale: {
    height: 600, // テスト用の画面高さ
  },
  add: {
    rectangle: vi.fn().mockReturnValue({
      setOrigin: vi.fn().mockReturnThis(),
      setAlpha: vi.fn().mockReturnThis(),
      setStrokeStyle: vi.fn().mockReturnThis(),
      setFillStyle: vi.fn().mockReturnThis(),
      setVisible: vi.fn().mockReturnThis(),
    }),
    text: vi.fn().mockReturnValue({
      setOrigin: vi.fn().mockReturnThis(),
      setVisible: vi.fn().mockReturnThis(),
      setWordWrapWidth: vi.fn().mockReturnThis(),
      setText: vi.fn().mockReturnThis(),
      setStyle: vi.fn().mockReturnThis(),
    }),
  },
  input: {
    on: vi.fn(),
  },
  tweens: {
    add: vi.fn(),
  },
}

describe('DialogueBox', () => {
  let dialogueBox: DialogueBox

  beforeEach(() => {
    dialogueBox = new DialogueBox(mockScene as unknown as Phaser.Scene, 400, 500)
  })

  describe('ダイアログボックス初期化', () => {
    it('ダイアログボックスが作成される', () => {
      expect(dialogueBox).toBeDefined()
      expect(dialogueBox.isVisible).toBe(false)
    })

    it('指定された位置とサイズで初期化される', () => {
      expect(dialogueBox.width).toBe(400)
      expect(dialogueBox.height).toBe(500) // 指定した高さ
    })
  })

  describe('表示機能', () => {
    it('showメソッドでダイアログボックスを表示できる', () => {
      dialogueBox.show()
      expect(dialogueBox.isVisible).toBe(true)
    })

    it('hideメソッドでダイアログボックスを非表示にできる', () => {
      dialogueBox.show()
      dialogueBox.hide()
      expect(dialogueBox.isVisible).toBe(false)
    })
  })

  describe('テキスト表示機能', () => {
    it('setTextでテキストを設定できる', () => {
      const testText = 'こんにちは、テストです。'
      dialogueBox.setText(testText)
      expect(dialogueBox.currentText).toBe(testText)
    })

    it('setTextAndShowでテキスト設定と表示を同時に行える', () => {
      const testText = 'テスト文章です。'
      dialogueBox.setTextAndShow(testText)
      expect(dialogueBox.currentText).toBe(testText)
      expect(dialogueBox.isVisible).toBe(true)
    })
  })

  describe('話者名表示機能', () => {
    it('setSpeakerで話者名を設定できる', () => {
      const speakerName = 'アリス'
      dialogueBox.setSpeaker(speakerName)
      expect(dialogueBox.currentSpeaker).toBe(speakerName)
    })

    it('話者名が空の場合は非表示になる', () => {
      dialogueBox.setSpeaker('')
      expect(dialogueBox.currentSpeaker).toBe('')
    })
  })

  describe('メッセージ送り機能', () => {
    it('nextMessageで次のメッセージに進める', () => {
      const messages = ['メッセージ1', 'メッセージ2', 'メッセージ3']
      dialogueBox.setMessages(messages)

      expect(dialogueBox.currentMessageIndex).toBe(0)
      expect(dialogueBox.currentText).toBe('メッセージ1')

      dialogueBox.nextMessage()
      expect(dialogueBox.currentMessageIndex).toBe(1)
      expect(dialogueBox.currentText).toBe('メッセージ2')
    })

    it('最後のメッセージの後はisFinishedがtrueになる', () => {
      const messages = ['メッセージ1', 'メッセージ2']
      dialogueBox.setMessages(messages)

      dialogueBox.nextMessage() // メッセージ2
      dialogueBox.nextMessage() // 終了

      expect(dialogueBox.isFinished).toBe(true)
    })

    it('hasNextMessageで次のメッセージの有無を確認できる', () => {
      const messages = ['メッセージ1', 'メッセージ2']
      dialogueBox.setMessages(messages)

      expect(dialogueBox.hasNextMessage()).toBe(true)

      dialogueBox.nextMessage()
      expect(dialogueBox.hasNextMessage()).toBe(false)
    })
  })

  describe('イベントハンドリング', () => {
    it('クリックイベントでnextMessageが呼ばれる', () => {
      const nextMessageSpy = vi.spyOn(dialogueBox, 'nextMessage')

      // クリックイベントのシミュレート
      dialogueBox.handleClick()

      expect(nextMessageSpy).toHaveBeenCalled()
    })
  })
})
