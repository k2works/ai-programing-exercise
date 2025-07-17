import { Game } from './game'

// ぷよぷよゲームのエントリーポイント

console.log('ぷよぷよゲーム開始!')

// DOMが読み込まれてからゲームを開始
document.addEventListener('DOMContentLoaded', () => {
	// 必要なDOM要素がHTMLに存在するかチェック
	const requiredElements = ['stage', 'score', 'next', 'next2']
	const missingElements = requiredElements.filter(id => !document.getElementById(id))

	if (missingElements.length > 0) {
		console.error('必要なDOM要素が見つかりません:', missingElements)
		return
	}

	// 必要な画像要素がHTMLに存在するかチェック
	const requiredImages = ['puyo1', 'puyo2', 'puyo3', 'puyo4', 'puyo5', 'batankyu']
	const requiredFonts = Array.from({ length: 10 }, (_, i) => `font${i}`)
	const allRequiredImages = [...requiredImages, ...requiredFonts]

	const missingImages = allRequiredImages.filter(id => !document.getElementById(id))

	if (missingImages.length > 0) {
		console.error('必要な画像要素が見つかりません:', missingImages)
		return
	}

	// ゲームを初期化して開始
	const game = new Game()
	game.start()
})
