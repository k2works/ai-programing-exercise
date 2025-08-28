import type { ScenarioData } from '../types'

/**
 * サンプルシナリオ: シンプルな選択肢付きストーリー
 */
export const sampleScenario: ScenarioData = {
  id: 'sample-story',
  title: 'はじまりの物語',
  scenes: [
    {
      id: 'opening',
      text: 'あなたは静かな森の中で目を覚ました。\n辺りを見回すと、二つの道が見える。',
      character: 'narrator',
      background: 'forest',
    },
    {
      id: 'choice_path',
      text: 'どちらの道を選びますか？',
      character: 'narrator',
      background: 'forest',
      choices: [
        {
          id: 'left_path',
          text: '左の道（光の差す方へ）',
          nextSceneId: 'light_path',
        },
        {
          id: 'right_path',
          text: '右の道（暗い洞窟の方へ）',
          nextSceneId: 'dark_path',
        },
        {
          id: 'stay',
          text: 'その場に留まる',
          nextSceneId: 'stay_scene',
        },
      ],
    },
    {
      id: 'light_path',
      text: '光の道を進むと、美しい花畑に出た。\n花の香りに包まれながら、あなたは平穏を感じた。',
      character: 'narrator',
      background: 'meadow',
    },
    {
      id: 'dark_path',
      text: '暗い洞窟に入ると、キラキラと光る宝石を発見した。\n危険だったが、貴重な宝を手に入れることができた。',
      character: 'narrator',
      background: 'cave',
    },
    {
      id: 'stay_scene',
      text: 'その場に留まっていると、賢そうな老人が現れた。',
      character: 'narrator',
      background: 'forest',
    },
    {
      id: 'wise_man_speaks',
      text: 'おや、迷子かな？どちらへ向かいたいのじゃ？',
      character: 'wise_man',
      background: 'forest',
      choices: [
        {
          id: 'ask_advice',
          text: 'アドバイスをお願いします',
          nextSceneId: 'advice_scene',
        },
        {
          id: 'decline_help',
          text: '大丈夫です、自分で決めます',
          nextSceneId: 'independent_scene',
        },
      ],
    },
    {
      id: 'advice_scene',
      text: '光の道は平穏をもたらすが、暗い道は試練と共に成長をもたらすじゃろう。\n選択はそなた次第じゃ。',
      character: 'wise_man',
      background: 'forest',
    },
    {
      id: 'independent_scene',
      text: 'ほほう、自立心があるようじゃな。それも立派な選択じゃ。',
      character: 'wise_man',
      background: 'forest',
    },
    {
      id: 'ending',
      text: 'あなたの冒険はまだ始まったばかり。\nこれからどんな物語が待っているのでしょうか...',
      character: 'narrator',
      background: 'sky',
    },
  ],
}
