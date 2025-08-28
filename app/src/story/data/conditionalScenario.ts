import type { ScenarioData } from '../types'

/**
 * 条件分岐付きサンプルシナリオ
 */
export const conditionalScenario: ScenarioData = {
  id: 'conditional-story',
  title: '魔法の森の謎',
  scenes: [
    {
      id: 'forest_entrance',
      text: '魔法の森の入り口に立っている。\n古い看板には「真の勇者のみが進むべし」と書かれている。',
      character: 'narrator',
    },
    {
      id: 'find_sword',
      text: '道端で光る剣を発見した！',
      character: 'narrator',
      choices: [
        {
          id: 'take_sword',
          text: '剣を拾う',
          nextSceneId: 'sword_acquired',
        },
        {
          id: 'ignore_sword',
          text: '剣を無視して進む',
          nextSceneId: 'continue_without_sword',
        },
      ],
    },
    {
      id: 'sword_acquired',
      text: '勇者の剣を手に入れた！これで魔物にも立ち向かえるだろう。',
      character: 'narrator',
    },
    {
      id: 'continue_without_sword',
      text: '剣を置いて先に進んだ。平和主義者らしい選択だ。',
      character: 'narrator',
    },
    {
      id: 'encounter_guardian',
      text: '森の守護者が現れた！',
      character: 'guardian',
    },
    {
      id: 'guardian_challenge',
      text: 'この森を通りたければ、我が試練を受けよ！',
      character: 'guardian',
      choices: [
        {
          id: 'accept_challenge',
          text: '試練を受ける',
          nextSceneId: 'battle_scene',
        },
        {
          id: 'peaceful_solution',
          text: '平和的な解決を提案する',
          nextSceneId: 'peace_attempt',
          condition: { flag: 'hasSword', value: false },
        },
      ],
    },
    {
      id: 'battle_scene',
      text: '守護者との戦いが始まった！',
      character: 'narrator',
      condition: { flag: 'hasSword', value: true },
    },
    {
      id: 'battle_victory',
      text: '勇者の剣の力で守護者を倒した！森の奥へ進むことができる。',
      character: 'narrator',
      condition: { flag: 'hasSword', value: true },
    },
    {
      id: 'peace_attempt',
      text: '「争いは望んでいません。森を守るあなたの気持ちも理解できます」',
      character: 'player',
    },
    {
      id: 'peaceful_resolution',
      text: '守護者は感心したようだ。「お前の心は清らかだ。通るがよい」',
      character: 'guardian',
    },
    {
      id: 'forest_heart',
      text: '森の奥にある聖なる泉を発見した。\nあなたの選択によって、異なる結末を迎えることになる...',
      character: 'narrator',
    },
  ],
}
