defmodule Mix.Tasks.Quality do
  @moduledoc """
  コード品質チェックのための統合タスクです。

  以下のチェックを順番に実行します：
  - mix format --check-formatted (フォーマットチェック)
  - mix credo --strict (静的解析)
  - mix test (テスト実行)
  - mix coveralls (カバレッジ測定)

  ## 使用例

      mix quality

  すべてのチェックが通った場合のみ成功します。
  """

  use Mix.Task

  @shortdoc "コード品質の統合チェック"

  def run(_args) do
    Mix.shell().info("==> コード品質チェックを開始します")

    # 1. フォーマットチェック
    Mix.shell().info("\n==> フォーマットチェック")
    {_, format_exit} = System.cmd("mix", ["format", "--check-formatted"], stderr_to_stdout: true)

    if format_exit == 0 do
      Mix.shell().info("✓ フォーマットチェック 成功")
    else
      Mix.shell().error("✗ フォーマットチェック 失敗")
      System.halt(1)
    end

    # 2. 静的解析
    Mix.shell().info("\n==> 静的解析")
    {_, credo_exit} = System.cmd("mix", ["credo"], stderr_to_stdout: true)

    if credo_exit == 0 do
      Mix.shell().info("✓ 静的解析 成功")
    else
      Mix.shell().error("✗ 静的解析 失敗")
      System.halt(1)
    end

    # 3. テスト実行
    Mix.shell().info("\n==> テスト実行")
    {_, test_exit} = System.cmd("mix", ["test"], env: [{"MIX_ENV", "test"}], stderr_to_stdout: true)

    if test_exit == 0 do
      Mix.shell().info("✓ テスト実行 成功")
    else
      Mix.shell().error("✗ テスト実行 失敗")
      System.halt(1)
    end

    # 4. カバレッジ測定
    Mix.shell().info("\n==> カバレッジ測定")

    {_, coverage_exit} =
      System.cmd("mix", ["coveralls"], env: [{"MIX_ENV", "test"}], stderr_to_stdout: true)

    if coverage_exit == 0 do
      Mix.shell().info("✓ カバレッジ測定 成功")
    else
      Mix.shell().error("✗ カバレッジ測定 失敗")
      System.halt(1)
    end

    Mix.shell().info("\n🎉 すべての品質チェックが完了しました！")
  end
end
