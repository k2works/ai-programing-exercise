defmodule Mix.Tasks.Dev do
  @moduledoc """
  開発時によく使用するタスクのためのショートカットです。

  ## 使用例

      mix dev.test          # テストを実行
      mix dev.format        # コードをフォーマット
      mix dev.check         # フォーマットとCredoをチェック
      mix dev.coverage      # カバレッジレポートを生成

  """

  use Mix.Task

  @shortdoc "開発用ショートカットコマンド"

  def run(["test"]) do
    Mix.Task.run("test", [])
  end

  def run(["format"]) do
    Mix.Task.run("format", [])
    Mix.shell().info("✓ コードをフォーマットしました")
  end

  def run(["check"]) do
    Mix.shell().info("==> フォーマットチェック")
    Mix.Task.run("format", ["--check-formatted"])

    Mix.shell().info("==> 静的解析")
    Mix.Task.run("credo", [])

    Mix.shell().info("✓ チェック完了")
  end

  def run(["coverage"]) do
    Mix.Task.run("coveralls.html", [])
    Mix.shell().info("✓ カバレッジレポートを生成しました (cover/excoveralls.html)")
  end

  def run(_) do
    Mix.shell().info("""
    使用可能なコマンド:

      mix dev.test          # テストを実行
      mix dev.format        # コードをフォーマット  
      mix dev.check         # フォーマットとCredoをチェック
      mix dev.coverage      # カバレッジレポートを生成
    """)
  end
end
