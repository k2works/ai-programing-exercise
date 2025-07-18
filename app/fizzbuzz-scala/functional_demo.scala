// 関数型プログラミングFizzBuzzデモンストレーション

import domain.model.{FizzBuzzValue, FizzBuzzList}
import domain.types.FizzBuzzType
import application.FizzBuzzCommand

object FunctionalDemo extends App {
  
  println("=== 関数型FizzBuzzデモンストレーション ===\n")
  
  // 1. 基本的なFizzBuzz
  println("1. 基本的なFizzBuzz:")
  println(s"FizzBuzz.generate(15) = ${FizzBuzz.generate(15)}")
  println()
  
  // 2. 高階関数の使用
  println("2. 高階関数 - generateWith:")
  val generateType1 = FizzBuzz.generateWith(FizzBuzzType.Type01)
  println(s"generateType1(3) = ${generateType1(3)}")
  println(s"generateType1(5) = ${generateType1(5)}")
  println(s"generateType1(15) = ${generateType1(15)}")
  println()
  
  // 3. 関数合成とtransform
  println("3. 関数合成 - transform:")
  val lengths = FizzBuzz.transform(1, 5)(_.value.length)
  println(s"FizzBuzz.transform(1, 5)(_.value.length) = $lengths")
  println()
  
  // 4. Option型の安全な処理
  println("4. Option型での安全な処理:")
  println(s"FizzBuzz.safeGenerate(3, 1) = ${FizzBuzz.safeGenerate(3, 1)}")
  println(s"FizzBuzz.safeGenerate(-1, 1) = ${FizzBuzz.safeGenerate(-1, 1)}")
  println()
  
  // 5. Either型のエラーハンドリング
  println("5. Either型でのエラーハンドリング:")
  println(s"FizzBuzz.generateEither(3, 1) = ${FizzBuzz.generateEither(3, 1)}")
  println(s"FizzBuzz.generateEither(-1, 1) = ${FizzBuzz.generateEither(-1, 1)}")
  println()
  
  // 6. 遅延評価ストリーム
  println("6. 遅延評価ストリーム:")
  val stream = FizzBuzz.stream(1)
  val first10 = stream.take(10).toList
  println(s"FizzBuzz.stream(1).take(10) = $first10")
  println()
  
  // 7. フィルタリング  
  println("7. 述語に基づくフィルタリング:")
  val fizzBuzzOnly = FizzBuzz.generateWithFilter(1, 15)(_.value.contains("z"))
  println(s"FizzBuzz.generateWithFilter(1, 15)(_.value.contains(\"z\")) = $fizzBuzzOnly")
  println()
  
  // 8. ファーストクラスコレクション操作
  println("8. ファーストクラスコレクション操作:")
  val list = FizzBuzzList.generate(1, 10, FizzBuzzType.Type01)
  val filtered = list.filter(_.value.contains("z"))
  val numbers = filtered.map(_.number)
  println(s"FizzBuzzList.generate(1, 10).filter(_.value.contains(\"z\")).map(_.number) = $numbers")
  println()
  
  // 9. Builderパターン
  println("9. 関数型ビルダーパターン:")
  val builtList = FizzBuzzList.builder
    .add(FizzBuzzValue(1, "1"))
    .add(FizzBuzzValue(3, "Fizz"))
    .add(FizzBuzzValue(5, "Buzz"))
    .build
  println(s"ビルダーで構築したリスト: ${builtList.toString}")
  println()
  
  // 10. パーティアル適用
  println("10. パーティアル適用:")
  val operations = FizzBuzzCommand.withType(FizzBuzzType.Type01)
  println(s"operations.single(15) = ${operations.single(15)}")
  val rangeResult = operations.range(1, 5)
  println(s"operations.range(1, 5) = ${rangeResult.mkString("[", ", ", "]")}")
  println()
  
  // 11. 代数的データ型のパターンマッチング
  println("11. 代数的データ型とパターンマッチング:")
  val types = List(FizzBuzzType.Type01, FizzBuzzType.Type02, FizzBuzzType.Type03, FizzBuzzType.NotDefined)
  types.foreach { tpe =>
    val result = tpe.generate(15)
    val description = tpe match {
      case FizzBuzzType.Type01 => "標準FizzBuzz"
      case FizzBuzzType.Type02 => "数字のみ"
      case FizzBuzzType.Type03 => "FizzBuzz限定"
      case FizzBuzzType.NotDefined => "未定義"
    }
    println(s"$description: $result")
  }
  
  println("\n=== デモンストレーション完了 ===")
}
