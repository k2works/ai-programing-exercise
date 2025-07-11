"""Tests for FizzBuzz class."""
from src.fizzbuzz import FizzBuzz


class TestFizzBuzz:
    """Test cases for FizzBuzz class."""
    
    def setup_method(self):
        """Setup method for each test."""
        self.fizzbuzz = FizzBuzz

    class TestNumberToString:
        """Test number to string conversion - タイプごとに出力を切り替えることができる."""
        
        class TestType1:
            """Test type 1 case - タイプ1の場合."""
            
            def setup_method(self):
                """Setup method for each test."""
                self.fizzbuzz = FizzBuzz(1)
                
            class TestMultiplesOfThree:
                """Test multiples of three - 三の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(1)
                
                def test_3を渡したら文字列fizzを返す(self):
                    """Test that 3 returns 'Fizz'."""
                    assert self.fizzbuzz.generate(3).value == 'Fizz'
                    
            class TestMultiplesOfFive:
                """Test multiples of five - 五の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(1)
                
                def test_5を渡したら文字列buzzを返す(self):
                    """Test that 5 returns 'Buzz'."""
                    assert self.fizzbuzz.generate(5).value == 'Buzz'
                    
            class TestMultiplesOfThreeAndFive:
                """Test multiples of both three and five - 三と五の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(1)
                
                def test_15を渡したら文字列fizzbuzzを返す(self):
                    """Test that 15 returns 'FizzBuzz'."""
                    assert self.fizzbuzz.generate(15).value == 'FizzBuzz'
                    
            class TestOtherNumbers:
                """Test other numbers - その他の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(1)
                
                def test_1を渡したら文字列1を返す(self):
                    """Test that 1 returns '1'."""
                    assert self.fizzbuzz.generate(1).value == '1'
                    
            class TestGenerateList:
                """Test list generation functionality - 1から100までのFizzBuzzの配列を返す."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    fizzbuzz = FizzBuzz(1)
                    fizzbuzz.generate_list()
                    self.result = fizzbuzz.list
                
                def test_配列の初めは文字列の1を返す(self):
                    """Test that generate_list returns correct first item."""
                    assert self.result[0].value == '1'
                
                def test_配列の最後は文字列のbuzzを返す(self):
                    """Test that generate_list returns correct last item."""
                    assert self.result[-1].value == 'Buzz'
                
                def test_配列の2番目は文字列のfizzを返す(self):
                    """Test that generate_list returns correct third item."""
                    assert self.result[2].value == 'Fizz'
                
                def test_配列の4番目は文字列のbuzzを返す(self):
                    """Test that generate_list returns correct fifth item."""
                    assert self.result[4].value == 'Buzz'
                
                def test_配列の14番目は文字列のfizzbuzzを返す(self):
                    """Test that generate_list returns correct fifteenth item."""
                    assert self.result[14].value == 'FizzBuzz'
                    
        class TestType2:
            """Test type 2 case - タイプ2の場合."""
            
            def setup_method(self):
                """Setup method for each test."""
                self.fizzbuzz = FizzBuzz(2)
                
            class TestMultiplesOfThree:
                """Test multiples of three - 三の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(2)
                
                def test_3を渡したら文字列3を返す(self):
                    """Test that 3 returns '3' for type 2."""
                    assert self.fizzbuzz.generate(3).value == '3'
                    
            class TestMultiplesOfFive:
                """Test multiples of five - 五の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(2)
                
                def test_5を渡したら文字列5を返す(self):
                    """Test that 5 returns '5' for type 2."""
                    assert self.fizzbuzz.generate(5).value == '5'
                    
            class TestMultiplesOfThreeAndFive:
                """Test multiples of both three and five - 三と五の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(2)
                
                def test_15を渡したら文字列15を返す(self):
                    """Test that 15 returns '15' for type 2."""
                    assert self.fizzbuzz.generate(15).value == '15'
                
            class TestOtherNumbers:
                """Test other numbers - その他の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(2)
                
                def test_1を渡したら文字列1を返す(self):
                    """Test that 1 returns '1' for type 2."""
                    assert self.fizzbuzz.generate(1).value == '1'
                    
        class TestType3:
            """Test type 3 case - タイプ3の場合."""
            
            def setup_method(self):
                """Setup method for each test."""
                self.fizzbuzz = FizzBuzz(3)
                
            class TestMultiplesOfThree:
                """Test multiples of three - 三の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(3)
                
                def test_3を渡したら文字列3を返す(self):
                    """Test that 3 returns '3' for type 3."""
                    assert self.fizzbuzz.generate(3).value == '3'
                    
            class TestMultiplesOfFive:
                """Test multiples of five - 五の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(3)
                
                def test_5を渡したら文字列5を返す(self):
                    """Test that 5 returns '5' for type 3."""
                    assert self.fizzbuzz.generate(5).value == '5'
                    
            class TestMultiplesOfThreeAndFive:
                """Test multiples of both three and five - 三と五の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(3)
                
                def test_15を渡したら文字列fizzbuzzを返す(self):
                    """Test that 15 returns 'FizzBuzz' for type 3."""
                    assert self.fizzbuzz.generate(15).value == 'FizzBuzz'
                
            class TestOtherNumbers:
                """Test other numbers - その他の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz(3)
                
                def test_1を渡したら文字列1を返す(self):
                    """Test that 1 returns '1' for type 3."""
                    assert self.fizzbuzz.generate(1).value == '1'
        
    class TestInvalidType:
        """Test invalid type case - それ以外のタイプの場合."""
        
        def setup_method(self):
            """Setup method for each test."""
            pass
        
        def test_例外を返す(self):
            """Test that invalid type raises RuntimeError."""
            import pytest
            with pytest.raises(RuntimeError, match='該当するタイプは存在しません'):
                FizzBuzz(4)
    
    class TestFizzBuzzValue:
        """Test FizzBuzzValue value object - 値オブジェクト学習用テスト."""
        
        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzz(1)
        
        def test_同じ値である(self):
            """Test that same values are equal."""
            value1 = self.fizzbuzz.generate(1)
            value2 = self.fizzbuzz.generate(1)
            
            assert value1 == value2
        
        def test_to_stringメソッド(self):
            """Test string representation."""
            value = self.fizzbuzz.generate(3)
            
            assert str(value) == "3:Fizz"
    
    class TestFizzBuzzList:
        """Test FizzBuzzList first-class collection - ファーストクラスコレクション学習用テスト."""
        
        def setup_method(self):
            """Setup method for each test."""
            self.fizzbuzz = FizzBuzz(1)
            self.fizzbuzz.generate_list()
            self.fizzbuzz_list = self.fizzbuzz.list
        
        def test_不変オブジェクトである(self):
            """Test that FizzBuzzList is immutable."""
            from src.fizzbuzz import FizzBuzzValue
            
            # 元のvalueプロパティを取得
            original_values = self.fizzbuzz_list.value
            original_length = len(original_values)
            
            # valueプロパティから取得したリストを変更
            original_values.append(FizzBuzzValue(101, "101"))
            
            # 元のFizzBuzzListは変更されていないことを確認
            assert len(self.fizzbuzz_list) == original_length
            assert len(self.fizzbuzz_list.value) == original_length
        
        def test_addメソッド(self):
            """Test add method returns new FizzBuzzList."""
            from src.fizzbuzz import FizzBuzzValue
            
            # 新しい値を追加
            new_values = [FizzBuzzValue(101, "101"), FizzBuzzValue(102, "Fizz")]
            new_list = self.fizzbuzz_list.add(new_values)
            
            # 元のリストは変更されていない
            assert len(self.fizzbuzz_list) == 100
            
            # 新しいリストには追加された値が含まれている
            assert len(new_list) == 102
            assert new_list[100].value == "101"
            assert new_list[101].value == "Fizz"
        
        def test_インデクサアクセス(self):
            """Test index access."""
            # インデクサでアクセスできることを確認
            assert self.fizzbuzz_list[0].value == "1"
            assert self.fizzbuzz_list[2].value == "Fizz"
            assert self.fizzbuzz_list[4].value == "Buzz"
            assert self.fizzbuzz_list[14].value == "FizzBuzz"
        
        def test_イテレーション(self):
            """Test iteration."""
            # イテレーションできることを確認
            count = 0
            for value in self.fizzbuzz_list:
                count += 1
            
            assert count == 100
    
    class TestFizzBuzzCommand:
        """Test FizzBuzzCommand pattern implementation - Commandパターン学習用テスト."""
        
        def setup_method(self):
            """Setup method for each test."""
            from src.fizzbuzz import FizzBuzzType01, FizzBuzzType02, FizzBuzzValueCommand, FizzBuzzListCommand
            self.type01 = FizzBuzzType01()
            self.type02 = FizzBuzzType02()
            self.value_command = FizzBuzzValueCommand(self.type01)
            self.list_command = FizzBuzzListCommand(self.type01)
        
        def test_value_commandの実行(self):
            """Test FizzBuzzValueCommand execution."""
            # Type1でのテスト
            assert self.value_command.execute(1) == '1'
            assert self.value_command.execute(3) == 'Fizz'
            assert self.value_command.execute(5) == 'Buzz'
            assert self.value_command.execute(15) == 'FizzBuzz'
        
        def test_list_commandの実行(self):
            """Test FizzBuzzListCommand execution."""
            result = self.list_command.execute()
            
            # FizzBuzzListが返されることを確認
            from src.fizzbuzz import FizzBuzzList
            assert isinstance(result, FizzBuzzList)
            
            # リストの長さが100であることを確認
            assert len(result) == 100
            
            # 特定の位置の値を確認
            assert result[0].value == '1'
            assert result[2].value == 'Fizz'
            assert result[4].value == 'Buzz'
            assert result[14].value == 'FizzBuzz'
        
        def test_異なるタイプでのcommand実行(self):
            """Test command execution with different types."""
            from src.fizzbuzz import FizzBuzzValueCommand
            
            # Type2でのValueCommand
            value_command_type2 = FizzBuzzValueCommand(self.type02)
            assert value_command_type2.execute(3) == '3'  # Type2は数字のみ
            assert value_command_type2.execute(5) == '5'
            assert value_command_type2.execute(15) == '15'
