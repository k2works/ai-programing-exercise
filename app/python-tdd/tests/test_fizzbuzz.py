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
                self.fizzbuzz = FizzBuzz
                
            class TestMultiplesOfThree:
                """Test multiples of three - 三の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz
                
                def test_3を渡したら文字列fizzを返す(self):
                    """Test that 3 returns 'Fizz'."""
                    assert self.fizzbuzz.generate(3) == 'Fizz'
                    
            class TestMultiplesOfFive:
                """Test multiples of five - 五の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz
                
                def test_5を渡したら文字列buzzを返す(self):
                    """Test that 5 returns 'Buzz'."""
                    assert self.fizzbuzz.generate(5) == 'Buzz'
                    
            class TestMultiplesOfThreeAndFive:
                """Test multiples of both three and five - 三と五の倍数の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz
                
                def test_15を渡したら文字列fizzbuzzを返す(self):
                    """Test that 15 returns 'FizzBuzz'."""
                    assert self.fizzbuzz.generate(15) == 'FizzBuzz'
                    
            class TestOtherNumbers:
                """Test other numbers - その他の場合."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz
                
                def test_1を渡したら文字列1を返す(self):
                    """Test that 1 returns '1'."""
                    assert self.fizzbuzz.generate(1) == '1'
                    
            class TestGenerateList:
                """Test list generation functionality - 1から100までのFizzBuzzの配列を返す."""
                
                def setup_method(self):
                    """Setup method for each test."""
                    self.fizzbuzz = FizzBuzz
                
                def test_配列の初めは文字列の1を返す(self):
                    """Test that generate_list returns correct first item."""
                    result = self.fizzbuzz.generate_list()
                    assert result[0] == '1'
                
                def test_配列の最後は文字列のbuzzを返す(self):
                    """Test that generate_list returns correct last item."""
                    result = self.fizzbuzz.generate_list()
                    assert result[-1] == 'Buzz'
                
                def test_配列の2番目は文字列のfizzを返す(self):
                    """Test that generate_list returns correct third item."""
                    result = self.fizzbuzz.generate_list()
                    assert result[2] == 'Fizz'
                
                def test_配列の4番目は文字列のbuzzを返す(self):
                    """Test that generate_list returns correct fifth item."""
                    result = self.fizzbuzz.generate_list()
                    assert result[4] == 'Buzz'
                
                def test_配列の14番目は文字列のfizzbuzzを返す(self):
                    """Test that generate_list returns correct fifteenth item."""
                    result = self.fizzbuzz.generate_list()
                    assert result[14] == 'FizzBuzz'
