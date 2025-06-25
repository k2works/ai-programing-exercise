import pytest

def test_greeting():
    assert greeting() == 'hello world'

def greeting():
    return 'hello world'

if __name__ == '__main__':
    pytest.main([__file__])
