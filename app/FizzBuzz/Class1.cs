namespace FizzBuzz;

public class FizzBuzz
{
    private List<string> _list = new List<string>();

    public List<string> List
    {
        get { return _list; }
    }

    public string Generate(int number, int type = 1)
    {
        bool isFizz = number % 3 == 0;
        bool isBuzz = number % 5 == 0;

        switch (type)
        {
            case 1:
                if (isFizz && isBuzz)
                {
                    return "FizzBuzz";
                }
                if (isFizz)
                {
                    return "Fizz";
                }
                if (isBuzz)
                {
                    return "Buzz";
                }
                return number.ToString();
            case 2:
                return number.ToString();
            case 3:
                if (isFizz && isBuzz)
                {
                    return "FizzBuzz";
                }
                return number.ToString();
            default:
                throw new ArgumentException("無効なタイプです");
        }
    }

    public void GenerateList()
    {
        _list = new List<string>();
        for (int i = 1; i <= 100; i++)
        {
            _list.Add(Generate(i));
        }
    }
}
