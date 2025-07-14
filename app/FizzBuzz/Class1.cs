using System;
using System.Collections.Generic;

namespace FizzBuzz;

public class FizzBuzz
{
	private List<string> _list = new List<string>();
	private int _type;

	public FizzBuzz(int type)
	{
		_type = type;
	}

	public List<string> List
	{
		get { return _list; }
	}

	public string Generate(int number)
	{
		bool isFizz = number % 3 == 0;
		bool iBuzz = number % 5 == 0;

		switch (_type)
		{
			case 1:
				if (isFizz && iBuzz)
				{
					return "FizzBuzz";
				}
				if (isFizz)
				{
					return "Fizz";
				}
				if (iBuzz)
				{
					return "Buzz";
				}
				return number.ToString();
			case 2:
				return number.ToString();
			case 3:
				if (isFizz && iBuzz)
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
