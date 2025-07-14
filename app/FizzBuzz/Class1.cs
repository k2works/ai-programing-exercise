using System;
using System.Collections.Generic;

namespace FizzBuzz;

public class FizzBuzz
{
	private List<string> _list = new List<string>();
	private FizzBuzzType _type;

	public FizzBuzz(int type)
	{
		_type = Create(type);
	}

	public List<string> List
	{
		get { return _list; }
	}

	public static FizzBuzzType Create(int type)
	{
		switch (type)
		{
			case 1:
				return new FizzBuzzType01();
			case 2:
				return new FizzBuzzType02();
			case 3:
				return new FizzBuzzType03();
			default:
				return new NullFizzBuzzType();
		}
	}

	public string Generate(int number)
	{
		return _type.Generate(new Number(number)).ToString();
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
