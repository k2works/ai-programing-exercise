using System.Globalization;
using System.Windows;
using System.Windows.Data;

namespace ProductionManagement.WPF.Converters;

/// <summary>
/// 複数の bool 値がすべて true の場合に true を返す MultiValueConverter
/// </summary>
public class AllTrueConverter : IMultiValueConverter
{
    public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
    {
        foreach (var value in values)
        {
            if (value == DependencyProperty.UnsetValue)
            {
                return false;
            }

            if (value is bool boolValue && !boolValue)
            {
                return false;
            }

            if (value is Visibility visibility && visibility != Visibility.Visible)
            {
                return false;
            }

            if (value == null)
            {
                return false;
            }
        }

        return true;
    }

    public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
    {
        throw new NotSupportedException();
    }
}
