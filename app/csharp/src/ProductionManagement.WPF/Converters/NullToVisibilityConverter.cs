using System.Globalization;
using System.Windows;
using System.Windows.Data;

namespace ProductionManagement.WPF.Converters;

/// <summary>
/// null → Visibility 変換
/// </summary>
public class NullToVisibilityConverter : IValueConverter
{
    public bool Invert { get; set; }

    public object Convert(object? value, Type targetType, object parameter, CultureInfo culture)
    {
        var isNull = value == null;
        if (Invert)
        {
            isNull = !isNull;
        }

        return isNull ? Visibility.Collapsed : Visibility.Visible;
    }

    public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
    {
        throw new NotSupportedException();
    }
}
