using System.Globalization;
using System.Windows;
using System.Windows.Data;

namespace ProductionManagement.WPF.Converters;

/// <summary>
/// bool → Visibility 変換
/// </summary>
public class BoolToVisibilityConverter : IValueConverter
{
    /// <summary>
    /// 変換を反転するかどうか
    /// </summary>
    public bool Invert { get; set; }

    public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
    {
        if (value is bool boolValue)
        {
            if (Invert)
            {
                boolValue = !boolValue;
            }

            return boolValue ? Visibility.Visible : Visibility.Collapsed;
        }

        return Visibility.Collapsed;
    }

    public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
    {
        var result = value is Visibility visibility && visibility == Visibility.Visible;
        return Invert ? !result : result;
    }
}
