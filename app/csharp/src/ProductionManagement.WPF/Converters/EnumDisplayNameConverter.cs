using System.ComponentModel.DataAnnotations;
using System.Globalization;
using System.Reflection;
using System.Windows.Data;

namespace ProductionManagement.WPF.Converters;

/// <summary>
/// Enum → 表示名変換
/// </summary>
public class EnumDisplayNameConverter : IValueConverter
{
    public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
    {
        if (value is Enum enumValue)
        {
            return GetDisplayName(enumValue);
        }

        return value?.ToString() ?? string.Empty;
    }

    public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
    {
        throw new NotSupportedException();
    }

    private static string GetDisplayName(Enum enumValue)
    {
        var field = enumValue.GetType().GetField(enumValue.ToString());
        if (field == null)
        {
            return enumValue.ToString();
        }

        var displayAttribute = field.GetCustomAttribute<DisplayAttribute>();
        return displayAttribute?.Name ?? enumValue.ToString();
    }
}
