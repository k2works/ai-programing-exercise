using System.Windows.Controls;
using Microsoft.Extensions.DependencyInjection;
using ProductionManagement.WPF.ViewModels.Reports;

namespace ProductionManagement.WPF.Views.Reports;

/// <summary>
/// 帳票出力画面のコードビハインド
/// </summary>
public partial class ReportListView : UserControl
{
    public ReportListView()
    {
        InitializeComponent();
        DataContext = App.Services.GetRequiredService<ReportListViewModel>();
    }
}
