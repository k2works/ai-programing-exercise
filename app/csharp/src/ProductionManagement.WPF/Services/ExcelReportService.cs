using System.Globalization;
using ClosedXML.Excel;
using ProductionManagement.Domain.Models.Inventory;
using ProductionManagement.Domain.Models.Process;
using ProductionManagement.Domain.Models.Purchase;
using QuestPDF.Fluent;
using QuestPDF.Helpers;
using QuestPDF.Infrastructure;

namespace ProductionManagement.WPF.Services;

/// <summary>
/// 帳票出力サービス実装（Excel: ClosedXML, PDF: QuestPDF）
/// </summary>
public class ExcelReportService : IReportService
{
    static ExcelReportService()
    {
        // QuestPDF ライセンス設定（Community License）
        QuestPDF.Settings.License = LicenseType.Community;
    }

    /// <summary>
    /// 在庫一覧を Excel 出力
    /// </summary>
    public Task ExportStockListToExcelAsync(IEnumerable<Stock> stocks, string filePath)
    {
        using var workbook = new XLWorkbook();
        var worksheet = workbook.Worksheets.Add("在庫一覧");

        // ヘッダー行
        var headers = new[] { "拠点コード", "品目コード", "在庫数量", "良品数量", "不良品数量", "未検査数量", "更新日時" };
        for (var i = 0; i < headers.Length; i++)
        {
            var cell = worksheet.Cell(1, i + 1);
            cell.Value = headers[i];
            cell.Style.Font.Bold = true;
            cell.Style.Fill.BackgroundColor = XLColor.LightGray;
            cell.Style.Border.OutsideBorder = XLBorderStyleValues.Thin;
        }

        // データ行
        var row = 2;
        foreach (var stock in stocks)
        {
            worksheet.Cell(row, 1).Value = stock.LocationCode;
            worksheet.Cell(row, 2).Value = stock.ItemCode;
            worksheet.Cell(row, 3).Value = (double)stock.StockQuantity;
            worksheet.Cell(row, 4).Value = (double)stock.PassedQuantity;
            worksheet.Cell(row, 5).Value = (double)stock.DefectiveQuantity;
            worksheet.Cell(row, 6).Value = (double)stock.UninspectedQuantity;
            worksheet.Cell(row, 7).Value = stock.UpdatedAt.ToString("yyyy/MM/dd HH:mm");

            // 数量列は右寄せ
            for (var col = 3; col <= 6; col++)
            {
                worksheet.Cell(row, col).Style.Alignment.Horizontal = XLAlignmentHorizontalValues.Right;
            }

            row++;
        }

        // 列幅自動調整
        worksheet.Columns().AdjustToContents();

        // 保存
        workbook.SaveAs(filePath);
        return Task.CompletedTask;
    }

    /// <summary>
    /// 発注書を Excel 出力
    /// </summary>
    public Task ExportPurchaseOrderToExcelAsync(PurchaseOrder order, IEnumerable<PurchaseOrderDetail> details, string filePath)
    {
        using var workbook = new XLWorkbook();
        var worksheet = workbook.Worksheets.Add("発注書");

        // タイトル
        var titleCell = worksheet.Cell(1, 1);
        titleCell.Value = "発 注 書";
        titleCell.Style.Font.Bold = true;
        titleCell.Style.Font.FontSize = 18;
        worksheet.Range(1, 1, 1, 5).Merge();

        // ヘッダー情報
        worksheet.Cell(3, 1).Value = "発注番号:";
        worksheet.Cell(3, 2).Value = order.PurchaseOrderNumber;
        worksheet.Cell(4, 1).Value = "取引先コード:";
        worksheet.Cell(4, 2).Value = order.SupplierCode;
        worksheet.Cell(5, 1).Value = "発注日:";
        worksheet.Cell(5, 2).Value = order.OrderDate.ToString("yyyy/MM/dd");
        worksheet.Cell(6, 1).Value = "ステータス:";
        worksheet.Cell(6, 2).Value = order.Status.GetDisplayName();

        // 明細ヘッダー
        var detailHeaders = new[] { "行番号", "品目コード", "数量", "単価", "金額" };
        for (var i = 0; i < detailHeaders.Length; i++)
        {
            var cell = worksheet.Cell(8, i + 1);
            cell.Value = detailHeaders[i];
            cell.Style.Font.Bold = true;
            cell.Style.Fill.BackgroundColor = XLColor.LightGray;
            cell.Style.Border.OutsideBorder = XLBorderStyleValues.Thin;
        }

        // 明細データ
        var row = 9;
        decimal totalAmount = 0;
        foreach (var detail in details)
        {
            var amount = detail.OrderAmount;
            totalAmount += amount;

            worksheet.Cell(row, 1).Value = detail.LineNumber;
            worksheet.Cell(row, 2).Value = detail.ItemCode;
            worksheet.Cell(row, 3).Value = (double)detail.OrderQuantity;
            worksheet.Cell(row, 4).Value = (double)detail.OrderUnitPrice;
            worksheet.Cell(row, 5).Value = (double)amount;

            // 数量列は右寄せ、金額はカンマ区切り
            worksheet.Cell(row, 3).Style.Alignment.Horizontal = XLAlignmentHorizontalValues.Right;
            worksheet.Cell(row, 4).Style.NumberFormat.Format = "#,##0";
            worksheet.Cell(row, 5).Style.NumberFormat.Format = "#,##0";

            row++;
        }

        // 合計行
        worksheet.Cell(row, 4).Value = "合計:";
        worksheet.Cell(row, 4).Style.Font.Bold = true;
        worksheet.Cell(row, 5).Value = (double)totalAmount;
        worksheet.Cell(row, 5).Style.NumberFormat.Format = "#,##0";
        worksheet.Cell(row, 5).Style.Font.Bold = true;

        // 列幅自動調整
        worksheet.Columns().AdjustToContents();

        workbook.SaveAs(filePath);
        return Task.CompletedTask;
    }

    /// <summary>
    /// 作業指示書を Excel 出力
    /// </summary>
    public Task ExportWorkOrderToExcelAsync(WorkOrder workOrder, string filePath)
    {
        using var workbook = new XLWorkbook();
        var worksheet = workbook.Worksheets.Add("作業指示書");

        // タイトル
        var titleCell = worksheet.Cell(1, 1);
        titleCell.Value = "作 業 指 示 書";
        titleCell.Style.Font.Bold = true;
        titleCell.Style.Font.FontSize = 18;
        worksheet.Range(1, 1, 1, 4).Merge();

        // 指示情報
        worksheet.Cell(3, 1).Value = "作業指示番号:";
        worksheet.Cell(3, 2).Value = workOrder.WorkOrderNumber;
        worksheet.Cell(4, 1).Value = "品目コード:";
        worksheet.Cell(4, 2).Value = workOrder.ItemCode;
        worksheet.Cell(5, 1).Value = "拠点コード:";
        worksheet.Cell(5, 2).Value = workOrder.LocationCode;
        worksheet.Cell(6, 1).Value = "計画数量:";
        worksheet.Cell(6, 2).Value = (double)workOrder.OrderQuantity;
        worksheet.Cell(7, 1).Value = "着手予定日:";
        worksheet.Cell(7, 2).Value = workOrder.PlannedStartDate.ToString("yyyy/MM/dd");
        worksheet.Cell(8, 1).Value = "完了予定日:";
        worksheet.Cell(8, 2).Value = workOrder.PlannedEndDate.ToString("yyyy/MM/dd");
        worksheet.Cell(9, 1).Value = "ステータス:";
        worksheet.Cell(9, 2).Value = workOrder.Status.GetDisplayName();

        // 実績情報（ヘッダー）
        worksheet.Cell(11, 1).Value = "【実績情報】";
        worksheet.Cell(11, 1).Style.Font.Bold = true;

        worksheet.Cell(12, 1).Value = "完了数量:";
        worksheet.Cell(12, 2).Value = (double)workOrder.CompletedQuantity;
        worksheet.Cell(13, 1).Value = "着手日:";
        worksheet.Cell(13, 2).Value = workOrder.ActualStartDate?.ToString("yyyy/MM/dd") ?? "-";
        worksheet.Cell(14, 1).Value = "完了日:";
        worksheet.Cell(14, 2).Value = workOrder.ActualEndDate?.ToString("yyyy/MM/dd") ?? "-";

        // 備考欄
        worksheet.Cell(16, 1).Value = "【備考】";
        worksheet.Cell(16, 1).Style.Font.Bold = true;
        worksheet.Range(17, 1, 20, 4).Merge();
        worksheet.Range(17, 1, 20, 4).Style.Border.OutsideBorder = XLBorderStyleValues.Thin;

        // 列幅自動調整
        worksheet.Columns().AdjustToContents();

        workbook.SaveAs(filePath);
        return Task.CompletedTask;
    }

    // PDF 出力メソッド

    /// <summary>
    /// 在庫一覧を PDF 出力
    /// </summary>
    public Task ExportStockListToPdfAsync(IEnumerable<Stock> stocks, string filePath)
    {
        var stockList = stocks.ToList();

        Document.Create(container =>
        {
            container.Page(page =>
            {
                page.Size(PageSizes.A4.Landscape());
                page.Margin(30);
                page.DefaultTextStyle(x => x.FontSize(10));

                page.Header().Element(c => ComposeHeader(c, "在庫一覧"));

                page.Content().Table(table =>
                {
                    // 列定義
                    table.ColumnsDefinition(columns =>
                    {
                        columns.RelativeColumn(1);   // 拠点コード
                        columns.RelativeColumn(1.5f); // 品目コード
                        columns.RelativeColumn(1);   // 在庫数量
                        columns.RelativeColumn(1);   // 良品数量
                        columns.RelativeColumn(1);   // 不良品数量
                        columns.RelativeColumn(1);   // 未検査数量
                        columns.RelativeColumn(1.5f); // 更新日時
                    });

                    // ヘッダー
                    table.Header(header =>
                    {
                        header.Cell().Element(CellStyle).Text("拠点コード").Bold();
                        header.Cell().Element(CellStyle).Text("品目コード").Bold();
                        header.Cell().Element(CellStyle).AlignRight().Text("在庫数量").Bold();
                        header.Cell().Element(CellStyle).AlignRight().Text("良品数量").Bold();
                        header.Cell().Element(CellStyle).AlignRight().Text("不良品数量").Bold();
                        header.Cell().Element(CellStyle).AlignRight().Text("未検査数量").Bold();
                        header.Cell().Element(CellStyle).Text("更新日時").Bold();
                    });

                    // データ行
                    foreach (var stock in stockList)
                    {
                        table.Cell().Element(CellStyle).Text(stock.LocationCode);
                        table.Cell().Element(CellStyle).Text(stock.ItemCode);
                        table.Cell().Element(CellStyle).AlignRight().Text(stock.StockQuantity.ToString("N0", CultureInfo.InvariantCulture));
                        table.Cell().Element(CellStyle).AlignRight().Text(stock.PassedQuantity.ToString("N0", CultureInfo.InvariantCulture));
                        table.Cell().Element(CellStyle).AlignRight().Text(stock.DefectiveQuantity.ToString("N0", CultureInfo.InvariantCulture));
                        table.Cell().Element(CellStyle).AlignRight().Text(stock.UninspectedQuantity.ToString("N0", CultureInfo.InvariantCulture));
                        table.Cell().Element(CellStyle).Text(stock.UpdatedAt.ToString("yyyy/MM/dd HH:mm", CultureInfo.InvariantCulture));
                    }
                });

                page.Footer().Element(ComposeFooter);
            });
        }).GeneratePdf(filePath);

        return Task.CompletedTask;
    }

    /// <summary>
    /// 発注書を PDF 出力
    /// </summary>
    public Task ExportPurchaseOrderToPdfAsync(PurchaseOrder order, IEnumerable<PurchaseOrderDetail> details, string filePath)
    {
        var detailList = details.ToList();
        var totalAmount = detailList.Sum(d => d.OrderAmount);

        Document.Create(container =>
        {
            container.Page(page =>
            {
                page.Size(PageSizes.A4);
                page.Margin(30);
                page.DefaultTextStyle(x => x.FontSize(10));

                page.Header().Element(c => ComposeHeader(c, "発 注 書"));

                page.Content().Column(column =>
                {
                    column.Spacing(10);

                    // ヘッダー情報
                    column.Item().Row(row =>
                    {
                        row.RelativeItem().Column(c =>
                        {
                            c.Item().Text($"発注番号: {order.PurchaseOrderNumber}");
                            c.Item().Text($"取引先コード: {order.SupplierCode}");
                        });
                        row.RelativeItem().Column(c =>
                        {
                            c.Item().Text($"発注日: {order.OrderDate.ToString("yyyy/MM/dd", CultureInfo.InvariantCulture)}");
                            c.Item().Text($"ステータス: {order.Status.GetDisplayName()}");
                        });
                    });

                    column.Item().PaddingTop(10);

                    // 明細テーブル
                    column.Item().Table(table =>
                    {
                        table.ColumnsDefinition(columns =>
                        {
                            columns.RelativeColumn(0.5f); // 行番号
                            columns.RelativeColumn(2);    // 品目コード
                            columns.RelativeColumn(1);    // 数量
                            columns.RelativeColumn(1);    // 単価
                            columns.RelativeColumn(1);    // 金額
                        });

                        table.Header(header =>
                        {
                            header.Cell().Element(CellStyle).Text("行番号").Bold();
                            header.Cell().Element(CellStyle).Text("品目コード").Bold();
                            header.Cell().Element(CellStyle).AlignRight().Text("数量").Bold();
                            header.Cell().Element(CellStyle).AlignRight().Text("単価").Bold();
                            header.Cell().Element(CellStyle).AlignRight().Text("金額").Bold();
                        });

                        foreach (var detail in detailList)
                        {
                            table.Cell().Element(CellStyle).Text(detail.LineNumber.ToString(CultureInfo.InvariantCulture));
                            table.Cell().Element(CellStyle).Text(detail.ItemCode);
                            table.Cell().Element(CellStyle).AlignRight().Text(detail.OrderQuantity.ToString("N0", CultureInfo.InvariantCulture));
                            table.Cell().Element(CellStyle).AlignRight().Text(detail.OrderUnitPrice.ToString("N0", CultureInfo.InvariantCulture));
                            table.Cell().Element(CellStyle).AlignRight().Text(detail.OrderAmount.ToString("N0", CultureInfo.InvariantCulture));
                        }

                        // 合計行
                        table.Cell().ColumnSpan(4).Element(CellStyle).AlignRight().Text("合計:").Bold();
                        table.Cell().Element(CellStyle).AlignRight().Text(totalAmount.ToString("N0", CultureInfo.InvariantCulture)).Bold();
                    });
                });

                page.Footer().Element(ComposeFooter);
            });
        }).GeneratePdf(filePath);

        return Task.CompletedTask;
    }

    /// <summary>
    /// 作業指示書を PDF 出力
    /// </summary>
    public Task ExportWorkOrderToPdfAsync(WorkOrder workOrder, string filePath)
    {
        Document.Create(container =>
        {
            container.Page(page =>
            {
                page.Size(PageSizes.A4);
                page.Margin(30);
                page.DefaultTextStyle(x => x.FontSize(10));

                page.Header().Element(c => ComposeHeader(c, "作 業 指 示 書"));

                page.Content().Column(column =>
                {
                    column.Spacing(5);

                    // 基本情報
                    column.Item().Text("【基本情報】").Bold();
                    column.Item().Table(table =>
                    {
                        table.ColumnsDefinition(columns =>
                        {
                            columns.RelativeColumn(1);
                            columns.RelativeColumn(2);
                        });

                        AddInfoRow(table, "作業指示番号", workOrder.WorkOrderNumber);
                        AddInfoRow(table, "品目コード", workOrder.ItemCode);
                        AddInfoRow(table, "拠点コード", workOrder.LocationCode);
                        AddInfoRow(table, "計画数量", workOrder.OrderQuantity.ToString("N0", CultureInfo.InvariantCulture));
                        AddInfoRow(table, "着手予定日", workOrder.PlannedStartDate.ToString("yyyy/MM/dd", CultureInfo.InvariantCulture));
                        AddInfoRow(table, "完了予定日", workOrder.PlannedEndDate.ToString("yyyy/MM/dd", CultureInfo.InvariantCulture));
                        AddInfoRow(table, "ステータス", workOrder.Status.GetDisplayName());
                    });

                    column.Item().PaddingTop(15);

                    // 実績情報
                    column.Item().Text("【実績情報】").Bold();
                    column.Item().Table(table =>
                    {
                        table.ColumnsDefinition(columns =>
                        {
                            columns.RelativeColumn(1);
                            columns.RelativeColumn(2);
                        });

                        AddInfoRow(table, "完了数量", workOrder.CompletedQuantity.ToString("N0", CultureInfo.InvariantCulture));
                        AddInfoRow(table, "着手日", workOrder.ActualStartDate?.ToString("yyyy/MM/dd", CultureInfo.InvariantCulture) ?? "-");
                        AddInfoRow(table, "完了日", workOrder.ActualEndDate?.ToString("yyyy/MM/dd", CultureInfo.InvariantCulture) ?? "-");
                    });

                    column.Item().PaddingTop(15);

                    // 備考欄
                    column.Item().Text("【備考】").Bold();
                    column.Item().Border(1).Padding(10).MinHeight(100).Text(workOrder.Remarks ?? string.Empty);
                });

                page.Footer().Element(ComposeFooter);
            });
        }).GeneratePdf(filePath);

        return Task.CompletedTask;
    }

    // ヘルパーメソッド

    private static void ComposeHeader(IContainer container, string title)
    {
        container.Column(column =>
        {
            column.Item().AlignCenter().Text(title).FontSize(18).Bold();
            column.Item().PaddingTop(5).LineHorizontal(1);
            column.Item().PaddingBottom(10);
        });
    }

    private static void ComposeFooter(IContainer container)
    {
        container.AlignCenter().Text(text =>
        {
            text.Span("出力日時: ");
            text.Span(DateTime.Now.ToString("yyyy/MM/dd HH:mm", CultureInfo.InvariantCulture));
        });
    }

    private static IContainer CellStyle(IContainer container)
    {
        return container.Border(0.5f).BorderColor(Colors.Grey.Medium).Padding(5);
    }

    private static void AddInfoRow(TableDescriptor table, string label, string value)
    {
        table.Cell().Element(CellStyle).Text(label);
        table.Cell().Element(CellStyle).Text(value);
    }
}
