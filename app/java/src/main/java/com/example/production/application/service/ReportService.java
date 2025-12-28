package com.example.production.application.service;

import com.example.production.domain.exception.ReportExportException;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.process.WorkOrder;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;

/**
 * 帳票出力サービス
 */
@Service
public class ReportService {

    /**
     * 在庫一覧を Excel 出力
     */
    public ByteArrayResource exportInventoryToExcel(List<Stock> stocks) {
        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("在庫一覧");

            // ヘッダースタイル
            CellStyle headerStyle = createHeaderStyle(workbook);

            // ヘッダー行
            Row headerRow = sheet.createRow(0);
            String[] headers = {"場所コード", "品目コード", "総在庫数", "合格品", "不良品", "未検査"};
            for (int i = 0; i < headers.length; i++) {
                Cell cell = headerRow.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            // データ行
            int rowNum = 1;
            for (Stock stock : stocks) {
                Row row = sheet.createRow(rowNum++);
                row.createCell(0).setCellValue(stock.getLocationCode());
                row.createCell(1).setCellValue(stock.getItemCode());
                row.createCell(2).setCellValue(
                        stock.getStockQuantity() != null ? stock.getStockQuantity().doubleValue() : 0);
                row.createCell(3).setCellValue(
                        stock.getPassedQuantity() != null ? stock.getPassedQuantity().doubleValue() : 0);
                row.createCell(4).setCellValue(
                        stock.getDefectiveQuantity() != null ? stock.getDefectiveQuantity().doubleValue() : 0);
                row.createCell(5).setCellValue(
                        stock.getUninspectedQuantity() != null ? stock.getUninspectedQuantity().doubleValue() : 0);
            }

            // 列幅自動調整
            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }

            return createResource(workbook);

        } catch (IOException e) {
            throw new ReportExportException("Excel 出力に失敗しました", e);
        }
    }

    /**
     * 品目一覧を Excel 出力
     */
    public ByteArrayResource exportItemsToExcel(List<Item> items) {
        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("品目一覧");

            // ヘッダースタイル
            CellStyle headerStyle = createHeaderStyle(workbook);

            // ヘッダー行
            Row headerRow = sheet.createRow(0);
            String[] headers = {"品目コード", "品名", "品目区分", "リードタイム", "安全在庫"};
            for (int i = 0; i < headers.length; i++) {
                Cell cell = headerRow.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            // データ行
            int rowNum = 1;
            for (Item item : items) {
                Row row = sheet.createRow(rowNum++);
                row.createCell(0).setCellValue(item.getItemCode());
                row.createCell(1).setCellValue(item.getItemName());
                row.createCell(2).setCellValue(
                        item.getItemCategory() != null ? item.getItemCategory().getDisplayName() : "");
                int leadTime = item.getLeadTime() != null ? item.getLeadTime() : 0;
                row.createCell(3).setCellValue(leadTime);
                row.createCell(4).setCellValue(
                        item.getSafetyStock() != null ? item.getSafetyStock().doubleValue() : 0);
            }

            // 列幅自動調整
            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }

            return createResource(workbook);

        } catch (IOException e) {
            throw new ReportExportException("Excel 出力に失敗しました", e);
        }
    }

    /**
     * オーダ一覧を Excel 出力
     */
    public ByteArrayResource exportOrdersToExcel(List<Order> orders) {
        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("オーダ一覧");

            // ヘッダースタイル
            CellStyle headerStyle = createHeaderStyle(workbook);

            // ヘッダー行
            Row headerRow = sheet.createRow(0);
            String[] headers = {"オーダ番号", "オーダタイプ", "品目コード", "計画数量", "納期", "ステータス"};
            for (int i = 0; i < headers.length; i++) {
                Cell cell = headerRow.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            // データ行
            int rowNum = 1;
            for (Order order : orders) {
                Row row = sheet.createRow(rowNum++);
                row.createCell(0).setCellValue(order.getOrderNumber());
                row.createCell(1).setCellValue(
                        order.getOrderType() != null ? order.getOrderType().getDisplayName() : "");
                row.createCell(2).setCellValue(order.getItemCode());
                row.createCell(3).setCellValue(
                        order.getPlanQuantity() != null ? order.getPlanQuantity().doubleValue() : 0);
                Cell dateCell = row.createCell(4);
                if (order.getDueDate() != null) {
                    dateCell.setCellValue(order.getDueDate().toString());
                }
                row.createCell(5).setCellValue(
                        order.getStatus() != null ? order.getStatus().getDisplayName() : "");
            }

            // 列幅自動調整
            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }

            return createResource(workbook);

        } catch (IOException e) {
            throw new ReportExportException("Excel 出力に失敗しました", e);
        }
    }

    /**
     * 作業指示一覧を Excel 出力
     */
    public ByteArrayResource exportWorkOrdersToExcel(List<WorkOrder> workOrders) {
        try (Workbook workbook = new XSSFWorkbook()) {
            Sheet sheet = workbook.createSheet("作業指示一覧");

            // ヘッダースタイル
            CellStyle headerStyle = createHeaderStyle(workbook);

            // ヘッダー行
            Row headerRow = sheet.createRow(0);
            String[] headers = {"作業指示番号", "オーダ番号", "品目コード", "指示数量",
                    "予定開始日", "予定終了日", "ステータス", "完了数"};
            for (int i = 0; i < headers.length; i++) {
                Cell cell = headerRow.createCell(i);
                cell.setCellValue(headers[i]);
                cell.setCellStyle(headerStyle);
            }

            // データ行
            int rowNum = 1;
            for (WorkOrder wo : workOrders) {
                Row row = sheet.createRow(rowNum++);
                row.createCell(0).setCellValue(wo.getWorkOrderNumber());
                row.createCell(1).setCellValue(wo.getOrderNumber());
                row.createCell(2).setCellValue(wo.getItemCode());
                row.createCell(3).setCellValue(
                        wo.getOrderQuantity() != null ? wo.getOrderQuantity().doubleValue() : 0);
                row.createCell(4).setCellValue(
                        wo.getPlannedStartDate() != null ? wo.getPlannedStartDate().toString() : "");
                row.createCell(5).setCellValue(
                        wo.getPlannedEndDate() != null ? wo.getPlannedEndDate().toString() : "");
                row.createCell(6).setCellValue(
                        wo.getStatus() != null ? wo.getStatus().getDisplayName() : "");
                row.createCell(7).setCellValue(
                        wo.getCompletedQuantity() != null ? wo.getCompletedQuantity().doubleValue() : 0);
            }

            // 列幅自動調整
            for (int i = 0; i < headers.length; i++) {
                sheet.autoSizeColumn(i);
            }

            return createResource(workbook);

        } catch (IOException e) {
            throw new ReportExportException("Excel 出力に失敗しました", e);
        }
    }

    /**
     * ヘッダースタイルを作成
     */
    private CellStyle createHeaderStyle(Workbook workbook) {
        CellStyle style = workbook.createCellStyle();
        style.setFillForegroundColor(IndexedColors.GREY_25_PERCENT.getIndex());
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND);
        style.setBorderBottom(BorderStyle.THIN);
        style.setBorderTop(BorderStyle.THIN);
        style.setBorderLeft(BorderStyle.THIN);
        style.setBorderRight(BorderStyle.THIN);

        Font font = workbook.createFont();
        font.setBold(true);
        style.setFont(font);

        return style;
    }

    /**
     * Workbook から ByteArrayResource を作成
     */
    private ByteArrayResource createResource(Workbook workbook) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        workbook.write(out);
        return new ByteArrayResource(out.toByteArray());
    }
}
