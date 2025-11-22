package com.example.accounting.application.model;

import java.util.ArrayList;
import java.util.List;

/**
 * 仕訳明細ドメインモデル
 */
public class JournalEntry {
    private Integer lineNumber;
    private String description;
    private List<JournalLine> lines;

    public JournalEntry() {
        this.lines = new ArrayList<>();
    }

    public JournalEntry(Integer lineNumber, String description) {
        this.lineNumber = lineNumber;
        this.description = description;
        this.lines = new ArrayList<>();
    }

    /**
     * 仕訳貸借明細を追加
     */
    public void addLine(JournalLine line) {
        this.lines.add(line);
    }

    public Integer getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(Integer lineNumber) {
        this.lineNumber = lineNumber;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public List<JournalLine> getLines() {
        return lines;
    }

    public void setLines(List<JournalLine> lines) {
        this.lines = lines;
    }
}
