package com.example.sales.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.LocalDateTime;
import java.util.Map;

@Data
@EqualsAndHashCode(callSuper = true)
public class ValidationErrorResponse extends ErrorResponse {
    private Map<String, String> errors;

    public ValidationErrorResponse(int status, String message,
                                  Map<String, String> errors,
                                  LocalDateTime timestamp) {
        super(status, message, timestamp);
        this.errors = errors;
    }
}
