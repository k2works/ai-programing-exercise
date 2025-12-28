package com.example.production.infrastructure.graphql.validation;

import graphql.GraphQLError;
import graphql.GraphqlErrorBuilder;
import graphql.schema.DataFetchingEnvironment;
import org.springframework.graphql.data.method.annotation.GraphQlExceptionHandler;
import org.springframework.graphql.execution.ErrorType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.MethodArgumentNotValidException;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import java.util.List;
import java.util.stream.Collectors;

/**
 * バリデーション例外ハンドラ
 * Jakarta Validation の例外を GraphQL エラーに変換する
 */
@Controller
public class ValidationExceptionHandler {

    /**
     * ConstraintViolationException をハンドリング
     */
    @GraphQlExceptionHandler
    public List<GraphQLError> handleConstraintViolation(
            ConstraintViolationException ex,
            DataFetchingEnvironment env) {

        return ex.getConstraintViolations().stream()
            .map(violation -> toGraphQLError(violation, env))
            .collect(Collectors.toList());
    }

    /**
     * MethodArgumentNotValidException をハンドリング
     */
    @GraphQlExceptionHandler
    public List<GraphQLError> handleMethodArgumentNotValid(
            MethodArgumentNotValidException ex,
            DataFetchingEnvironment env) {

        return ex.getBindingResult().getFieldErrors().stream()
            .map(fieldError -> GraphqlErrorBuilder.newError(env)
                .errorType(ErrorType.BAD_REQUEST)
                .message(String.format("%s: %s",
                    fieldError.getField(),
                    fieldError.getDefaultMessage()))
                .build())
            .collect(Collectors.toList());
    }

    /**
     * ConstraintViolation を GraphQLError に変換
     */
    private GraphQLError toGraphQLError(
            ConstraintViolation<?> violation,
            DataFetchingEnvironment env) {
        return GraphqlErrorBuilder.newError(env)
            .errorType(ErrorType.BAD_REQUEST)
            .message(String.format("%s: %s",
                violation.getPropertyPath(),
                violation.getMessage()))
            .build();
    }
}
