package com.example.production.infrastructure.grpc.interceptor;

import io.grpc.*;
import net.devh.boot.grpc.server.interceptor.GrpcGlobalServerInterceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;
import java.time.Instant;

/**
 * ロギング & エラーハンドリングインターセプター
 * gRPC リクエストのロギングとパフォーマンス計測を行う
 */
@GrpcGlobalServerInterceptor
public class LoggingInterceptor implements ServerInterceptor {

    private static final Logger log = LoggerFactory.getLogger(LoggingInterceptor.class);

    @Override
    public <ReqT, RespT> ServerCall.Listener<ReqT> interceptCall(
            ServerCall<ReqT, RespT> call,
            Metadata headers,
            ServerCallHandler<ReqT, RespT> next) {

        String methodName = call.getMethodDescriptor().getFullMethodName();
        Instant startTime = Instant.now();

        log.info("gRPC Request: {}", methodName);

        ServerCall<ReqT, RespT> wrappedCall = new ForwardingServerCall
                .SimpleForwardingServerCall<>(call) {
            @Override
            public void close(Status status, Metadata trailers) {
                Duration duration = Duration.between(startTime, Instant.now());
                if (status.isOk()) {
                    log.info("gRPC Response: {} - Status: {} - Duration: {}ms",
                        methodName, status.getCode(), duration.toMillis());
                } else {
                    log.warn("gRPC Response: {} - Status: {} - Duration: {}ms - Description: {}",
                        methodName, status.getCode(), duration.toMillis(), status.getDescription());
                }
                super.close(status, trailers);
            }
        };

        return new ForwardingServerCallListener.SimpleForwardingServerCallListener<>(
                next.startCall(wrappedCall, headers)) {
            @Override
            public void onMessage(ReqT message) {
                log.debug("gRPC Request Body: {} - {}", methodName, message);
                super.onMessage(message);
            }

            @Override
            public void onHalfClose() {
                try {
                    super.onHalfClose();
                } catch (Exception e) {
                    log.error("gRPC Error: {} - {}", methodName, e.getMessage(), e);
                    throw e;
                }
            }
        };
    }
}
