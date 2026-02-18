package com.example.carts.exception;

import java.time.Instant;

/**
 * Standard API error response following RFC 7807 Problem Details pattern.
 */
public record ApiErrorResponse(
        Instant timestamp,
        int status,
        String error,
        String message,
        String path
) {
    public static ApiErrorResponse of(int status, String error, String message, String path) {
        return new ApiErrorResponse(Instant.now(), status, error, message, path);
    }
}
