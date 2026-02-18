package com.example.products.exception;

import java.time.Instant;

/**
 * Standard API error response following a consistent structure
 * inspired by RFC 7807 Problem Details.
 *
 * @param timestamp when the error occurred
 * @param status    HTTP status code
 * @param error     short error description
 * @param message   detailed error message
 * @param path      the request URI path
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
