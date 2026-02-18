package com.example.products.exception;

/**
 * Custom exception for product resource not found scenarios.
 * Extends {@link RuntimeException} for unchecked exception handling.
 */
public class ResourceNotFoundException extends RuntimeException {
    public ResourceNotFoundException(String message) {
        super(message);
    }
}
