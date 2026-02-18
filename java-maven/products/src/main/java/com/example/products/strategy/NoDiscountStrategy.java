package com.example.products.strategy;

import org.springframework.stereotype.Component;

import java.math.BigDecimal;

/**
 * No-op pricing strategy — returns the base price unchanged.
 * Used when a product has no discount applied.
 */
@Component
public class NoDiscountStrategy implements PricingStrategy {

    @Override
    public BigDecimal calculateFinalPrice(BigDecimal basePrice, Double discountPercentage) {
        return basePrice;
    }
}
