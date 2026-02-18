package com.example.products.strategy;

import java.math.BigDecimal;

/**
 * Strategy interface for calculating final product prices.
 *
 * <p>Follows the <strong>Strategy Design Pattern</strong> to decouple
 * pricing logic from the Product entity. Different pricing strategies
 * (percentage discounts, flat discounts, seasonal pricing, etc.) can be
 * implemented independently without modifying the Product class.</p>
 *
 * <p>This adheres to the <strong>Open/Closed Principle</strong> (SOLID) —
 * new pricing strategies can be added without changing existing code.</p>
 *
 * @see PercentageDiscountStrategy
 * @see NoDiscountStrategy
 * @see PricingStrategyFactory
 */
public interface PricingStrategy {

    /**
     * Calculates the final price after applying the pricing strategy.
     *
     * @param basePrice          the original product price
     * @param discountPercentage the discount percentage (may be null for some strategies)
     * @return the calculated final price
     */
    BigDecimal calculateFinalPrice(BigDecimal basePrice, Double discountPercentage);
}
