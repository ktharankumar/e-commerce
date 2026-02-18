package com.example.products.strategy;

import org.springframework.stereotype.Component;

/**
 * Factory for selecting the appropriate {@link PricingStrategy}
 * based on whether a discount is present.
 *
 * <p>Follows the <strong>Factory Design Pattern</strong> to encapsulate
 * strategy selection logic. Clients do not need to know the concrete
 * strategy class — they simply ask the factory.</p>
 */
@Component
public class PricingStrategyFactory {

    private final PercentageDiscountStrategy percentageDiscountStrategy;
    private final NoDiscountStrategy noDiscountStrategy;

    public PricingStrategyFactory(PercentageDiscountStrategy percentageDiscountStrategy,
                                  NoDiscountStrategy noDiscountStrategy) {
        this.percentageDiscountStrategy = percentageDiscountStrategy;
        this.noDiscountStrategy = noDiscountStrategy;
    }

    /**
     * Selects the appropriate pricing strategy based on the discount value.
     *
     * @param discountPercentage the discount to evaluate (may be null)
     * @return the selected {@link PricingStrategy}
     */
    public PricingStrategy getStrategy(Double discountPercentage) {
        if (discountPercentage != null && discountPercentage > 0) {
            return percentageDiscountStrategy;
        }
        return noDiscountStrategy;
    }
}
