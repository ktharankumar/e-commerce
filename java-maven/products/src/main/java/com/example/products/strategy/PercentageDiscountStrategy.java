package com.example.products.strategy;

import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Applies a percentage-based discount to the base price.
 *
 * <p>Example: A product priced at $100 with a 15% discount
 * results in a final price of $85.00.</p>
 */
@Component
public class PercentageDiscountStrategy implements PricingStrategy {

    @Override
    public BigDecimal calculateFinalPrice(BigDecimal basePrice, Double discountPercentage) {
        if (discountPercentage == null || discountPercentage <= 0) {
            return basePrice;
        }
        BigDecimal discountFactor = BigDecimal.valueOf(discountPercentage)
                .divide(BigDecimal.valueOf(100), 4, RoundingMode.HALF_UP);
        return basePrice.subtract(basePrice.multiply(discountFactor))
                .setScale(2, RoundingMode.HALF_UP);
    }
}
