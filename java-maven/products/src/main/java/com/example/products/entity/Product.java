package com.example.products.entity;


import jakarta.persistence.*;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.math.BigDecimal;
import java.math.RoundingMode;

@Entity
@Table(name = "products")
@Getter
@Setter
public class Product {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private long id;

    @Column(name = "product_name", nullable = false)
    private String productName;

    @NotNull(message = "Price is required")
    @DecimalMin(value = "0.0", inclusive = false, message = "Price must be > 0")
    private BigDecimal price;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private Category category;

    private Double discountPercentage = null;
    private String specifications;

    @Column(name = "available_quantity", nullable = false)
    @Min(value = 1, message = "Quantity must be at least 1")
    private int availableQuantity;

    @Column(name="is_available", nullable=false)
    private Boolean isAvailable=true;

    @Transient
    private BigDecimal finalPrice;

    public Product() {
    }

    public Product(String productName, BigDecimal price, Category category, int availableQuantity, String specifications) {
        this.productName = productName;
        this.price = price;
        this.category = category;
        this.availableQuantity = availableQuantity;
        this.specifications = specifications;
    }

    public void calculate_final_price() {
        if (discountPercentage != null) {
            BigDecimal discountAmount = BigDecimal.valueOf(discountPercentage).divide(BigDecimal.valueOf(100), 2, RoundingMode.HALF_UP);
            this.finalPrice = price.subtract(price.multiply(discountAmount));
        }
    }
}
