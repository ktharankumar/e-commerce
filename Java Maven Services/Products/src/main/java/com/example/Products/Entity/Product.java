package com.example.Products.Entity;


import jakarta.persistence.*;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.math.BigDecimal;

@Entity
@Getter
@Setter
@Table(name = "products")
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

    private double discount = 0.0;
    private String specifications;

    @Column(name = "available_quantity", nullable = false)
    @Min(value = 1, message = "Quantity must be at least 1")
    private int availableQuantity;

    @Column(name="is_available", nullable=false)
    private Boolean isAvailable=false;

}
