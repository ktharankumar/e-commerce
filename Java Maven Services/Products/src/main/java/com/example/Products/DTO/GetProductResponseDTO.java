package com.example.Products.DTO;

import com.example.Products.Entity.Category;

import java.math.BigDecimal;

public record GetProductResponseDTO(
        String productName,
        BigDecimal price,
        double discount,
        String specifications,
        Boolean isAvailable) {}
