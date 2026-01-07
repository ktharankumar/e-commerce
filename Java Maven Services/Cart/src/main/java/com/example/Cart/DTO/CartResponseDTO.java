package com.example.Cart.DTO;

import java.math.BigDecimal;
import java.util.List;

public record CartResponseDTO(
        Long cartId,
        Long userId,
        String couponCode,
        BigDecimal cartTotal,
        List<CartItemResponseDTO> items
) {}