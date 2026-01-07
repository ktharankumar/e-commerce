package com.example.Cart.DTO;

public record RemoveItemRequestDTO(
        Long cartId,
        Long productId,
        Integer quantity
){}