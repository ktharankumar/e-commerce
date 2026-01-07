package com.example.Cart.Mapper;

import com.example.Cart.DTO.CartItemResponseDTO;
import com.example.Cart.DTO.CartResponseDTO;
import com.example.Cart.Entity.Cart;
import com.example.Cart.Entity.CartItems;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface CartMapper{
    @Mapping(source="cartItems", target= "items")
    CartResponseDTO toCartResponseDTO(Cart cart);

    CartItemResponseDTO toCartItemDTO(CartItems cartItems);
}
