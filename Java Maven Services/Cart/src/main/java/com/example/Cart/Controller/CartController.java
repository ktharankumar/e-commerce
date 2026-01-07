package com.example.Cart.Controller;

import lombok.RequiredArgsConstructor;
import com.example.Cart.DTO.AddItemRequestDTO;
import com.example.Cart.DTO.CartResponseDTO;
import com.example.Cart.DTO.RemoveItemRequestDTO;
import com.example.Cart.Entity.Cart;
import com.example.Cart.Service.CartService;
import org.springframework.web.bind.annotation.*;

@RequiredArgsConstructor // Constructor DI
@RestController
@CrossOrigin(origins = "http://localhost:3000")
@RequestMapping("/api/cart")
public class CartController {

    private final CartService cartService;

    @GetMapping("/{userId}")
    public CartResponseDTO getCart(@PathVariable Long userId) {
        return cartService.getCartByUserId(userId);
    }

    @PostMapping("/{userId}/items")
    public CartResponseDTO addItemToCart(@PathVariable Long userId, @RequestBody AddItemRequestDTO addItemRequestDTO) {
        return cartService.addToCart(userId, addItemRequestDTO);
    }

    @PatchMapping("{userId}/items/{productId}")
    public CartResponseDTO reduceCount(@PathVariable Long userId, @PathVariable Long productId, @RequestParam(defaultValue = "1") int amount) {
        return cartService.reduceFromCart(userId, productId, amount);
    }

    @DeleteMapping("/{userId}/items/{productId}")
    public CartResponseDTO removeProduct(@PathVariable Long userId, @PathVariable Long productId) {
        return cartService.removeFromCart(userId, productId);
    }
}
