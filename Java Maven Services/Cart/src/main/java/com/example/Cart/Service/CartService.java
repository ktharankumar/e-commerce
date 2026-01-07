package com.example.Cart.Service;

import lombok.RequiredArgsConstructor;

import com.example.Cart.DTO.*;

import com.example.Cart.Entity.Cart;
import com.example.Cart.Mapper.CartMapper;
import com.example.Cart.Repository.CartRepo;
import com.example.Cart.Entity.CartItems;
//import com.example.Products.Entity.Product;
//import com.example.Products.Service.ProductService;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Optional;

@RequiredArgsConstructor
@Service
public class CartService {

    private final CartRepo cartRepo;
    private final CartMapper cartMapper;

    public CartResponseDTO getCartByUserId(Long userId) {
        Cart cart = cartRepo.findByUserId(userId).orElseThrow(() ->
                new RuntimeException("user not found"));
        return cartMapper.toCartResponseDTO(cart);
    }

    public CartResponseDTO addToCart(Long userId, AddItemRequestDTO request) {
        Long productId = request.productId();
        Cart cart = getCartOrCreateNewCart(userId);

        Optional<CartItems> existingItemOpt = getOptCartItem(cart, productId);

        int quantity = request.quantity();
        existingItemOpt.ifPresentOrElse(cartItems -> cartItems.increaseQuantity(quantity),
                () -> {addProductToCartItem(cart, productId, quantity);});
        return cartMapper.toCartResponseDTO(cartRepo.save(cart));
    }

    public CartResponseDTO removeFromCart(Long userId,Long productId) {
        Cart cart = getCartOrThrow(userId);

        Optional<CartItems> targetCartItemsOpt = getOptCartItem(cart, productId);

        targetCartItemsOpt.ifPresent(cart::removeCartItem);
        return cartMapper.toCartResponseDTO(cartRepo.save(cart));
    }

    public CartResponseDTO reduceFromCart(Long userId, Long productId, Integer amount) {
        Cart cart = getCartOrThrow(userId);

        Optional<CartItems> targetCartItemsOpt = getOptCartItem(cart, productId);

        targetCartItemsOpt.ifPresent(cartItems -> {
            cartItems.decreaseQuantity(amount);
            if(cartItems.getQuantity() <= 0) {
                cart.removeCartItem(cartItems);
            }
        });

        return cartMapper.toCartResponseDTO(cartRepo.save(cart));
    }

    // Helper functions

    private Cart getCartOrThrow(Long userId){
        return cartRepo.findByUserId(userId).
                orElseThrow(() -> new RuntimeException("Cart not found"));
    }

    private Cart getCartOrCreateNewCart(Long userId){
        return cartRepo.findByUserId(userId)
                .orElseGet(() -> {
                    Cart newCart = new Cart();
                    newCart.setUserId(userId);
                    return cartRepo.save(newCart);
                });
    }

    private Optional<CartItems> getOptCartItem(Cart cart, Long productId) {
        return cart.getCartItems()
                .stream().
                filter(item -> item.
                        getProductId().
                        equals(productId)).
                findFirst();
    }

    private void addProductToCartItem(Cart cart, Long productId, Integer quantity) {
        //Product product = productService.getProduct(productId);

        CartItems newItem = new CartItems(100, BigDecimal.valueOf(1000), 5);
        cart.addCartItem(newItem);
    }

}
