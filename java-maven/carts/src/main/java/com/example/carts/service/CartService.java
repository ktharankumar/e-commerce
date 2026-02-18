package com.example.carts.service;

import com.example.carts.dto.AddItemRequestDTO;
import com.example.carts.dto.CartResponseDTO;
import com.example.carts.entity.Cart;
import com.example.carts.entity.CartItems;
import com.example.carts.exception.ResourceNotFoundException;
import com.example.carts.mapper.CartMapper;
import com.example.carts.repository.CartRepo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

/**
 * Service layer for shopping cart business logic.
 *
 * <p>All mutation methods are annotated with {@code @Transactional}
 * for ACID compliance. The Cart entity uses {@code @Version} for
 * optimistic locking, so concurrent modifications will throw
 * {@link org.springframework.dao.OptimisticLockingFailureException}
 * (handled by the global exception handler with a 409 Conflict response).</p>
 *
 * <p>Follows the <strong>Single Responsibility Principle</strong> — handles
 * only cart manipulation logic. Checkout is delegated to {@link CheckoutService}.</p>
 */
@Service
public class CartService {

    private static final Logger logger = LoggerFactory.getLogger(CartService.class);

    private final CartRepo cartRepo;
    private final CartMapper cartMapper;

    public CartService(CartRepo cartRepo, CartMapper cartMapper) {
        this.cartRepo = cartRepo;
        this.cartMapper = cartMapper;
    }

    /** Retrieves the user's cart or throws 404 if not found. */
    @Transactional(readOnly = true)
    public CartResponseDTO getCart(Long userId) {
        Cart cart = findCartByUserId(userId);
        return cartMapper.toCartResponseDTO(cart);
    }

    /**
     * Adds an item to the user's cart. Creates a new cart if none exists.
     * If the product already exists in the cart, increases its quantity.
     */
    @Transactional
    public CartResponseDTO addToCart(Long userId, AddItemRequestDTO request) {
        Cart cart = cartRepo.findByUserId(userId)
                .orElseGet(() -> createNewCart(userId));

        cart.getCartItems().stream()
                .filter(item -> item.getProductId().equals(request.productId()))
                .findFirst()
                .ifPresentOrElse(
                        existingItem -> {
                            existingItem.increaseQuantity(request.quantity());
                            existingItem.calculateTotalPrice();
                        },
                        () -> {
                            CartItems newItem = new CartItems();
                            newItem.setProductId(request.productId());
                            newItem.setQuantity(request.quantity());
                            newItem.calculateTotalPrice();
                            cart.addCartItem(newItem);
                        }
                );

        Cart savedCart = cartRepo.save(cart);
        logger.info("Added product {} to cart for user {}", request.productId(), userId);
        return cartMapper.toCartResponseDTO(savedCart);
    }

    /** Removes an item entirely from the user's cart. */
    @Transactional
    public CartResponseDTO removeFromCart(Long userId, Long productId) {
        Cart cart = findCartByUserId(userId);

        CartItems item = findCartItem(cart, productId);
        cart.removeCartItem(item);

        Cart savedCart = cartRepo.save(cart);
        logger.info("Removed product {} from cart for user {}", productId, userId);
        return cartMapper.toCartResponseDTO(savedCart);
    }

    /** Decreases the quantity of an item. Removes the item if quantity reaches 0. */
    @Transactional
    public CartResponseDTO reduceFromCart(Long userId, Long productId, int amount) {
        Cart cart = findCartByUserId(userId);

        CartItems item = findCartItem(cart, productId);
        item.decreaseQuantity(amount);

        if (item.getQuantity() <= 0) {
            cart.removeCartItem(item);
        } else {
            item.calculateTotalPrice();
        }

        Cart savedCart = cartRepo.save(cart);
        logger.info("Reduced quantity for product {} in cart for user {} by {}", productId, userId, amount);
        return cartMapper.toCartResponseDTO(savedCart);
    }

    // ── Helper Methods ──────────────────────────────────────────────────

    private Cart findCartByUserId(Long userId) {
        return cartRepo.findByUserId(userId)
                .orElseThrow(() -> new ResourceNotFoundException(
                        "Cart not found for user id: " + userId));
    }

    private CartItems findCartItem(Cart cart, Long productId) {
        return cart.getCartItems().stream()
                .filter(item -> item.getProductId().equals(productId))
                .findFirst()
                .orElseThrow(() -> new ResourceNotFoundException(
                        "Item with product id " + productId + " not found in cart"));
    }

    /** Factory method — creates a new empty cart for a user. */
    private Cart createNewCart(Long userId) {
        Cart cart = new Cart();
        cart.setUserId(userId);
        logger.info("Created new cart for user {}", userId);
        return cart;
    }
}
