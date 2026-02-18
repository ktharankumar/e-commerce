package com.example.carts.controller;

import com.example.carts.dto.AddItemRequestDTO;
import com.example.carts.dto.CartResponseDTO;
import com.example.carts.dto.CheckoutSummaryDTO;
import com.example.carts.service.CartService;
import com.example.carts.service.CheckoutService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * REST controller for shopping cart operations.
 *
 * <p>All endpoints require JWT authentication. Follows REST naming
 * conventions with noun-based, plural resource URIs.</p>
 */
@RestController
@RequestMapping("/api/v1/carts")
@Tag(name = "Carts", description = "Shopping cart management APIs")
@SecurityRequirement(name = "bearerAuth")
public class CartController {

    private final CartService cartService;
    private final CheckoutService checkoutService;

    public CartController(CartService cartService, CheckoutService checkoutService) {
        this.cartService = cartService;
        this.checkoutService = checkoutService;
    }

    @Operation(summary = "Get cart with checkout summary",
               description = "Retrieves the user's cart enriched with real-time product pricing from the Product service")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Cart retrieved successfully",
                    content = @Content(schema = @Schema(implementation = CheckoutSummaryDTO.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "404", description = "Cart not found")
    })
    @GetMapping("/{userId}")
    public ResponseEntity<CheckoutSummaryDTO> getCart(
            @Parameter(description = "User ID", example = "1") @PathVariable Long userId) {
        return ResponseEntity.ok(checkoutService.buildCheckoutSummary(userId));
    }

    @Operation(summary = "Add item to cart",
               description = "Adds a product to the user's cart. Creates a new cart if none exists.")
    @ApiResponses({
            @ApiResponse(responseCode = "201", description = "Item added to cart",
                    content = @Content(schema = @Schema(implementation = CartResponseDTO.class))),
            @ApiResponse(responseCode = "400", description = "Invalid input"),
            @ApiResponse(responseCode = "401", description = "Unauthorized")
    })
    @PostMapping("/{userId}/items")
    public ResponseEntity<CartResponseDTO> addItemToCart(
            @Parameter(description = "User ID", example = "1") @PathVariable Long userId,
            @Valid @RequestBody AddItemRequestDTO addItemRequestDTO) {
        CartResponseDTO cart = cartService.addToCart(userId, addItemRequestDTO);
        return new ResponseEntity<>(cart, HttpStatus.CREATED);
    }

    @Operation(summary = "Reduce item quantity",
               description = "Decreases the quantity of a product in the cart. Removes it if quantity reaches 0.")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Quantity reduced",
                    content = @Content(schema = @Schema(implementation = CartResponseDTO.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "404", description = "Cart or item not found"),
            @ApiResponse(responseCode = "409", description = "Concurrent modification conflict")
    })
    @PatchMapping("/{userId}/items/{productId}")
    public ResponseEntity<CartResponseDTO> reduceCount(
            @Parameter(description = "User ID") @PathVariable Long userId,
            @Parameter(description = "Product ID") @PathVariable Long productId,
            @Parameter(description = "Amount to reduce") @RequestParam(defaultValue = "1") int amount) {
        return ResponseEntity.ok(cartService.reduceFromCart(userId, productId, amount));
    }

    @Operation(summary = "Remove item from cart",
               description = "Completely removes a product from the user's cart")
    @ApiResponses({
            @ApiResponse(responseCode = "204", description = "Item removed"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "404", description = "Cart or item not found")
    })
    @DeleteMapping("/{userId}/items/{productId}")
    public ResponseEntity<Void> removeProduct(
            @Parameter(description = "User ID") @PathVariable Long userId,
            @Parameter(description = "Product ID") @PathVariable Long productId) {
        cartService.removeFromCart(userId, productId);
        return ResponseEntity.noContent().build();
    }

    @Operation(summary = "Get checkout summary",
               description = "Builds a checkout summary with enriched product data and availability status")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Checkout summary built",
                    content = @Content(schema = @Schema(implementation = CheckoutSummaryDTO.class))),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "404", description = "Cart not found")
    })
    @GetMapping("/{userId}/checkout-summary")
    public ResponseEntity<CheckoutSummaryDTO> checkoutSummary(
            @Parameter(description = "User ID") @PathVariable Long userId) {
        return ResponseEntity.ok(checkoutService.buildCheckoutSummary(userId));
    }
}
