package com.example.carts.service;

import com.example.carts.dto.CheckoutItemDTO;
import com.example.carts.dto.CheckoutSummaryDTO;
import com.example.carts.entity.Cart;
import com.example.carts.entity.CartItems;
import com.example.carts.exception.ResourceNotFoundException;
import com.example.carts.integration.product.ProductClient;
import com.example.carts.integration.product.ProductSnapshotDTO;
import com.example.carts.repository.CartRepo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Builds checkout summaries by enriching cart data with real-time
 * product information from the Product service.
 *
 * <p>Separated from {@link CartService} following the
 * <strong>Single Responsibility Principle</strong>.</p>
 */
@Service
public class CheckoutService {

    private static final Logger logger = LoggerFactory.getLogger(CheckoutService.class);

    private final CartRepo cartRepo;
    private final ProductClient productClient;

    public CheckoutService(CartRepo cartRepo, ProductClient productClient) {
        this.cartRepo = cartRepo;
        this.productClient = productClient;
    }

    /**
     * Builds a checkout summary with up-to-date pricing from the Product service.
     *
     * @param userId the user whose cart to summarize
     * @return a complete checkout summary with per-item pricing and availability
     * @throws ResourceNotFoundException if the user has no cart
     */
    @Transactional(readOnly = true)
    public CheckoutSummaryDTO buildCheckoutSummary(Long userId) {
        Cart cart = cartRepo.findByUserId(userId)
                .orElseThrow(() -> new ResourceNotFoundException(
                        "Cart not found for user id: " + userId));

        List<CartItems> cartItems = cart.getCartItems();
        List<Long> productIds = cartItems.stream()
                .map(CartItems::getProductId)
                .distinct()
                .toList();

        // Inter-service call — JWT is propagated via ProductClientConfig interceptor
        List<ProductSnapshotDTO> products = productClient.getProductsBatch(productIds);
        Map<Long, ProductSnapshotDTO> byId = products.stream()
                .collect(Collectors.toMap(ProductSnapshotDTO::id, p -> p));

        BigDecimal total = BigDecimal.ZERO;
        boolean allAvailable = true;
        List<CheckoutItemDTO> items = new ArrayList<>();

        for (CartItems ci : cartItems) {
            ProductSnapshotDTO p = byId.get(ci.getProductId());

            if (p == null) {
                logger.warn("Product {} not found in Product service", ci.getProductId());
                allAvailable = false;
                items.add(new CheckoutItemDTO(ci.getProductId(), "UNKNOWN",
                        ci.getQuantity(), BigDecimal.ZERO, BigDecimal.ZERO, false));
                continue;
            }

            boolean available = Boolean.TRUE.equals(p.isAvailable());
            allAvailable = allAvailable && available;

            BigDecimal unitPrice = (p.finalPrice() != null ? p.finalPrice() : p.price());
            BigDecimal lineTotal = unitPrice.multiply(BigDecimal.valueOf(ci.getQuantity()));
            total = total.add(lineTotal);

            items.add(new CheckoutItemDTO(
                    ci.getProductId(),
                    p.productName(),
                    ci.getQuantity(),
                    unitPrice,
                    lineTotal,
                    available
            ));
        }

        return new CheckoutSummaryDTO(cart.getUserId(), cart.getId(), total, items, allAvailable);
    }
}
