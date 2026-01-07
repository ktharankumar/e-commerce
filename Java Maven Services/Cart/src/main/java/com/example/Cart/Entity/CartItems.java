package com.example.Cart.Entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import java.math.BigDecimal;

@Entity
@Getter
@Setter
@Table(name = "cart_items")
@NoArgsConstructor
public class CartItems {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    private int quantity = 0;

    private BigDecimal price;

    @Column(name = "total_price")
    private BigDecimal totalPrice = BigDecimal.ZERO;

    @Column(name = "product_id")
    private Long productId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "cart_id")
    private Cart cart;

    public CartItems(long productId, BigDecimal price, Integer quantity) {
        this.productId = productId;
        this.price = price;
        this.quantity = quantity;
        calculateTotalPrice();
    }

    public void increaseQuantity(Integer amount) {
        this.quantity += amount;
        calculateTotalPrice();
    }

    public void decreaseQuantity(Integer amount) {
        this.quantity -= amount;
        if (this.quantity < 0) {
            this.quantity = 0;
        }
        calculateTotalPrice();
    }

    @PrePersist
    @PreUpdate
    public void calculateTotalPrice() {
        if(this.price != null) {
            this.totalPrice = this.price.multiply(BigDecimal.valueOf(quantity));
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CartItems cartItems)) return false;
        return id != null && id.equals(cartItems.getId());
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

}
