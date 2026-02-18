package com.example.carts.entity;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Entity
@Table(name = "cart_items")
@Getter
@Setter
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

    public CartItems() {
    }

    public CartItems(long productId, Integer quantity) {
        this.productId = productId;
        this.quantity = quantity;
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
