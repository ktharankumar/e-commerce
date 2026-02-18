package com.example.carts.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Entity
@Table(name="carts")
@Getter
@Setter
public class Cart {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name="coupon_code")
    private String couponCode;

    @Column(name="cart_total")
    private BigDecimal cartTotal = BigDecimal.ZERO;

    @Version
    private Long version;

    @Getter(AccessLevel.NONE)
    @OneToMany(mappedBy = "cart", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<CartItems> cartItems = new ArrayList<>();

    @Column(name = "user_id")
    private Long userId;

    public List<CartItems> getCartItems() {
        return Collections.unmodifiableList(cartItems);
    }

    public void addCartItem(CartItems item) {
        this.cartItems.add(item);
        item.setCart(this);
    }

    public void removeCartItem(CartItems item) {
        this.cartItems.remove(item);
        item.setCart(null);
    }

    @PrePersist
    @PreUpdate
    public void recalculateTotal() {
        this.cartTotal = this.cartItems.stream().
                map(CartItems::getTotalPrice).
                reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Cart cart)) return false;
        return id != null && id.equals(cart.id);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}