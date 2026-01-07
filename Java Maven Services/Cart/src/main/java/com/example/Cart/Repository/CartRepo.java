package com.example.Cart.Repository;

import com.example.Cart.Entity.Cart;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface CartRepo extends JpaRepository<Cart, Long> {
    public Optional<Cart> findByUserId(Long userId);
}
