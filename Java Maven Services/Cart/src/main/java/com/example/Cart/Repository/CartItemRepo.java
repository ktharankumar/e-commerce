package com.example.Cart.Repository;

import com.example.Cart.Entity.CartItems;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CartItemRepo extends JpaRepository<CartItems, Long> {

}
