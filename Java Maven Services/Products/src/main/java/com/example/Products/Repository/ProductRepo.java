package com.example.Products.Repository;

import com.example.Products.Entity.Category;
import com.example.Products.Entity.Product;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ProductRepo extends JpaRepository<Product, Long> {
    public List<Product> findByCategory(Category category);
    public Product findById(long id);
}
