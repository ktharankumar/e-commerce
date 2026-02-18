package com.example.products.repository;

import com.example.products.entity.Category;
import com.example.products.entity.Product;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/**
 * Spring Data JPA repository for {@link Product} entities.
 *
 * <p>Extends {@link JpaRepository} which inherits {@code PagingAndSortingRepository},
 * enabling paginated queries out of the box.</p>
 */
public interface ProductRepo extends JpaRepository<Product, Long> {

    /** Finds products by category with pagination support. */
    Page<Product> findByCategory(Category category, Pageable pageable);

    /** Finds products by category (non-paginated, retained for backward compatibility). */
    List<Product> findByCategory(Category category);
}
