package com.example.products.service;

import com.example.products.dto.ProductCreateRequestDTO;
import com.example.products.dto.ProductResponseDTO;
import com.example.products.entity.Category;
import com.example.products.entity.Product;
import com.example.products.exception.ResourceNotFoundException;
import com.example.products.mapper.ProductMapper;
import com.example.products.repository.ProductRepo;
import com.example.products.strategy.PricingStrategy;
import com.example.products.strategy.PricingStrategyFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * Service layer for product business logic.
 *
 * <p>Follows the <strong>Single Responsibility Principle</strong> — handles
 * only product CRUD operations and delegates pricing to the
 * {@link PricingStrategyFactory} (Strategy Pattern).</p>
 *
 * <p>All mutation methods are annotated with {@code @Transactional}
 * for ACID compliance. Read methods use {@code readOnly = true}
 * for optimized database access.</p>
 */
@Service
public class ProductService {

    private static final Logger logger = LoggerFactory.getLogger(ProductService.class);

    private final ProductRepo productRepo;
    private final ProductMapper productMapper;
    private final PricingStrategyFactory pricingStrategyFactory;

    public ProductService(ProductRepo productRepo,
                          ProductMapper productMapper,
                          PricingStrategyFactory pricingStrategyFactory) {
        this.productRepo = productRepo;
        this.productMapper = productMapper;
        this.pricingStrategyFactory = pricingStrategyFactory;
    }

    /**
     * Retrieves all products with optional category filtering and pagination.
     *
     * @param category optional category filter
     * @param pageable pagination parameters (page, size, sort)
     * @return paginated product results
     */
    @Transactional(readOnly = true)
    public Page<ProductResponseDTO> getAllProducts(Category category, Pageable pageable) {
        Page<Product> products = (category != null)
                ? productRepo.findByCategory(category, pageable)
                : productRepo.findAll(pageable);

        return products.map(this::mapToResponseWithPricing);
    }

    /** Retrieves a single product by ID, applying pricing strategy. */
    @Transactional(readOnly = true)
    public ProductResponseDTO getProductById(Long id) {
        Product product = productRepo.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Product not found with id: " + id));
        return mapToResponseWithPricing(product);
    }

    /** Creates a new product. Atomic via {@code @Transactional}. */
    @Transactional
    public ProductResponseDTO createProduct(ProductCreateRequestDTO requestDTO) {
        Product product = productMapper.toProduct(requestDTO);
        Product savedProduct = productRepo.save(product);
        logger.info("Created product: {} (id={})", savedProduct.getProductName(), savedProduct.getId());
        return mapToResponseWithPricing(savedProduct);
    }

    /** Updates an existing product. Atomic via {@code @Transactional}. */
    @Transactional
    public ProductResponseDTO updateProduct(Long id, ProductCreateRequestDTO requestDTO) {
        Product existingProduct = productRepo.findById(id)
                .orElseThrow(() -> new ResourceNotFoundException("Product not found with id: " + id));

        existingProduct.setProductName(requestDTO.productName());
        existingProduct.setPrice(requestDTO.price());
        existingProduct.setDiscountPercentage(requestDTO.discountPercentage());
        existingProduct.setCategory(requestDTO.category());
        existingProduct.setAvailableQuantity(requestDTO.availableQuantity());

        Product updatedProduct = productRepo.save(existingProduct);
        logger.info("Updated product: {} (id={})", updatedProduct.getProductName(), id);
        return mapToResponseWithPricing(updatedProduct);
    }

    /** Deletes a product by ID. Atomic via {@code @Transactional}. */
    @Transactional
    public void deleteProduct(Long id) {
        if (!productRepo.existsById(id)) {
            throw new ResourceNotFoundException("Cannot delete. Product not found with id: " + id);
        }
        productRepo.deleteById(id);
        logger.info("Deleted product with id={}", id);
    }

    /** Batch retrieval for inter-service communication (used by Cart service). */
    @Transactional(readOnly = true)
    public List<ProductResponseDTO> getProductsByIds(List<Long> ids) {
        return productRepo.findAllById(ids).stream()
                .map(this::mapToResponseWithPricing)
                .toList();
    }

    /**
     * Maps a Product entity to a response DTO, applying the pricing strategy.
     * Uses the Factory pattern to select the appropriate strategy.
     */
    private ProductResponseDTO mapToResponseWithPricing(Product product) {
        PricingStrategy strategy = pricingStrategyFactory.getStrategy(product.getDiscountPercentage());
        product.setFinalPrice(strategy.calculateFinalPrice(product.getPrice(), product.getDiscountPercentage()));
        return productMapper.toProductResponseDTO(product);
    }
}