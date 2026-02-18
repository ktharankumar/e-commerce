package com.example.products.controller;

import com.example.products.dto.ProductCreateRequestDTO;
import com.example.products.dto.ProductResponseDTO;
import com.example.products.entity.Category;
import com.example.products.service.ProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * REST controller for product CRUD operations.
 *
 * <p>Follows REST API best practices:
 * <ul>
 *   <li>Noun-based, plural resource naming: {@code /api/v1/products}</li>
 *   <li>Proper HTTP methods: GET, POST, PUT, DELETE</li>
 *   <li>Correct status codes: 200, 201, 204, 400, 404</li>
 *   <li>Paginated list endpoints</li>
 *   <li>Bean Validation with {@code @Valid}</li>
 * </ul></p>
 */
@RestController
@RequestMapping("/api/v1/products")
@Tag(name = "Products", description = "Product management APIs for CRUD operations")
public class ProductController {

    private final ProductService productService;

    public ProductController(ProductService productService) {
        this.productService = productService;
    }

    @Operation(summary = "Create a new product",
               description = "Creates a new product. Requires JWT authentication.",
               security = @SecurityRequirement(name = "bearerAuth"))
    @ApiResponses({
            @ApiResponse(responseCode = "201", description = "Product created successfully",
                    content = @Content(schema = @Schema(implementation = ProductResponseDTO.class))),
            @ApiResponse(responseCode = "400", description = "Invalid input data"),
            @ApiResponse(responseCode = "401", description = "Unauthorized — JWT token required")
    })
    @PostMapping
    public ResponseEntity<ProductResponseDTO> createProduct(
            @Valid @RequestBody ProductCreateRequestDTO requestDTO) {
        return new ResponseEntity<>(productService.createProduct(requestDTO), HttpStatus.CREATED);
    }

    @Operation(summary = "Get product by ID",
               description = "Retrieves a specific product by its unique identifier. Public endpoint.")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Product found",
                    content = @Content(schema = @Schema(implementation = ProductResponseDTO.class))),
            @ApiResponse(responseCode = "404", description = "Product not found")
    })
    @GetMapping("/{id}")
    public ResponseEntity<ProductResponseDTO> getProduct(
            @Parameter(description = "Unique product ID", example = "1") @PathVariable Long id) {
        return ResponseEntity.ok(productService.getProductById(id));
    }

    @Operation(summary = "Get all products (paginated)",
               description = "Retrieves products with pagination and optional category filter. Public endpoint.")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Products retrieved successfully")
    })
    @GetMapping
    public ResponseEntity<Page<ProductResponseDTO>> getProducts(
            @Parameter(description = "Filter by category", example = "ELECTRONICS")
            @RequestParam(required = false) Category category,
            @PageableDefault(size = 20, sort = "id") Pageable pageable) {
        return ResponseEntity.ok(productService.getAllProducts(category, pageable));
    }

    @Operation(summary = "Update a product",
               description = "Updates an existing product. Requires JWT authentication.",
               security = @SecurityRequirement(name = "bearerAuth"))
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Product updated successfully",
                    content = @Content(schema = @Schema(implementation = ProductResponseDTO.class))),
            @ApiResponse(responseCode = "400", description = "Invalid input data"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "404", description = "Product not found")
    })
    @PutMapping("/{id}")
    public ResponseEntity<ProductResponseDTO> updateProduct(
            @Parameter(description = "Product ID to update", example = "1") @PathVariable Long id,
            @Valid @RequestBody ProductCreateRequestDTO requestDTO) {
        return ResponseEntity.ok(productService.updateProduct(id, requestDTO));
    }

    @Operation(summary = "Delete a product",
               description = "Deletes a product by ID. Requires JWT authentication.",
               security = @SecurityRequirement(name = "bearerAuth"))
    @ApiResponses({
            @ApiResponse(responseCode = "204", description = "Product deleted successfully"),
            @ApiResponse(responseCode = "401", description = "Unauthorized"),
            @ApiResponse(responseCode = "404", description = "Product not found")
    })
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteProduct(
            @Parameter(description = "Product ID to delete", example = "1") @PathVariable Long id) {
        productService.deleteProduct(id);
        return ResponseEntity.noContent().build();
    }

    @Operation(summary = "Get products by IDs (batch)",
               description = "Internal endpoint used by Cart service for batch product retrieval.")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Products retrieved successfully")
    })
    @PostMapping("/batch")
    public ResponseEntity<List<ProductResponseDTO>> getProductsBatch(
            @RequestBody List<Long> ids) {
        return ResponseEntity.ok(productService.getProductsByIds(ids));
    }
}
