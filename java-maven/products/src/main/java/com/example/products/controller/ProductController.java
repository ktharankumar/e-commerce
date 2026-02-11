package com.example.products.controller;

import java.util.List;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.example.products.dto.ProductCreateRequestDTO;
import com.example.products.dto.ProductResponseDTO;
import com.example.products.entity.Category;
import com.example.products.service.ProductService;

@RestController
@RequestMapping("/api/v1/products")
@Tag(name = "Products", description = "Product management APIs for creating, reading, updating, and deleting products")
public class ProductController {

        @Autowired
        private final ProductService productService;

        public ProductController(ProductService productService) {
                this.productService = productService;
        }

        @Operation(summary = "Create a new product", description = "Creates a new product with the provided details and returns the created product")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "201", description = "Product created successfully", content = @Content(schema = @Schema(implementation = ProductResponseDTO.class))),
                        @ApiResponse(responseCode = "400", description = "Invalid input data"),
                        @ApiResponse(responseCode = "500", description = "Internal server error")
        })
        @PostMapping
        public ResponseEntity<ProductResponseDTO> createProduct(
                        @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "Product details to create", required = true) @RequestBody ProductCreateRequestDTO requestDTO) {
                ProductResponseDTO createdProduct = productService.createProduct(requestDTO);
                return new ResponseEntity<>(createdProduct, HttpStatus.CREATED);
        }

        @Operation(summary = "Get product by ID", description = "Retrieves a specific product by its unique identifier")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Product found", content = @Content(schema = @Schema(implementation = ProductResponseDTO.class))),
                        @ApiResponse(responseCode = "404", description = "Product not found"),
                        @ApiResponse(responseCode = "500", description = "Internal server error")
        })
        @GetMapping("/{id}")
        public ResponseEntity<ProductResponseDTO> getProduct(
                        @Parameter(description = "Unique product ID", required = true, example = "1") @PathVariable Long id) {
                return ResponseEntity.ok(productService.getProductById(id));
        }

        @Operation(summary = "Get all products", description = "Retrieves all products, optionally filtered by category")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Products retrieved successfully"),
                        @ApiResponse(responseCode = "500", description = "Internal server error")
        })
        @GetMapping
        public ResponseEntity<List<ProductResponseDTO>> getProducts(
                        @Parameter(description = "Filter products by category", example = "ELECTRONICS") @RequestParam(required = false) Category category) {
                return ResponseEntity.ok(productService.getAllProducts(category));
        }

        @Operation(summary = "Update a product", description = "Updates an existing product with the provided details")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Product updated successfully", content = @Content(schema = @Schema(implementation = ProductResponseDTO.class))),
                        @ApiResponse(responseCode = "400", description = "Invalid input data"),
                        @ApiResponse(responseCode = "404", description = "Product not found"),
                        @ApiResponse(responseCode = "500", description = "Internal server error")
        })
        @PutMapping("/{id}")
        public ResponseEntity<ProductResponseDTO> updateProduct(
                        @Parameter(description = "Unique product ID to update", required = true, example = "1") @PathVariable Long id,
                        @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "Updated product details", required = true) @RequestBody ProductCreateRequestDTO requestDTO) {
                return ResponseEntity.ok(productService.updateProduct(id, requestDTO));
        }

        @Operation(summary = "Delete a product", description = "Deletes a product by its unique identifier")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "204", description = "Product deleted successfully"),
                        @ApiResponse(responseCode = "404", description = "Product not found"),
                        @ApiResponse(responseCode = "500", description = "Internal server error")
        })
        @DeleteMapping("/{id}")
        public ResponseEntity<Void> deleteProduct(
                        @Parameter(description = "Unique product ID to delete", required = true, example = "1") @PathVariable Long id) {
                productService.deleteProduct(id);
                return ResponseEntity.noContent().build();
        }

        @Operation(summary = "Get products by IDs (batch)", description = "Retrieves multiple products by their IDs in a single request. Used by Cart service.")
        @ApiResponses(value = {
                        @ApiResponse(responseCode = "200", description = "Products retrieved successfully"),
                        @ApiResponse(responseCode = "400", description = "Invalid input data"),
                        @ApiResponse(responseCode = "500", description = "Internal server error")
        })
        @PostMapping("/batch")
        public ResponseEntity<List<ProductResponseDTO>> getProductsBatch(
                        @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "List of product IDs to retrieve", required = true) @RequestBody List<Long> ids) {
                return ResponseEntity.ok(productService.getProductsByIds(ids));
        }
}
