package com.example.Products.Controller;

import com.example.Products.DTO.AddProductRequestDTO;
import com.example.Products.DTO.GetProductResponseDTO;
import com.example.Products.Entity.Category;
import com.example.Products.Entity.Product;
import com.example.Products.Service.ProductService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin(origins = "http://localhost:3000")
@RequestMapping("/products")
@RequiredArgsConstructor
public class ProductController {
    private final ProductService productsService;

    @GetMapping
    public List<Product> getProductsByCategory(@RequestParam Category category)
    {return productsService.getProductByCategory(category);}

    @GetMapping("/getProduct/{productId}")
    public Product getProduct(@PathVariable Long productId) {
        return productsService.getProduct(productId);
    }

    @PostMapping("/addProduct")
    public GetProductResponseDTO addProduct(@RequestBody AddProductRequestDTO newProduct) {
        return productsService.addProduct(newProduct);
    }

    @PatchMapping("/updateProduct")
    public void updateProduct(@RequestBody Product Product) {
        productsService.updateProduct(Product);
    }
}