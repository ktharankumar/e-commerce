package com.example.Products.Service;

import com.example.Products.DTO.AddProductRequestDTO;
import com.example.Products.DTO.GetProductResponseDTO;
import com.example.Products.Entity.Category;
import com.example.Products.Mapper.ProductMapper;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;
import com.example.Products.Entity.Product;
import com.example.Products.Repository.ProductRepo;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@RequiredArgsConstructor
@Service
@Getter
@ToString
public class ProductService {

    private final ProductRepo productRepo;

    private final ProductMapper productMapper;

    public List<Product> getAllProducts() {
        return productRepo.findAll();
    }

    public Product getProduct(long productId){
        return productRepo.findById(productId);
    }

    public List<Product> getProductByCategory(Category category){
        return productRepo.findByCategory(category);
    }

    public GetProductResponseDTO addProduct(AddProductRequestDTO addProductRequestDTO){
        return productMapper.toProductResponseDTO(productMapper.toProduct(addProductRequestDTO));
    }

    public void updateProduct(Product productToUpdate){
        Product Product = productRepo.findAll().
                stream().
                filter(p -> p.getId() == (productToUpdate.getId())).
                findFirst().orElseThrow(() -> new RuntimeException("Product Not found"));
        //productToUpdate.setImage(Base64.getDecoder().decode(Product.getImage()));
        productToUpdate.setProductName(Product.getProductName());
        productToUpdate.setPrice(Product.getPrice());
        productToUpdate.setDiscount(Product.getDiscount());
        productRepo.save(productToUpdate);
    }

    public BigDecimal getProductPrice(Long productId){
        Optional<Product> ThisProduct = productRepo.findAll().
                stream().
                filter(p -> p.getId() == productId).
                findFirst();
        if(ThisProduct.isPresent()){
            Product product = ThisProduct.get();
            return product.getPrice().multiply(BigDecimal.valueOf(product.getDiscount()).divide(BigDecimal.valueOf(100),2, RoundingMode.HALF_UP));
        }
        else throw new RuntimeException("Product not found");
    }
}
