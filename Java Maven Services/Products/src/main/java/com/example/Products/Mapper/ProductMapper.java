package com.example.Products.Mapper;

import com.example.Products.DTO.AddProductRequestDTO;
import com.example.Products.DTO.GetProductResponseDTO;
import com.example.Products.Entity.Product;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface ProductMapper {
    GetProductResponseDTO toProductResponseDTO(Product product);
    Product toProduct(AddProductRequestDTO addProductRequestDTO);
}
