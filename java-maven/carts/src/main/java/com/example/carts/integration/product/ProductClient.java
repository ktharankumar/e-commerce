package com.example.carts.integration.product;

import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClient;

import java.util.List;

@Component
public class ProductClient {

    private final RestClient productRestClient;

    public ProductClient(RestClient productRestClient) {
        this.productRestClient = productRestClient;
    }

    public List<ProductSnapshotDTO> getProductsBatch(List<Long> ids) {
        return productRestClient.post()
                .uri("/api/v1/products/batch")
                .body(ids)
                .retrieve()
                .body(new ParameterizedTypeReference<List<ProductSnapshotDTO>>() {});
    }
}
