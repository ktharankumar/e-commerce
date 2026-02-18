package com.example.carts.integration.product;

import jakarta.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestClient;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

/**
 * Configuration for the inter-service REST client that communicates
 * with the Product service.
 *
 * <p>Includes a request interceptor that propagates the JWT Authorization
 * header from the current incoming request to outbound product-service calls,
 * ensuring authenticated inter-service communication.</p>
 */
@Configuration
public class ProductClientConfig {

    @Bean
    RestClient productRestClient(@Value("${product.service.baseurl}") String baseUrl) {
        return RestClient.builder()
                .baseUrl(baseUrl)
                .requestInterceptor((request, body, execution) -> {
                    // Propagate JWT token from current request context
                    ServletRequestAttributes attrs =
                            (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
                    if (attrs != null) {
                        HttpServletRequest currentRequest = attrs.getRequest();
                        String authHeader = currentRequest.getHeader("Authorization");
                        if (authHeader != null) {
                            request.getHeaders().set("Authorization", authHeader);
                        }
                    }
                    return execution.execute(request, body);
                })
                .build();
    }
}
