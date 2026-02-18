package com.example.products.config;

import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.security.SecurityScheme;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.servers.Server;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

/**
 * OpenAPI/Swagger configuration for the Product service.
 *
 * <p>Defines the API documentation metadata and JWT bearer token
 * security scheme. The {@code bearerAuth} scheme is referenced by
 * {@code @SecurityRequirement} annotations on protected endpoints.</p>
 */
@Configuration
@SecurityScheme(
        name = "bearerAuth",
        type = SecuritySchemeType.HTTP,
        scheme = "bearer",
        bearerFormat = "JWT",
        description = "JWT token obtained from POST /api/auth/signin on the Users service"
)
public class OpenApiConfig {

    @Bean
    public OpenAPI productServiceOpenAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title("Product Service API")
                        .description("RESTful API for managing products in the Shoppers e-commerce platform. "
                                + "Supports CRUD operations with JWT authentication for write operations.")
                        .version("1.0.0")
                        .contact(new Contact()
                                .name("Shoppers Team")
                                .email("team@shoppers.dev"))
                        .license(new License()
                                .name("MIT License")
                                .url("https://opensource.org/licenses/MIT")))
                .servers(List.of(
                        new Server().url("http://localhost:8081").description("Local development"),
                        new Server().url("https://api.shoppers.dev").description("Production")
                ));
    }
}
