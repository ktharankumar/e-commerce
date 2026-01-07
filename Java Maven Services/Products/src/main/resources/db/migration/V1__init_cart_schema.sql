CREATE TABLE products (
                          id BIGSERIAL PRIMARY KEY,
                          product_name VARCHAR(255) NOT NULL,
                          price NUMERIC(19,2) NOT NULL,
                          discount DOUBLE PRECISION NOT NULL DEFAULT 0.0,
                          specifications TEXT,
                          is_available BOOLEAN NOT NULL DEFAULT FALSE
);

-- Optional, but useful for filtering available products
CREATE INDEX idx_products_is_available ON products(is_available);

-- Optional, if you search by name often
CREATE INDEX idx_products_name ON products(product_name);
