-- Flyway migration: Performance indexes for products table
-- V7: Adds indexes on category and product_name for faster query performance

-- Index on category for filtered product listings
CREATE INDEX IF NOT EXISTS idx_products_category ON products (category);

-- Index on product_name for search functionality
CREATE INDEX IF NOT EXISTS idx_products_product_name ON products (product_name);

-- Index on is_available for availability filtering
CREATE INDEX IF NOT EXISTS idx_products_is_available ON products (is_available);
