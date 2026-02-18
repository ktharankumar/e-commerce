-- Flyway migration: Performance indexes for carts tables
-- V2: Adds indexes on user_id and product_id for faster cart lookups

-- Index on user_id for fast cart retrieval by user
CREATE INDEX IF NOT EXISTS idx_carts_user_id ON carts (user_id);

-- Index on product_id in cart_items for fast item lookups
CREATE INDEX IF NOT EXISTS idx_cart_items_product_id ON cart_items (product_id);
