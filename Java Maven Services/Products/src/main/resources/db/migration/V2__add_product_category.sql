-- V2: Initial Category Setup
-- 1. Create the high-performance ENUM type
CREATE TYPE product_category AS ENUM ('ELECTRONICS', 'CLOTHING', 'HOME_GOODS', 'SHOES', 'FOODS', 'OTHER');

-- 2. Add the column to the existing products table
-- We allow it to be NULL for a moment or set a default so existing products don't crash
ALTER TABLE products ADD COLUMN category product_category DEFAULT 'OTHER';

-- 3. Make it NOT NULL after the default is applied
ALTER TABLE products ALTER COLUMN category SET NOT NULL;