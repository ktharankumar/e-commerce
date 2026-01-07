ALTER TABLE products
    ADD CONSTRAINT check_minimum_quantity
        CHECK (available_quantity >= 1);