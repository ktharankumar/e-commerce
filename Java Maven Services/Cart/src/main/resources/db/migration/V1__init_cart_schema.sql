-- carts table
CREATE TABLE carts (
                       id      BIGSERIAL PRIMARY KEY,
                       coupon_code  VARCHAR(255),
                       cart_total   NUMERIC(19,2) NOT NULL DEFAULT 0,
                       version      BIGINT NOT NULL DEFAULT 0,
                       user_id      BIGINT
);

-- cart_items table
CREATE TABLE cart_items (
                            id BIGSERIAL PRIMARY KEY,
                            quantity     INTEGER NOT NULL DEFAULT 0,
                            price        NUMERIC(19,2),
                            total_price  NUMERIC(19,2) NOT NULL DEFAULT 0,
                            product_id   BIGINT,
                            cart_id      BIGINT,

                            CONSTRAINT fk_cart_items_cart
                                FOREIGN KEY (cart_id)
                                    REFERENCES carts(id)
                                    ON DELETE CASCADE
);

CREATE INDEX idx_cart_items_cart_id ON cart_items(cart_id);
CREATE INDEX idx_cart_items_product_id ON cart_items(product_id);
CREATE INDEX idx_carts_user_id ON carts(user_id);
