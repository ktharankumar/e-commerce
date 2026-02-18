-- Flyway migration: Performance indexes for app_user table
-- V4: Adds indexes on username and email for faster lookups and uniqueness enforcement

-- Index on username for login lookups
CREATE INDEX IF NOT EXISTS idx_users_username ON app_user (user_name);

-- Index on email for uniqueness checks and lookups
CREATE INDEX IF NOT EXISTS idx_users_email ON app_user (email);
