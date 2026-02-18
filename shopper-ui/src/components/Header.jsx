import React, { useState, useRef, useEffect } from "react";
import { useAuth } from "../context/AuthContext.jsx";
import { CATEGORIES, CATEGORY_ICONS } from "../api/productApi";

export default function Header({ onGoProducts, onGoCart, onGoCreate, onGoLogin, onGoRegister, cartCount }) {
  const { isAuthenticated, user, logout } = useAuth();
  const [catOpen, setCatOpen] = useState(false);
  const [userOpen, setUserOpen] = useState(false);
  const catRef = useRef(null);
  const userRef = useRef(null);

  // Close dropdowns on outside click
  useEffect(() => {
    function handleClick(e) {
      if (catRef.current && !catRef.current.contains(e.target)) setCatOpen(false);
      if (userRef.current && !userRef.current.contains(e.target)) setUserOpen(false);
    }
    document.addEventListener("mousedown", handleClick);
    return () => document.removeEventListener("mousedown", handleClick);
  }, []);

  return (
    <nav className="navbar">
      <div className="container navbar-inner">
        {/* Brand */}
        <div className="brand" onClick={onGoProducts} role="button" tabIndex={0}>
          <div className="brand-logo">S</div>
          <div className="brand-text">
            <div className="brand-name">Shoppers Stop</div>
            <div className="brand-sub">Premium Shopping</div>
          </div>
        </div>

        {/* Categories Dropdown */}
        <div className="nav-dropdown nav-categories" ref={catRef}>
          <button
            className={`nav-dropdown-trigger ${catOpen ? "active" : ""}`}
            onClick={() => setCatOpen(!catOpen)}
          >
            <span>📂</span> Categories <span style={{ fontSize: 10 }}>▼</span>
          </button>
          <div className={`nav-dropdown-menu ${catOpen ? "show" : ""}`}>
            <div className="nav-dropdown-header">Browse by category</div>
            <button className="nav-dropdown-item" onClick={() => { onGoProducts?.(""); setCatOpen(false); }}>
              <span className="item-icon">🏷️</span> All Products
            </button>
            <div className="nav-dropdown-divider" />
            {CATEGORIES.map((c) => (
              <button
                key={c}
                className="nav-dropdown-item"
                onClick={() => { onGoProducts?.(c); setCatOpen(false); }}
              >
                <span className="item-icon">{CATEGORY_ICONS[c] || "📦"}</span>
                {c.replace(/_/g, " ")}
              </button>
            ))}
          </div>
        </div>

        {/* Search */}
        <div className="search-wrapper nav-middle">
          <span className="search-icon">🔍</span>
          <input
            className="search-input"
            placeholder="Search products..."
            id="header-search"
            onKeyDown={(e) => {
              if (e.key === "Enter") {
                onGoProducts?.("", e.target.value);
              }
            }}
          />
        </div>

        {/* Right Actions */}
        <div className="nav-actions">
          {/* Add Product (Admin only) */}
          {isAuthenticated && user?.roles?.includes("ROLE_ADMIN") && (
            <button className="nav-dropdown-trigger" onClick={onGoCreate}>
              <span>➕</span> Add
            </button>
          )}

          {/* Cart */}
          <button className="cart-btn" onClick={onGoCart}>
            <span>🛒</span> Cart
            {cartCount > 0 && <span className="cart-badge">{cartCount}</span>}
          </button>

          {/* User Menu */}
          {isAuthenticated ? (
            <div className="nav-dropdown" ref={userRef}>
              <button
                className={`nav-dropdown-trigger ${userOpen ? "active" : ""}`}
                onClick={() => setUserOpen(!userOpen)}
              >
                <span>👤</span> {user?.username} <span style={{ fontSize: 10 }}>▼</span>
              </button>
              <div className={`nav-dropdown-menu right ${userOpen ? "show" : ""}`}>
                <div className="nav-dropdown-header">Account</div>
                <div className="nav-dropdown-item" style={{ cursor: "default", opacity: 0.7 }}>
                  <span className="item-icon">📧</span> {user?.email}
                </div>
                <div className="nav-dropdown-divider" />
                <button
                  className="nav-dropdown-item"
                  onClick={() => { logout(); setUserOpen(false); }}
                  style={{ color: "#ef4444" }}
                >
                  <span className="item-icon">🚪</span> Logout
                </button>
              </div>
            </div>
          ) : (
            <>
              <button className="btn btn-ghost" onClick={onGoLogin}>Sign In</button>
              <button className="btn btn-primary" onClick={onGoRegister}>Register</button>
            </>
          )}
        </div>
      </div>
    </nav>
  );
}
