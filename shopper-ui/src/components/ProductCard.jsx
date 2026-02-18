import React from "react";
import { CATEGORY_ICONS } from "../api/productApi";

function money(n) {
  const num = Number(n ?? 0);
  return new Intl.NumberFormat(undefined, { style: "currency", currency: "EUR" }).format(num);
}

function mockRating() {
  // Generate a pseudo-random rating based on the current render
  return (3.5 + Math.random() * 1.5).toFixed(1);
}

export default function ProductCard({ p, onOpen, onAddToCart }) {
  const category = (p.category || "OTHER").toLowerCase();
  const icon = CATEGORY_ICONS[p.category] || "📦";
  const rating = mockRating();

  return (
    <div className="card">
      {/* Image Area */}
      <div className="card-image">
        <div className={`card-image-bg ${category}`}>
          <span>{icon}</span>
        </div>
        {p.discountPercentage > 0 && (
          <div className="discount-ribbon">{p.discountPercentage}% OFF</div>
        )}
        <div className="card-quick-view" onClick={() => onOpen(p.id)}>
          <span>Quick View</span>
        </div>
      </div>

      {/* Body */}
      <div className="card-body">
        <div className="card-top">
          <div className="badge">{p.category || "OTHER"}</div>
          <div className={`avail ${p.isAvailable ? "avail-yes" : "avail-no"}`}>
            {p.isAvailable ? "In Stock" : "Out of Stock"}
          </div>
        </div>

        <div className="card-name" onClick={() => onOpen(p.id)} role="button" tabIndex={0}>
          {p.productName || "Unnamed product"}
        </div>

        <div className="card-spec">{p.specifications || "—"}</div>

        {/* Rating */}
        <div className="star-rating">
          {"★".repeat(Math.floor(rating))}{"☆".repeat(5 - Math.floor(rating))}
          <span className="count">({rating})</span>
        </div>

        <div className="price-row">
          <div className="price">{money(p.finalPrice ?? p.price)}</div>
          {p.discountPercentage ? <div className="strike">{money(p.price)}</div> : null}
        </div>
      </div>

      {/* Actions */}
      <div className="card-actions">
        <button
          className="btn btn-primary btn-wide"
          disabled={!p.isAvailable}
          onClick={() => onAddToCart?.(p.id)}
        >
          🛒 Add to Cart
        </button>
      </div>
    </div>
  );
}
