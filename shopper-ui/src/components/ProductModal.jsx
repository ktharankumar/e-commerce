import React from "react";
import Loader from "./Loader.jsx";
import { CATEGORY_ICONS } from "../api/productApi";

function money(n) {
  const num = Number(n ?? 0);
  return new Intl.NumberFormat(undefined, { style: "currency", currency: "EUR" }).format(num);
}

export default function ProductModal({ open, onClose, loading, error, product, onAddToCart }) {
  if (!open) return null;

  const icon = product ? (CATEGORY_ICONS[product.category] || "📦") : "📦";

  return (
    <div className="modal-backdrop" onMouseDown={onClose}>
      <div className="modal" onMouseDown={(e) => e.stopPropagation()}>
        <div className="modal-head">
          <div className="modal-title">Product Details</div>
          <button className="btn btn-ghost" onClick={onClose} style={{ padding: "6px 14px" }}>✕</button>
        </div>

        <div className="modal-body">
          {loading ? <Loader label="Loading product..." /> : null}
          {error ? (
            <div className="panel">
              <div className="panel-title" style={{ color: "var(--danger)" }}>⚠️ Error</div>
              <div className="text-muted">{error}</div>
            </div>
          ) : null}

          {!loading && !error && product ? (
            <>
              {/* Image Area */}
              <div
                className={`card-image-bg ${(product.category || "other").toLowerCase()}`}
                style={{ height: 180, borderRadius: "var(--radius-md)", fontSize: 64 }}
              >
                <span>{icon}</span>
              </div>

              <div className="row-between">
                <div className="badge">{product.category || "OTHER"}</div>
                <div className={`avail ${product.isAvailable ? "avail-yes" : "avail-no"}`}>
                  {product.isAvailable ? "In Stock" : "Out of Stock"}
                </div>
              </div>

              <h2 className="h2">{product.productName}</h2>
              <div className="text-muted">{product.specifications || "No specifications available."}</div>

              <div className="divider" />

              <div className="row-between">
                <div>
                  <div className="text-muted" style={{ fontSize: 13, marginBottom: 4 }}>Final Price</div>
                  <div className="modal-price">{money(product.finalPrice ?? product.price)}</div>
                </div>
                <div style={{ textAlign: "right" }}>
                  <div className="text-muted" style={{ fontSize: 13, marginBottom: 4 }}>Discount</div>
                  <div className="strong">{product.discountPercentage ? `${product.discountPercentage}%` : "—"}</div>
                </div>
              </div>

              {product.discountPercentage > 0 && (
                <div className="text-muted" style={{ fontSize: 13 }}>
                  Original price: <span className="strike">{money(product.price)}</span>
                </div>
              )}

              <div className="divider" />

              <button
                className="btn btn-primary btn-wide"
                disabled={!product.isAvailable}
                onClick={onAddToCart}
                style={{ padding: "14px 20px", fontSize: 16 }}
              >
                🛒 Add to Cart
              </button>
            </>
          ) : null}
        </div>
      </div>
    </div>
  );
}
