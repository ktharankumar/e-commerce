import React from "react";
import ProductCard from "./ProductCard.jsx";

export default function ProductGrid({ products, onOpen, onAddToCart }) {
  if (!products.length) {
    return (
      <div className="panel" style={{ textAlign: "center", padding: 40 }}>
        <div style={{ fontSize: 48, marginBottom: 12 }}>🔍</div>
        <div className="panel-title">No products found</div>
        <div className="text-muted">Try a different category or search term.</div>
      </div>
    );
  }

  return (
    <div className="grid">
      {products.map((p) => (
        <ProductCard key={p.id} p={p} onOpen={onOpen} onAddToCart={onAddToCart} />
      ))}
    </div>
  );
}
