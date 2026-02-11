import React from "react";

export default function Header({ onGoProducts, onGoCart, onGoCreate }) {
  return (
    <header className="header">
      <div className="brand" onClick={onGoProducts} role="button" tabIndex={0}>
        <div className="logo">S</div>
        <div>
          <div className="brand-name">Shopper MVP</div>
          <div className="brand-sub">Products → Cart → Checkout → Payment</div>
        </div>
      </div>

      <div className="row">
        <button className="btn btn-ghost" onClick={onGoProducts}>Products</button>
        <button className="btn btn-ghost" onClick={onGoCreate}>+ Add Product</button>
        <button className="btn" onClick={onGoCart}>Cart</button>
      </div>
    </header>
  );
}
