import React from "react";

function money(n) {
  const num = Number(n ?? 0);
  return new Intl.NumberFormat(undefined, { style: "currency", currency: "EUR" }).format(num);
}

export default function CartTable({ cart, onInc, onDec, onRemove }) {
  const items = cart?.items ?? [];

  if (!items.length) {
    return (
      <div className="panel" style={{ textAlign: "center", padding: 40 }}>
        <div style={{ fontSize: 48, marginBottom: 12 }}>🛒</div>
        <div className="panel-title">Your cart is empty</div>
        <div className="text-muted">Browse products and add items to start shopping.</div>
      </div>
    );
  }

  return (
    <div className="panel">
      <div className="panel-title">🛒 Items ({items.length})</div>

      <div className="table">
        <div className="trow thead">
          <div>Product</div>
          <div>Qty</div>
          <div>Unit Price</div>
          <div>Total</div>
          <div></div>
        </div>

        {items.map((it) => (
          <div key={it.itemId ?? it.productId} className="trow">
            <div>
              <div className="strong">{it.productName || `Product ${it.productId}`}</div>
              <div className="text-muted" style={{ fontSize: 12 }}>ID: {it.productId}</div>
            </div>

            <div className="qty">
              <button className="btn btn-mini btn-secondary" onClick={() => onDec(it.productId)}>−</button>
              <div className="strong" style={{ minWidth: 24, textAlign: "center" }}>{it.quantity}</div>
              <button className="btn btn-mini btn-secondary" onClick={() => onInc(it.productId)}>+</button>
            </div>

            <div className="strong">{money(it.unitPrice ?? 0)}</div>
            <div className="strong" style={{ color: "var(--accent)" }}>{money(it.lineTotal ?? 0)}</div>

            <div>
              <button className="btn btn-danger btn-mini" onClick={() => onRemove(it.productId)}>
                🗑️ Remove
              </button>
            </div>
          </div>
        ))}
      </div>

      <div className="row-between totalbar">
        <div className="strong" style={{ fontSize: 16 }}>Cart Total</div>
        <div className="strong" style={{ fontSize: 20, color: "var(--accent)" }}>{money(cart?.cartTotal ?? 0)}</div>
      </div>
    </div>
  );
}
