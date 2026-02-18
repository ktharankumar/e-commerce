import React from "react";
import { useLocation, useNavigate } from "react-router-dom";

export default function OrderSuccessPage() {
  const nav = useNavigate();
  const { state } = useLocation();
  const order = state?.order;
  const payment = state?.payment;

  return (
    <main className="container" style={{ paddingTop: 40, paddingBottom: 40 }}>
      <div className="panel success" style={{ maxWidth: 520, margin: "0 auto", textAlign: "center", padding: 40 }}>
        <div className="success-icon">🎉</div>
        <div className="panel-title" style={{ fontSize: 24, marginBottom: 4 }}>Order Confirmed!</div>
        <div className="text-muted" style={{ marginBottom: 20 }}>Thank you for shopping with Shoppers Stop</div>

        <div className="kv" style={{ textAlign: "left", maxWidth: 300, margin: "0 auto" }}>
          <div className="k">Order ID</div><div className="v">{order?.orderId ?? "—"}</div>
          <div className="k">Payment</div><div className="v">{payment?.paymentId ?? "—"}</div>
          <div className="k">Status</div>
          <div className="v">
            <span style={{
              padding: "4px 12px",
              borderRadius: "var(--radius-full)",
              background: "var(--success-bg)",
              color: "var(--success)",
              fontWeight: 700,
              fontSize: 13
            }}>
              {payment?.status ?? "—"}
            </span>
          </div>
        </div>

        <div className="divider" style={{ margin: "20px 0" }} />

        <div className="row" style={{ justifyContent: "center" }}>
          <button className="btn btn-primary" onClick={() => nav("/")}>🛍️ Continue Shopping</button>
          <button className="btn btn-ghost" onClick={() => nav("/cart")}>🛒 Go to Cart</button>
        </div>
      </div>
    </main>
  );
}
