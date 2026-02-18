import React, { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { getCart } from "../api/cartApi";
import { createOrderFromCart } from "../api/orderApi";
import { createPaymentIntent } from "../api/paymentApi";
import { useAuth } from "../context/AuthContext.jsx";
import Loader from "../components/Loader.jsx";

function money(n) {
  const num = Number(n ?? 0);
  return new Intl.NumberFormat(undefined, { style: "currency", currency: "EUR" }).format(num);
}

export default function CheckoutPage({ setToast }) {
  const nav = useNavigate();
  const { isAuthenticated, user } = useAuth();
  const userId = user?.id || 1;
  const [cart, setCart] = useState(null);
  const [loading, setLoading] = useState(true);
  const [err, setErr] = useState("");

  useEffect(() => {
    if (!isAuthenticated) {
      nav("/login");
      return;
    }
    (async () => {
      setLoading(true);
      setErr("");
      try {
        const c = await getCart(userId);
        setCart(c);
      } catch (e) {
        setErr(e.message || "Failed to load cart");
      } finally {
        setLoading(false);
      }
    })();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  async function proceed() {
    try {
      const order = createOrderFromCart({ userId, cart });
      const payment = createPaymentIntent({ orderId: order.orderId, amount: order.total });
      nav("/payment", { state: { order, payment } });
    } catch (e) {
      setToast?.({ type: "error", title: "Checkout failed", message: e.message });
    }
  }

  if (loading) return <Loader label="Preparing checkout..." />;

  if (err) {
    return (
      <main className="container" style={{ paddingTop: 24 }}>
        <div className="panel" style={{ textAlign: "center", padding: 40 }}>
          <div style={{ fontSize: 48, marginBottom: 12 }}>⚠️</div>
          <div className="panel-title">Checkout failed</div>
          <div className="text-muted" style={{ marginBottom: 16 }}>{err}</div>
          <button className="btn btn-primary" onClick={() => nav("/cart")}>Back to cart</button>
        </div>
      </main>
    );
  }

  const items = cart?.items ?? [];
  const total = cart?.cartTotal ?? 0;

  return (
    <main className="container" style={{ paddingTop: 24, paddingBottom: 40 }}>
      <div className="page-head row-between">
        <div>
          <div className="title">📋 Checkout</div>
          <div className="text-muted">Review your order and proceed to payment</div>
        </div>
        <button className="btn btn-ghost" onClick={() => nav("/cart")}>← Back to cart</button>
      </div>

      <div className="grid-2">
        <div className="panel">
          <div className="panel-title">Order Items</div>
          {items.length ? (
            <div className="list">
              {items.map((it) => (
                <div key={it.itemId ?? it.productId} className="list-row">
                  <div>
                    <div className="strong">{it.productName || `Product ${it.productId}`}</div>
                    <div className="text-muted" style={{ fontSize: 13 }}>Qty: {it.quantity}</div>
                  </div>
                  <div className="strong" style={{ color: "var(--accent)" }}>{money(it.totalPrice ?? 0)}</div>
                </div>
              ))}
            </div>
          ) : (
            <div className="text-muted">Cart is empty.</div>
          )}
        </div>

        <div className="panel">
          <div className="panel-title">Summary</div>
          <div className="list">
            <div className="list-row">
              <div className="text-muted">Buyer</div>
              <div className="strong">{user?.username || "Guest"}</div>
            </div>
            <div className="list-row">
              <div className="text-muted">Cart Total</div>
              <div className="strong">{money(total)}</div>
            </div>
            <div className="list-row">
              <div className="text-muted">Shipping</div>
              <div className="strong" style={{ color: "var(--success)" }}>Free</div>
            </div>
            <div className="divider" />
            <div className="list-row">
              <div className="strong" style={{ fontSize: 16 }}>Payable</div>
              <div className="strong" style={{ fontSize: 20, color: "var(--accent)" }}>{money(total)}</div>
            </div>
          </div>

          <button className="btn btn-primary btn-wide" onClick={proceed} disabled={!items.length} style={{ marginTop: 16, padding: 14 }}>
            💳 Proceed to Payment
          </button>

          <div className="hint">
            Note: Payment/Order services are simulated — no real charges will be made.
          </div>
        </div>
      </div>
    </main>
  );
}
