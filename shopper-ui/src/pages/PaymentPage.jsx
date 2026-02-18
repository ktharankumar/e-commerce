import React from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { confirmPayment } from "../api/paymentApi";

function money(n) {
  const num = Number(n ?? 0);
  return new Intl.NumberFormat(undefined, { style: "currency", currency: "EUR" }).format(num);
}

export default function PaymentPage({ setToast }) {
  const nav = useNavigate();
  const { state } = useLocation();

  const order = state?.order;
  const payment = state?.payment;

  if (!order || !payment) {
    return (
      <main className="container" style={{ paddingTop: 24 }}>
        <div className="panel" style={{ textAlign: "center", padding: 40 }}>
          <div style={{ fontSize: 48, marginBottom: 12 }}>🔒</div>
          <div className="panel-title">No payment context</div>
          <div className="text-muted" style={{ marginBottom: 16 }}>Please go through checkout first.</div>
          <button className="btn btn-primary" onClick={() => nav("/checkout")}>Go to Checkout</button>
        </div>
      </main>
    );
  }

  async function payNow() {
    try {
      const res = confirmPayment({ paymentId: payment.paymentId });
      if (res.status === "SUCCESS") {
        nav("/success", { state: { order, payment: { ...payment, status: "SUCCESS" } } });
      } else {
        setToast?.({ type: "error", title: "Payment failed", message: "Please try again" });
      }
    } catch (e) {
      setToast?.({ type: "error", title: "Payment failed", message: e.message });
    }
  }

  return (
    <main className="container" style={{ paddingTop: 24, paddingBottom: 40 }}>
      <div className="page-head row-between">
        <div>
          <div className="title">💳 Payment</div>
          <div className="text-muted">Complete your purchase</div>
        </div>
        <button className="btn btn-ghost" onClick={() => nav("/checkout")}>← Back</button>
      </div>

      <div className="panel" style={{ maxWidth: 500 }}>
        <div className="panel-title">Order Summary</div>

        <div className="kv">
          <div className="k">Order ID</div><div className="v">{order.orderId}</div>
          <div className="k">Payment ID</div><div className="v">{payment.paymentId}</div>
          <div className="k">Amount</div><div className="v strong" style={{ color: "var(--accent)", fontSize: 20 }}>{money(payment.amount)}</div>
          <div className="k">Status</div>
          <div className="v">
            <span style={{ padding: "4px 10px", borderRadius: "var(--radius-full)", background: "var(--accent-glow)", color: "var(--accent)", fontWeight: 700, fontSize: 13 }}>
              {payment.status}
            </span>
          </div>
        </div>

        <button className="btn btn-primary btn-wide" onClick={payNow} style={{ padding: 14, fontSize: 16, marginTop: 8 }}>
          ✨ Pay Now
        </button>

        <div className="hint">
          This is a simulated payment — no real charges. A real payment gateway (Stripe, Razorpay) can replace this later.
        </div>
      </div>
    </main>
  );
}
