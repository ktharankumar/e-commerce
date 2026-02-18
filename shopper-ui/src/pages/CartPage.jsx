import React, { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { getCart, addItem, reduceItem, removeItem } from "../api/cartApi";
import { useAuth } from "../context/AuthContext.jsx";
import CartTable from "../components/CartTable.jsx";
import Loader from "../components/Loader.jsx";

export default function CartPage({ setToast, onCartUpdate }) {
  const nav = useNavigate();
  const { user } = useAuth();
  const userId = user?.id || 1;
  const [cart, setCart] = useState(null);
  const [loading, setLoading] = useState(true);
  const [err, setErr] = useState("");

  async function load() {
    setLoading(true);
    setErr("");
    try {
      const c = await getCart(userId);
      setCart(c);
    } catch (e) {
      setErr(e.message || "Failed to load cart");
      setCart(null);
    } finally {
      setLoading(false);
    }
  }

  useEffect(() => {
    load();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [userId]);

  async function inc(productId) {
    try {
      await addItem(userId, productId, 1);
      await load();
      onCartUpdate?.();
    } catch (e) {
      setToast?.({ type: "error", title: "Update failed", message: e.message });
    }
  }

  async function dec(productId) {
    try {
      await reduceItem(userId, productId, 1);
      await load();
      onCartUpdate?.();
    } catch (e) {
      setToast?.({ type: "error", title: "Update failed", message: e.message });
    }
  }

  async function remove(productId) {
    try {
      await removeItem(userId, productId);
      await load();
      onCartUpdate?.();
      setToast?.({ type: "success", title: "Removed", message: `Product removed from cart` });
    } catch (e) {
      setToast?.({ type: "error", title: "Remove failed", message: e.message });
    }
  }

  return (
    <main className="container" style={{ paddingTop: 24, paddingBottom: 40 }}>
      <div className="page-head row-between">
        <div>
          <div className="title">🛒 Your Cart</div>
          <div className="text-muted">{user ? `Logged in as ${user.username}` : "Guest"}</div>
        </div>
        <div className="row">
          <button className="btn btn-ghost" onClick={load}>🔄 Refresh</button>
          <button className="btn btn-primary" onClick={() => nav("/checkout")} disabled={!cart?.items?.length}>
            Checkout →
          </button>
        </div>
      </div>

      {err ? (
        <div className="panel" style={{ textAlign: "center", padding: 40 }}>
          <div style={{ fontSize: 48, marginBottom: 12 }}>⚠️</div>
          <div className="panel-title">Couldn't load cart</div>
          <div className="text-muted" style={{ marginBottom: 16 }}>{err}</div>
          <button className="btn btn-primary" onClick={load}>Retry</button>
        </div>
      ) : loading ? (
        <Loader label="Loading your cart..." />
      ) : (
        <CartTable cart={cart} onInc={inc} onDec={dec} onRemove={remove} />
      )}
    </main>
  );
}
