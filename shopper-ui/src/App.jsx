import React, { useEffect, useState } from "react";
import { Routes, Route, useNavigate, useLocation } from "react-router-dom";

import Header from "./components/Header.jsx";
import Toast from "./components/Toast.jsx";

import ProductsPage from "./pages/ProductsPage.jsx";
import CartPage from "./pages/CartPage.jsx";
import CheckoutPage from "./pages/CheckoutPage.jsx";
import PaymentPage from "./pages/PaymentPage.jsx";
import OrderSuccessPage from "./pages/OrderSuccessPage.jsx";
import CreateProductPage from "./pages/CreateProductPage.jsx";

export default function App() {
  const [toast, setToast] = useState(null);
  const nav = useNavigate();
  const loc = useLocation();

  useEffect(() => {
    // close toast on route change
    setToast(null);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [loc.pathname]);

  return (
    <div className="app">
      <Header
        onGoProducts={() => nav("/")}
        onGoCart={() => nav("/cart")}
        onGoCreate={() => nav("/create-product")}
      />

      {toast ? (
        <Toast
          title={toast.title}
          message={toast.message}
          type={toast.type}
          onClose={() => setToast(null)}
        />
      ) : null}

      <Routes>
        <Route path="/" element={<ProductsPage setToast={setToast} />} />
        <Route path="/create-product" element={<CreateProductPage setToast={setToast} />} />
        <Route path="/cart" element={<CartPage setToast={setToast} />} />
        <Route path="/checkout" element={<CheckoutPage setToast={setToast} />} />
        <Route path="/payment" element={<PaymentPage setToast={setToast} />} />
        <Route path="/success" element={<OrderSuccessPage />} />
      </Routes>
    </div>
  );
}
