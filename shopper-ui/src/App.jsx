import React, { useEffect, useState, useCallback } from "react";
import { Routes, Route, useNavigate, useLocation } from "react-router-dom";
import { useAuth } from "./context/AuthContext.jsx";
import { getCart } from "./api/cartApi";

import Header from "./components/Header.jsx";
import Toast from "./components/Toast.jsx";

import ProductsPage from "./pages/ProductsPage.jsx";
import CartPage from "./pages/CartPage.jsx";
import CheckoutPage from "./pages/CheckoutPage.jsx";
import PaymentPage from "./pages/PaymentPage.jsx";
import OrderSuccessPage from "./pages/OrderSuccessPage.jsx";
import CreateProductPage from "./pages/CreateProductPage.jsx";
import LoginPage from "./pages/LoginPage.jsx";
import RegisterPage from "./pages/RegisterPage.jsx";

export default function App() {
  const [toast, setToast] = useState(null);
  const [cartCount, setCartCount] = useState(0);
  const [headerCategory, setHeaderCategory] = useState(undefined);
  const [headerSearch, setHeaderSearch] = useState(undefined);
  const nav = useNavigate();
  const loc = useLocation();
  const { user } = useAuth();

  useEffect(() => {
    setToast(null);
  }, [loc.pathname]);

  // Fetch cart count
  const refreshCartCount = useCallback(async () => {
    const userId = user?.id || 1;
    try {
      const cart = await getCart(userId);
      setCartCount(cart?.items?.length ?? 0);
    } catch {
      setCartCount(0);
    }
  }, [user?.id]);

  useEffect(() => {
    refreshCartCount();
  }, [refreshCartCount]);

  function handleGoProducts(category, search) {
    setHeaderCategory(category ?? "");
    setHeaderSearch(search ?? "");
    nav("/");
  }

  return (
    <div className="app">
      <Header
        onGoProducts={handleGoProducts}
        onGoCart={() => nav("/cart")}
        onGoCreate={() => nav("/create-product")}
        onGoLogin={() => nav("/login")}
        onGoRegister={() => nav("/register")}
        cartCount={cartCount}
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
        <Route
          path="/"
          element={
            <ProductsPage
              setToast={setToast}
              initialCategory={headerCategory}
              initialSearch={headerSearch}
              onCartUpdate={refreshCartCount}
            />
          }
        />
        <Route path="/create-product" element={<CreateProductPage setToast={setToast} />} />
        <Route path="/cart" element={<CartPage setToast={setToast} onCartUpdate={refreshCartCount} />} />
        <Route path="/checkout" element={<CheckoutPage setToast={setToast} />} />
        <Route path="/payment" element={<PaymentPage setToast={setToast} />} />
        <Route path="/success" element={<OrderSuccessPage />} />
        <Route path="/login" element={<LoginPage setToast={setToast} />} />
        <Route path="/register" element={<RegisterPage setToast={setToast} />} />
      </Routes>
    </div>
  );
}
