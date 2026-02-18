import React, { useEffect, useMemo, useState } from "react";
import { CATEGORIES, getProduct, getProducts } from "../api/productApi";
import { addItem } from "../api/cartApi";
import { useAuth } from "../context/AuthContext.jsx";

import CategoryBar from "../components/CategoryBar.jsx";
import ProductGrid from "../components/ProductGrid.jsx";
import ProductModal from "../components/ProductModal.jsx";
import { SkeletonGrid } from "../components/Loader.jsx";

export default function ProductsPage({ setToast, initialCategory, initialSearch, onCartUpdate }) {
  const { isAuthenticated, user } = useAuth();
  const [category, setCategory] = useState(initialCategory || "");
  const [query, setQuery] = useState(initialSearch || "");
  const [items, setItems] = useState([]);
  const [loading, setLoading] = useState(true);
  const [err, setErr] = useState("");
  const [sortBy, setSortBy] = useState("default");

  const [selectedId, setSelectedId] = useState(null);
  const [selectedProduct, setSelectedProduct] = useState(null);
  const [modalLoading, setModalLoading] = useState(false);
  const [modalErr, setModalErr] = useState("");

  // Update category from header dropdown
  useEffect(() => {
    if (initialCategory !== undefined) setCategory(initialCategory);
  }, [initialCategory]);

  useEffect(() => {
    if (initialSearch !== undefined) setQuery(initialSearch);
  }, [initialSearch]);

  async function loadProducts() {
    setErr("");
    setLoading(true);
    try {
      const data = await getProducts(category || undefined);
      // Backend returns a Page object: { content: [...], pageable: ... }
      const list = data?.content || data;
      setItems(Array.isArray(list) ? list : []);
    } catch (e) {
      setErr(e.message || "Failed to load products");
      setItems([]);
    } finally {
      setLoading(false);
    }
  }

  useEffect(() => {
    loadProducts();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [category]);

  const filtered = useMemo(() => {
    const q = query.trim().toLowerCase();
    let result = items;
    if (q) {
      result = result.filter((p) => {
        const name = (p.productName || "").toLowerCase();
        const spec = (p.specifications || "").toLowerCase();
        return name.includes(q) || spec.includes(q);
      });
    }

    // Sort
    if (sortBy === "price-asc") result = [...result].sort((a, b) => (a.finalPrice ?? a.price) - (b.finalPrice ?? b.price));
    if (sortBy === "price-desc") result = [...result].sort((a, b) => (b.finalPrice ?? b.price) - (a.finalPrice ?? a.price));
    if (sortBy === "name") result = [...result].sort((a, b) => (a.productName || "").localeCompare(b.productName || ""));

    return result;
  }, [items, query, sortBy]);

  async function openProduct(id) {
    setSelectedId(id);
    setSelectedProduct(null);
    setModalErr("");
    setModalLoading(true);
    try {
      const p = await getProduct(id);
      setSelectedProduct(p);
    } catch (e) {
      setModalErr(e.message || "Failed to load product");
    } finally {
      setModalLoading(false);
    }
  }

  async function addToCartQuick(productId) {
    const userId = user?.id || 1;
    try {
      await addItem(userId, productId, 1);
      setToast?.({ type: "success", title: "Added to cart", message: `Product added successfully` });
      onCartUpdate?.();
    } catch (e) {
      setToast?.({ type: "error", title: "Add to cart failed", message: e.message });
    }
  }

  return (
    <>
      {/* Category Bar */}
      <CategoryBar categories={CATEGORIES} selected={category} onSelect={setCategory} />

      <main className="container" style={{ paddingTop: 24, paddingBottom: 40 }}>
        {/* Toolbar */}
        <div className="grid-toolbar">
          <div>
            <div className="title" style={{ fontSize: 20 }}>
              {category ? category.replace(/_/g, " ") : "All Products"}
            </div>
            <div className="grid-count">
              {loading ? "Loading..." : `${filtered.length} product${filtered.length !== 1 ? "s" : ""} found`}
            </div>
          </div>

          <div className="row" style={{ gap: 12 }}>
            <input
              className="search-input"
              value={query}
              onChange={(e) => setQuery(e.target.value)}
              placeholder="🔍 Filter by name or specs..."
              style={{ maxWidth: 280 }}
            />
            <select className="sort-select" value={sortBy} onChange={(e) => setSortBy(e.target.value)}>
              <option value="default">Sort: Default</option>
              <option value="price-asc">Price: Low → High</option>
              <option value="price-desc">Price: High → Low</option>
              <option value="name">Name: A → Z</option>
            </select>
          </div>
        </div>

        {/* Error / Loading / Grid */}
        {err ? (
          <div className="panel" style={{ textAlign: "center", padding: 40 }}>
            <div style={{ fontSize: 48, marginBottom: 12 }}>⚠️</div>
            <div className="panel-title">Couldn't load products</div>
            <div className="text-muted" style={{ marginBottom: 16 }}>{err}</div>
            <button className="btn btn-primary" onClick={loadProducts}>Retry</button>
          </div>
        ) : loading ? (
          <SkeletonGrid count={8} />
        ) : (
          <ProductGrid products={filtered} onOpen={openProduct} onAddToCart={addToCartQuick} />
        )}
      </main>

      <ProductModal
        open={!!selectedId}
        onClose={() => setSelectedId(null)}
        loading={modalLoading}
        error={modalErr}
        product={selectedProduct}
        onAddToCart={selectedProduct ? () => addToCartQuick(selectedProduct.id) : null}
      />
    </>
  );
}
