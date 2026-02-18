import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { CATEGORIES, createProduct } from "../api/productApi";
import { useAuth } from "../context/AuthContext.jsx";

const EMPTY = {
    productName: "",
    price: "",
    category: "ELECTRONICS",
    discountPercentage: "0",
    specifications: "",
    availableQuantity: "1",
};

export default function CreateProductPage({ setToast }) {
    const [form, setForm] = useState({ ...EMPTY });
    const [submitting, setSubmitting] = useState(false);
    const nav = useNavigate();
    const { isAuthenticated, user } = useAuth();
    const isAdmin = isAuthenticated && user?.roles?.includes("ROLE_ADMIN");

    // Redirect if not authenticated or not admin
    if (!isAuthenticated) {
        return (
            <main className="container" style={{ paddingTop: 24 }}>
                <div className="panel" style={{ textAlign: "center", padding: 40 }}>
                    <div style={{ fontSize: 48, marginBottom: 12 }}>🔒</div>
                    <div className="panel-title">Authentication Required</div>
                    <div className="text-muted" style={{ marginBottom: 16 }}>You need to sign in to add products.</div>
                    <button className="btn btn-primary" onClick={() => nav("/login")}>Sign In</button>
                </div>
            </main>
        );
    }

    if (!isAdmin) {
        return (
            <main className="container" style={{ paddingTop: 24 }}>
                <div className="panel" style={{ textAlign: "center", padding: 40 }}>
                    <div style={{ fontSize: 48, marginBottom: 12 }}>🚫</div>
                    <div className="panel-title">Not Authorized</div>
                    <div className="text-muted" style={{ marginBottom: 16 }}>Only administrators can add products.</div>
                    <button className="btn btn-primary" onClick={() => nav("/")}>Back to Products</button>
                </div>
            </main>
        );
    }

    function set(field) {
        return (e) => setForm((f) => ({ ...f, [field]: e.target.value }));
    }

    async function handleSubmit(e) {
        e.preventDefault();
        setSubmitting(true);
        try {
            const payload = {
                productName: form.productName.trim(),
                price: Number(form.price),
                category: form.category,
                discountPercentage: Number(form.discountPercentage) || 0,
                specifications: form.specifications.trim(),
                availableQuantity: Number(form.availableQuantity),
            };
            await createProduct(payload);
            setToast?.({
                type: "success",
                title: "Product created",
                message: `"${payload.productName}" added successfully`,
            });
            nav("/");
        } catch (err) {
            setToast?.({
                type: "error",
                title: "Failed to create product",
                message: err.message,
            });
        } finally {
            setSubmitting(false);
        }
    }

    return (
        <main className="container" style={{ paddingTop: 24, paddingBottom: 40 }}>
            <div className="page-head">
                <div className="title">➕ Create New Product</div>
                <div className="text-muted" style={{ marginTop: 4 }}>Add a new product to the catalogue</div>
            </div>

            <form className="panel form-panel" onSubmit={handleSubmit}>
                <div className="form-grid">
                    <div className="form-group form-full">
                        <label className="form-label" htmlFor="cp-name">Product Name *</label>
                        <input
                            id="cp-name"
                            className="form-input"
                            value={form.productName}
                            onChange={set("productName")}
                            placeholder="e.g. Gaming Laptop"
                            required
                            autoFocus
                        />
                    </div>

                    <div className="form-group">
                        <label className="form-label" htmlFor="cp-price">Price (€) *</label>
                        <input
                            id="cp-price"
                            className="form-input"
                            type="number"
                            min="0"
                            step="0.01"
                            value={form.price}
                            onChange={set("price")}
                            placeholder="300"
                            required
                        />
                    </div>

                    <div className="form-group">
                        <label className="form-label" htmlFor="cp-cat">Category</label>
                        <select
                            id="cp-cat"
                            className="form-input"
                            value={form.category}
                            onChange={set("category")}
                        >
                            {CATEGORIES.map((c) => (
                                <option key={c} value={c}>
                                    {c.replace(/_/g, " ")}
                                </option>
                            ))}
                        </select>
                    </div>

                    <div className="form-group">
                        <label className="form-label" htmlFor="cp-disc">Discount %</label>
                        <input
                            id="cp-disc"
                            className="form-input"
                            type="number"
                            min="0"
                            max="100"
                            value={form.discountPercentage}
                            onChange={set("discountPercentage")}
                            placeholder="0"
                        />
                    </div>

                    <div className="form-group">
                        <label className="form-label" htmlFor="cp-qty">Available Quantity *</label>
                        <input
                            id="cp-qty"
                            className="form-input"
                            type="number"
                            min="1"
                            value={form.availableQuantity}
                            onChange={set("availableQuantity")}
                            placeholder="100"
                            required
                        />
                    </div>

                    <div className="form-group form-full">
                        <label className="form-label" htmlFor="cp-spec">Specifications</label>
                        <textarea
                            id="cp-spec"
                            className="form-input form-textarea"
                            value={form.specifications}
                            onChange={set("specifications")}
                            placeholder="e.g. 4th Gen, i5, 16 GB RAM"
                            rows={3}
                        />
                    </div>
                </div>

                <div className="form-actions">
                    <button type="button" className="btn btn-ghost" onClick={() => nav("/")}>
                        Cancel
                    </button>
                    <button className="btn btn-primary" type="submit" disabled={submitting}>
                        {submitting ? "Creating…" : "✨ Create Product"}
                    </button>
                </div>
            </form>
        </main>
    );
}
