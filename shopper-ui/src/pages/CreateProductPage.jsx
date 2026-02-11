import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { CATEGORIES, createProduct } from "../api/productApi";

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
        <main className="container">
            <div className="page-head">
                <div className="title">Create New Product</div>
            </div>

            <form className="panel form-panel" onSubmit={handleSubmit}>
                <div className="form-grid">
                    {/* Product Name */}
                    <div className="form-group form-full">
                        <label className="form-label" htmlFor="cp-name">Product Name *</label>
                        <input
                            id="cp-name"
                            className="form-input"
                            value={form.productName}
                            onChange={set("productName")}
                            placeholder="e.g. Gaming Laptop"
                            required
                        />
                    </div>

                    {/* Price */}
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

                    {/* Category */}
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

                    {/* Discount */}
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

                    {/* Quantity */}
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

                    {/* Specifications */}
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
                    <button
                        type="button"
                        className="btn btn-ghost-dark"
                        onClick={() => nav("/")}
                    >
                        Cancel
                    </button>
                    <button className="btn btn-primary" type="submit" disabled={submitting}>
                        {submitting ? "Creating…" : "Create Product"}
                    </button>
                </div>
            </form>
        </main>
    );
}
