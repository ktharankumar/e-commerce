import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { register } from "../api/authApi";
import { encryptPassword } from "../api/crypto";

export default function RegisterPage({ setToast }) {
    const [form, setForm] = useState({
        userName: "",
        email: "",
        password: "",
        confirmPassword: "",
        firstName: "",
        lastName: "",
        role: "USER",
    });
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState("");
    const nav = useNavigate();

    function set(field) {
        return (e) => setForm((f) => ({ ...f, [field]: e.target.value }));
    }

    async function handleSubmit(e) {
        e.preventDefault();
        setError("");

        // Validate passwords match
        if (form.password !== form.confirmPassword) {
            setError("Passwords do not match. Please try again.");
            return;
        }

        setLoading(true);
        try {
            const { confirmPassword, ...payload } = form;
            const encryptedPassword = await encryptPassword(payload.password);
            await register({ ...payload, password: encryptedPassword });
            setToast?.({ type: "success", title: "Account created!", message: "You can now sign in." });
            nav("/login");
        } catch (err) {
            setError(err.message || "Registration failed. Please try again.");
        } finally {
            setLoading(false);
        }
    }

    return (
        <div className="auth-page">
            <div className="auth-card" style={{ maxWidth: 760, padding: "32px" }}>
                <div className="auth-header">
                    <div className="auth-logo">S</div>
                    <div className="auth-title">Create Account</div>
                    <div className="auth-subtitle">Join Shoppers Stop for a premium shopping experience</div>
                </div>

                {error && (
                    <div className="panel" style={{ borderColor: "rgba(239,68,68,0.3)", marginBottom: 20, padding: 12 }}>
                        <div className="text-muted" style={{ color: "var(--danger)", fontSize: 13 }}>❌ {error}</div>
                    </div>
                )}

                <form className="auth-form" onSubmit={handleSubmit}>
                    <div className="auth-form-grid">
                        <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
                            <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16 }}>
                                <div className="form-group">
                                    <label className="form-label" htmlFor="reg-first">First Name</label>
                                    <input
                                        id="reg-first"
                                        className="form-input"
                                        value={form.firstName}
                                        onChange={set("firstName")}
                                        placeholder="John"
                                        required
                                    />
                                </div>
                                <div className="form-group">
                                    <label className="form-label" htmlFor="reg-last">Last Name</label>
                                    <input
                                        id="reg-last"
                                        className="form-input"
                                        value={form.lastName}
                                        onChange={set("lastName")}
                                        placeholder="Doe"
                                        required
                                    />
                                </div>
                            </div>

                            <div className="form-group">
                                <label className="form-label" htmlFor="reg-user">Username</label>
                                <input
                                    id="reg-user"
                                    className="form-input"
                                    value={form.userName}
                                    onChange={set("userName")}
                                    placeholder="Choose a username"
                                    required
                                />
                            </div>

                            <div className="form-group">
                                <label className="form-label" htmlFor="reg-email">Email</label>
                                <input
                                    id="reg-email"
                                    className="form-input"
                                    type="email"
                                    value={form.email}
                                    onChange={set("email")}
                                    placeholder="john@example.com"
                                    required
                                />
                            </div>
                        </div>

                        <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
                            <div className="form-group">
                                <label className="form-label">Register as</label>
                                <div style={{ display: "flex", gap: 12, marginTop: 4 }}>
                                    <button
                                        type="button"
                                        onClick={() => setForm(f => ({ ...f, role: "USER" }))}
                                        style={{
                                            flex: 1,
                                            padding: "10px 16px",
                                            borderRadius: 8,
                                            border: form.role === "USER" ? "2px solid var(--accent)" : "1px solid rgba(255,255,255,0.1)",
                                            background: form.role === "USER" ? "rgba(99,102,241,0.15)" : "rgba(255,255,255,0.03)",
                                            color: form.role === "USER" ? "var(--accent)" : "var(--text-muted)",
                                            cursor: "pointer",
                                            fontSize: 14,
                                            fontWeight: form.role === "USER" ? 600 : 400,
                                            transition: "all 0.2s ease",
                                        }}
                                    >
                                        🛒 Buyer
                                    </button>
                                    <button
                                        type="button"
                                        onClick={() => setForm(f => ({ ...f, role: "ADMIN" }))}
                                        style={{
                                            flex: 1,
                                            padding: "10px 16px",
                                            borderRadius: 8,
                                            border: form.role === "ADMIN" ? "2px solid var(--accent)" : "1px solid rgba(255,255,255,0.1)",
                                            background: form.role === "ADMIN" ? "rgba(99,102,241,0.15)" : "rgba(255,255,255,0.03)",
                                            color: form.role === "ADMIN" ? "var(--accent)" : "var(--text-muted)",
                                            cursor: "pointer",
                                            fontSize: 14,
                                            fontWeight: form.role === "ADMIN" ? 600 : 400,
                                            transition: "all 0.2s ease",
                                        }}
                                    >
                                        🏪 Seller
                                    </button>
                                </div>
                            </div>

                            <div className="form-group">
                                <label className="form-label" htmlFor="reg-pass">Password</label>
                                <input
                                    id="reg-pass"
                                    className="form-input"
                                    type="password"
                                    value={form.password}
                                    onChange={set("password")}
                                    placeholder="Choose a strong password"
                                    required
                                    minLength={6}
                                />
                            </div>

                            <div className="form-group">
                                <label className="form-label" htmlFor="reg-confirm-pass">Confirm Password</label>
                                <input
                                    id="reg-confirm-pass"
                                    className="form-input"
                                    type="password"
                                    value={form.confirmPassword}
                                    onChange={set("confirmPassword")}
                                    placeholder="Re-enter your password"
                                    required
                                    minLength={6}
                                />
                                {form.confirmPassword && form.password !== form.confirmPassword && (
                                    <div style={{ color: "var(--danger)", fontSize: 12, marginTop: 4 }}>
                                        ⚠️ Passwords do not match
                                    </div>
                                )}
                            </div>
                        </div>
                    </div>

                    <button className="btn btn-primary btn-wide" type="submit" disabled={loading} style={{ padding: 14, fontSize: 15, marginTop: 12 }}>
                        {loading ? "Creating account..." : "Create Account"}
                    </button>
                </form>

                <div className="auth-footer">
                    Already have an account?{" "}
                    <span className="auth-link" onClick={() => nav("/login")}>Sign in</span>
                </div>
            </div>
        </div>
    );
}

