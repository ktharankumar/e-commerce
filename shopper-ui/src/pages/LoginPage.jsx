import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { login as loginApi } from "../api/authApi";
import { useAuth } from "../context/AuthContext.jsx";
import { encryptPassword } from "../api/crypto";

export default function LoginPage({ setToast }) {
    const [form, setForm] = useState({ username: "", password: "" });
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState("");
    const nav = useNavigate();
    const { login } = useAuth();

    function set(field) {
        return (e) => setForm((f) => ({ ...f, [field]: e.target.value }));
    }

    async function handleSubmit(e) {
        e.preventDefault();
        setError("");
        setLoading(true);
        try {
            const encryptedPassword = await encryptPassword(form.password);
            const res = await loginApi(form.username, encryptedPassword);
            login(res);
            setToast?.({ type: "success", title: "Welcome back!", message: `Signed in as ${res.username}` });
            nav("/");
        } catch (err) {
            setError(err.message || "Login failed. Please check your credentials.");
        } finally {
            setLoading(false);
        }
    }

    return (
        <div className="auth-page">
            <div className="auth-card">
                <div className="auth-header">
                    <div className="auth-logo">S</div>
                    <div className="auth-title">Welcome Back</div>
                    <div className="auth-subtitle">Sign in to your Shoppers Stop account</div>
                </div>

                {error && (
                    <div className="panel" style={{ borderColor: "rgba(239,68,68,0.3)", marginBottom: 20, padding: 12 }}>
                        <div className="text-muted" style={{ color: "var(--danger)", fontSize: 13 }}>❌ {error}</div>
                    </div>
                )}

                <form className="auth-form" onSubmit={handleSubmit}>
                    <div className="form-group">
                        <label className="form-label" htmlFor="login-user">Username</label>
                        <input
                            id="login-user"
                            className="form-input"
                            value={form.username}
                            onChange={set("username")}
                            placeholder="Enter your username"
                            required
                            autoFocus
                        />
                    </div>

                    <div className="form-group">
                        <label className="form-label" htmlFor="login-pass">Password</label>
                        <input
                            id="login-pass"
                            className="form-input"
                            type="password"
                            value={form.password}
                            onChange={set("password")}
                            placeholder="Enter your password"
                            required
                        />
                    </div>

                    <button className="btn btn-primary btn-wide" type="submit" disabled={loading} style={{ padding: 14, fontSize: 15 }}>
                        {loading ? "Signing in..." : "Sign In"}
                    </button>
                </form>

                <div className="auth-footer">
                    Don't have an account?{" "}
                    <span className="auth-link" onClick={() => nav("/register")}>Create one</span>
                </div>
            </div>
        </div>
    );
}
