import React, { createContext, useContext, useState, useEffect } from "react";

const AuthContext = createContext(null);

function parseJwt(token) {
    try {
        const base64Url = token.split(".")[1];
        const base64 = base64Url.replace(/-/g, "+").replace(/_/g, "/");
        const json = decodeURIComponent(
            atob(base64)
                .split("")
                .map((c) => "%" + ("00" + c.charCodeAt(0).toString(16)).slice(-2))
                .join("")
        );
        return JSON.parse(json);
    } catch {
        return null;
    }
}

export function AuthProvider({ children }) {
    const [token, setToken] = useState(() => localStorage.getItem("jwt"));
    const [user, setUser] = useState(() => {
        const t = localStorage.getItem("jwt");
        if (!t) return null;
        const stored = localStorage.getItem("user");
        return stored ? JSON.parse(stored) : null;
    });

    function login(jwtResponse) {
        // jwtResponse = { token, id, username, email, roles }
        const { token: jwt, id, username, email, roles } = jwtResponse;
        localStorage.setItem("jwt", jwt);
        const userObj = { id, username, email, roles };
        localStorage.setItem("user", JSON.stringify(userObj));
        setToken(jwt);
        setUser(userObj);
    }

    function logout() {
        localStorage.removeItem("jwt");
        localStorage.removeItem("user");
        setToken(null);
        setUser(null);
    }

    // Check token expiry
    useEffect(() => {
        if (!token) return;
        const payload = parseJwt(token);
        if (!payload || !payload.exp) return;
        const expiresMs = payload.exp * 1000 - Date.now();
        if (expiresMs <= 0) {
            logout();
            return;
        }
        const timer = setTimeout(logout, expiresMs);
        return () => clearTimeout(timer);
    }, [token]);

    const isAuthenticated = !!token && !!user;

    return (
        <AuthContext.Provider value={{ token, user, isAuthenticated, login, logout }}>
            {children}
        </AuthContext.Provider>
    );
}

export function useAuth() {
    const ctx = useContext(AuthContext);
    if (!ctx) throw new Error("useAuth must be used within AuthProvider");
    return ctx;
}
