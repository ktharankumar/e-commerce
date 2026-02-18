export const API_BASE = "/api";

async function request(method, path, body) {
  const headers = {
    "Content-Type": "application/json",
    Accept: "application/json",
  };

  // Attach JWT if available
  const jwt = localStorage.getItem("jwt");
  if (jwt) {
    headers["Authorization"] = `Bearer ${jwt}`;
  }

  const res = await fetch(`${API_BASE}${path}`, {
    method,
    headers,
    body: body ? JSON.stringify(body) : undefined,
  });

  if (!res.ok) {
    const text = await res.text().catch(() => "");
    throw new Error(`HTTP ${res.status}: ${text || res.statusText}`);
  }

  const ct = res.headers.get("content-type") || "";
  if (ct.includes("application/json")) return res.json();
  // Return text for non-JSON responses (like auth signup success message)
  const text = await res.text().catch(() => "");
  return text || null;
}

export const http = {
  get: (path) => request("GET", path),
  post: (path, body) => request("POST", path, body),
  patch: (path, body) => request("PATCH", path, body),
  del: (path) => request("DELETE", path),
};
