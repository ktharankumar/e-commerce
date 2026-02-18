import React, { useEffect } from "react";

export default function Toast({ title, message, type, onClose }) {
  useEffect(() => {
    const timer = setTimeout(onClose, 4000);
    return () => clearTimeout(timer);
  }, [onClose]);

  const icon = type === "success" ? "✅" : type === "error" ? "❌" : "ℹ️";

  return (
    <div className={`toast toast-${type}`}>
      <div className="toast-head">
        <div className="toast-title">
          <span>{icon}</span> {title}
        </div>
        <button className="btn btn-mini btn-ghost" onClick={onClose} style={{ padding: "4px 8px" }}>✕</button>
      </div>
      <div className="text-muted" style={{ fontSize: 13 }}>{message}</div>
      <div className="toast-progress">
        <div className="toast-progress-bar" />
      </div>
    </div>
  );
}
