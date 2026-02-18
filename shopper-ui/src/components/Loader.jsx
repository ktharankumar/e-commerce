import React from "react";

export default function Loader({ label }) {
  return (
    <div className="loader">
      <div className="spinner" />
      <div className="text-muted">{label || "Loading..."}</div>
    </div>
  );
}

export function SkeletonGrid({ count = 8 }) {
  return (
    <div className="grid">
      {Array.from({ length: count }).map((_, i) => (
        <div key={i} className="skeleton skeleton-card" />
      ))}
    </div>
  );
}
