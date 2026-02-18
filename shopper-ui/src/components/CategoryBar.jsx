import React from "react";
import { CATEGORY_ICONS } from "../api/productApi";

export default function CategoryBar({ categories, selected, onSelect }) {
  return (
    <div className="categorybar">
      <div className="container" style={{ display: "flex", gap: 8, paddingTop: 10, paddingBottom: 10 }}>
        <button
          className={`pill ${selected === "" ? "pill-active" : ""}`}
          onClick={() => onSelect("")}
        >
          🏷️ All
        </button>
        {categories.map((c) => (
          <button
            key={c}
            className={`pill ${selected === c ? "pill-active" : ""}`}
            onClick={() => onSelect(c)}
          >
            {CATEGORY_ICONS[c] || "📦"} {c.replaceAll("_", " ")}
          </button>
        ))}
      </div>
    </div>
  );
}
