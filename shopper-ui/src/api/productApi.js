import { http } from "./http";

export const CATEGORIES = [
  "ELECTRONICS",
  "CLOTHING",
  "HOME_GOODS",
  "SHOES",
  "FOODS",
  "OTHER"
];

export function getProducts(category) {
  const qs = category ? `?category=${encodeURIComponent(category)}` : "";
  return http.get(`/v1/products${qs}`);
}

export function getProduct(id) {
  return http.get(`/v1/products/${id}`);
}

export function createProduct(data) {
  return http.post("/v1/products", data);
}
