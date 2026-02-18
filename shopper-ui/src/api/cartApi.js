import { http } from "./http";

export function getCart(userId) {
  return http.get(`/v1/carts/${userId}`);
}

export function addItem(userId, productId, quantity = 1) {
  return http.post(`/v1/carts/${userId}/items`, { productId, quantity });
}

export function removeItem(userId, productId) {
  return http.del(`/v1/carts/${userId}/items/${productId}`);
}

export function reduceItem(userId, productId, amount = 1) {
  return http.patch(`/v1/carts/${userId}/items/${productId}?amount=${amount}`);
}
