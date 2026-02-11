import { http } from "./http";

// MVP: hardcode a user for now (you can replace with auth later)
export const DEFAULT_USER_ID = 1;

export function getCart(userId = DEFAULT_USER_ID) {
  return http.get(`/v1/carts/${userId}`);
}

export function addItem(userId = DEFAULT_USER_ID, productId, quantity = 1) {
  return http.post(`/v1/carts/${userId}/items`, { productId, quantity });
}

export function removeItem(userId = DEFAULT_USER_ID, productId) {
  return http.del(`/v1/carts/${userId}/items/${productId}`);
}

export function reduceItem(userId = DEFAULT_USER_ID, productId, amount = 1) {
  return http.patch(`/v1/carts/${userId}/items/${productId}?amount=${amount}`);
}
