import { http } from "./http";

export function login(username, password) {
    return http.post("/auth/signin", { username, password });
}

export function register({ userName, email, password, firstName, lastName, role }) {
    return http.post("/auth/signup", { userName, email, password, firstName, lastName, role });
}
