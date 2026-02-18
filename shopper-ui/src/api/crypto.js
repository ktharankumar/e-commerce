/**
 * AES-256-GCM encryption utility for secure password transit.
 *
 * The same 256-bit key must exist on both frontend and backend.
 * In production, this key would be fetched securely or managed via a KMS.
 */

// Shared AES-256 key (32 bytes / 256 bits) — hex-encoded
const AES_KEY_HEX = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";

function hexToBytes(hex) {
    const bytes = new Uint8Array(hex.length / 2);
    for (let i = 0; i < hex.length; i += 2) {
        bytes[i / 2] = parseInt(hex.substring(i, i + 2), 16);
    }
    return bytes;
}

function bytesToBase64(bytes) {
    let binary = "";
    for (let i = 0; i < bytes.byteLength; i++) {
        binary += String.fromCharCode(bytes[i]);
    }
    return btoa(binary);
}

/**
 * Encrypts a plaintext password using AES-256-GCM.
 * Returns a Base64-encoded string in the format: base64(iv):base64(ciphertext)
 */
export async function encryptPassword(plaintext) {
    const keyBytes = hexToBytes(AES_KEY_HEX);

    const cryptoKey = await crypto.subtle.importKey(
        "raw",
        keyBytes,
        { name: "AES-GCM" },
        false,
        ["encrypt"]
    );

    // Generate a random 12-byte IV for each encryption
    const iv = crypto.getRandomValues(new Uint8Array(12));

    const encoded = new TextEncoder().encode(plaintext);
    const cipherBuffer = await crypto.subtle.encrypt(
        { name: "AES-GCM", iv },
        cryptoKey,
        encoded
    );

    const cipherBytes = new Uint8Array(cipherBuffer);

    return `${bytesToBase64(iv)}:${bytesToBase64(cipherBytes)}`;
}
