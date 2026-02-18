package com.example.users.config;

import java.nio.charset.StandardCharsets;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.springframework.stereotype.Component;

/**
 * Utility to decrypt AES-256-GCM encrypted passwords sent from the frontend.
 *
 * <p>The frontend encrypts the password with AES-256-GCM and sends it as
 * {@code base64(iv):base64(ciphertext)}. This class decrypts it back
 * to the original plaintext password.</p>
 *
 * <p>The same 256-bit key must be shared between frontend and backend.</p>
 */
@Component
public class PasswordDecryptor {

    // Same 32-byte (256-bit) key as in the frontend crypto.js — hex-encoded
    private static final String AES_KEY_HEX = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
    private static final int GCM_TAG_LENGTH = 128; // bits

    private static byte[] hexToBytes(String hex) {
        byte[] bytes = new byte[hex.length() / 2];
        for (int i = 0; i < hex.length(); i += 2) {
            bytes[i / 2] = (byte) Integer.parseInt(hex.substring(i, i + 2), 16);
        }
        return bytes;
    }

    /**
     * Decrypts an AES-256-GCM encrypted password.
     *
     * @param encryptedPayload the encrypted payload in format {@code base64(iv):base64(ciphertext)}
     * @return the original plaintext password
     * @throws RuntimeException if decryption fails
     */
    public String decrypt(String encryptedPayload) {
        try {
            String[] parts = encryptedPayload.split(":");
            if (parts.length != 2) {
                throw new IllegalArgumentException("Invalid encrypted payload format. Expected base64(iv):base64(ciphertext)");
            }

            byte[] iv = Base64.getDecoder().decode(parts[0]);
            byte[] ciphertext = Base64.getDecoder().decode(parts[1]);
            byte[] keyBytes = hexToBytes(AES_KEY_HEX);

            SecretKeySpec keySpec = new SecretKeySpec(keyBytes, "AES");
            GCMParameterSpec gcmSpec = new GCMParameterSpec(GCM_TAG_LENGTH, iv);

            Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
            cipher.init(Cipher.DECRYPT_MODE, keySpec, gcmSpec);

            byte[] plaintext = cipher.doFinal(ciphertext);
            return new String(plaintext, StandardCharsets.UTF_8);

        } catch (Exception e) {
            throw new RuntimeException("Password decryption failed: " + e.getMessage(), e);
        }
    }
}
