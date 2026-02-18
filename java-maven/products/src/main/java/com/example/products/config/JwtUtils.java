package com.example.products.config;

import io.jsonwebtoken.*;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;

/**
 * JWT utility class for token validation in the Products service.
 *
 * <p>This is a validate-only utility — it does NOT generate tokens.
 * Token generation is the responsibility of the Users service.
 * All services share the same HMAC secret for signature verification.</p>
 */
@Component
public class JwtUtils {

    private static final Logger logger = LoggerFactory.getLogger(JwtUtils.class);

    @Value("${app.jwtSecret:SecretKeyMustBeAtLeast32BytesLongForHS256AlgorithmSecurity}")
    private String jwtSecret;

    /**
     * Derives the HMAC-SHA signing key from the Base64-encoded secret.
     *
     * @return the {@link SecretKey} used for JWT signature verification
     */
    private SecretKey key() {
        return Keys.hmacShaKeyFor(Decoders.BASE64.decode(jwtSecret));
    }

    /**
     * Extracts the username (subject claim) from a valid JWT token.
     *
     * @param token the JWT token string
     * @return the username embedded in the token's subject claim
     */
    public String getUserNameFromJwtToken(String token) {
        return Jwts.parser().verifyWith(key()).build()
                   .parseSignedClaims(token).getPayload().getSubject();
    }

    /**
     * Validates a JWT token's signature and expiration.
     *
     * @param authToken the JWT token string to validate
     * @return {@code true} if the token is valid, {@code false} otherwise
     */
    public boolean validateJwtToken(String authToken) {
        try {
            Jwts.parser().verifyWith(key()).build().parseSignedClaims(authToken);
            return true;
        } catch (MalformedJwtException e) {
            logger.error("Invalid JWT token: {}", e.getMessage());
        } catch (ExpiredJwtException e) {
            logger.error("JWT token is expired: {}", e.getMessage());
        } catch (UnsupportedJwtException e) {
            logger.error("JWT token is unsupported: {}", e.getMessage());
        } catch (IllegalArgumentException e) {
            logger.error("JWT claims string is empty: {}", e.getMessage());
        }
        return false;
    }
}
