package com.example.users.controller;

import com.example.users.config.AppUserDetails;
import com.example.users.config.JwtUtils;
import com.example.users.config.PasswordDecryptor;
import com.example.users.dto.CreateUserRequest;
import com.example.users.dto.JwtResponse;
import com.example.users.dto.LoginRequest;
import com.example.users.dto.MessageResponse;
import com.example.users.entity.User;
import com.example.users.repository.UserRepo;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;

@CrossOrigin(origins = "*", maxAge = 3600)
@RestController
@RequestMapping("/api/auth")
public class AuthController {
    
    private final AuthenticationManager authenticationManager;
    private final UserRepo userRepository;
    private final PasswordEncoder encoder;
    private final JwtUtils jwtUtils;
    private final PasswordDecryptor passwordDecryptor;

    public AuthController(AuthenticationManager authenticationManager, UserRepo userRepository, PasswordEncoder encoder, JwtUtils jwtUtils, PasswordDecryptor passwordDecryptor) {
        this.authenticationManager = authenticationManager;
        this.userRepository = userRepository;
        this.encoder = encoder;
        this.jwtUtils = jwtUtils;
        this.passwordDecryptor = passwordDecryptor;
    }

    @PostMapping("/signin")
    public ResponseEntity<?> authenticateUser(@Valid @RequestBody LoginRequest loginRequest) {

        // Decrypt the AES-encrypted password from the frontend
        String decryptedPassword = passwordDecryptor.decrypt(loginRequest.getPassword());

        Authentication authentication = authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(loginRequest.getUsername(), decryptedPassword));

        SecurityContextHolder.getContext().setAuthentication(authentication);
        String jwt = jwtUtils.generateJwtToken(authentication);
        
        AppUserDetails userDetails = (AppUserDetails) authentication.getPrincipal();    
        List<String> roles = userDetails.getAuthorities().stream()
                .map(item -> item.getAuthority())
                .collect(Collectors.toList());

        return ResponseEntity.ok(new JwtResponse(jwt, 
                                                 userDetails.getId(), 
                                                 userDetails.getUsername(), 
                                                 userDetails.getEmail(),
                                                 roles));
    }

    @PostMapping("/signup")
    public ResponseEntity<?> registerUser(@Valid @RequestBody CreateUserRequest signUpRequest) {
        if (userRepository.existsByUserName(signUpRequest.userName())) {
            return ResponseEntity
                    .badRequest()
                    .body(new MessageResponse("Error: Username is already taken!"));
        }

        if (userRepository.existsByEmail(signUpRequest.email())) {
            return ResponseEntity
                    .badRequest()
                    .body(new MessageResponse("Error: Email is already in use!"));
        }

        // Create new user's account
        User user = new User();
        user.setUserName(signUpRequest.userName());
        user.setEmail(signUpRequest.email());
        // Decrypt the AES-encrypted password and encode with BCrypt
        String decryptedPassword = passwordDecryptor.decrypt(signUpRequest.password());
        user.setPassword(encoder.encode(decryptedPassword));
        user.setFirstName(signUpRequest.firstName());
        user.setLastName(signUpRequest.lastName());
        // Use role from request: ADMIN = Seller, USER = Buyer (default)
        String role = signUpRequest.role() != null && signUpRequest.role().equalsIgnoreCase("ADMIN")
                ? "ADMIN" : "USER";
        user.setRole(role);

        userRepository.save(user);

        return ResponseEntity.ok(new MessageResponse("User registered successfully!!!"));
    }
}
