package com.example.users.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

public record CreateUserRequest(
        @NotNull
        @Size(min = 4, max = 16, message = "Length of username should be between 4 and 16")
        String userName,

        @NotNull
        @Size(min = 8, message = "Minimum 8 characters required")
        String password,

        @NotNull
        @NotBlank(message = "First name cannot be blank")
        String firstName,

        @NotNull
        @NotBlank(message = "Last name cannot be blank, at least 1 character required")
        String lastName,

        @Email(message = "Please provide a valid email")
        @NotNull String email,

        String role
) {}
