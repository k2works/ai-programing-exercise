package mrs.application.domain.model.auth;

public record Password(String value) {
    public Password {
        if (value == null || value.isEmpty()) {
            throw new IllegalArgumentException("Password cannot be null or empty");
        }
    }

    public boolean isBCryptEncoded() {
        return value.startsWith("$2a$") || value.startsWith("$2b$") || value.startsWith("$2y$");
    }
}
