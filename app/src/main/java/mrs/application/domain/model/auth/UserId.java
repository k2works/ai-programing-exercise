package mrs.application.domain.model.auth;

public record UserId(String value) {
    public UserId {
        if (value == null || value.isEmpty()) {
            throw new IllegalArgumentException("UserId cannot be null or empty");
        }
    }
}
