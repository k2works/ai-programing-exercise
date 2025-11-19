package mrs.application.domain.model.auth;

public record Name(String lastName, String firstName) {
    public Name {
        if (lastName == null || lastName.isEmpty()) {
            throw new IllegalArgumentException("Last name cannot be null or empty");
        }
        if (firstName == null || firstName.isEmpty()) {
            throw new IllegalArgumentException("First name cannot be null or empty");
        }
    }

    public String fullName() {
        return lastName + " " + firstName;
    }
}
