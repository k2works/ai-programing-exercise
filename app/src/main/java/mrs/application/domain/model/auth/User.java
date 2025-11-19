package mrs.application.domain.model.auth;

public record User(UserId userId, Password password, Name name, RoleName roleName) {
    public boolean isAdmin() {
        return roleName == RoleName.ADMIN;
    }
}
