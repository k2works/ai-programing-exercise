package mrs.infrastructure.out.persistence;

import mrs.application.domain.model.auth.*;
import mrs.application.port.out.UserPort;
import mrs.infrastructure.out.persistence.entity.UserEntity;
import org.springframework.stereotype.Component;

@Component
public class UserPersistenceAdapter implements UserPort {
    private final UserJpaRepository repository;

    public UserPersistenceAdapter(UserJpaRepository repository) {
        this.repository = repository;
    }

    @Override
    public User findById(UserId userId) {
        return repository.findById(userId.value())
            .map(this::toDomain)
            .orElse(null);
    }

    @Override
    public void save(User user) {
        repository.save(toEntity(user));
    }

    @Override
    public void delete(User user) {
        repository.deleteById(user.userId().value());
    }

    private User toDomain(UserEntity entity) {
        return new User(
            new UserId(entity.getUserId()),
            new Password(entity.getPassword()),
            new Name(entity.getLastName(), entity.getFirstName()),
            RoleName.valueOf(entity.getRoleName())
        );
    }

    private UserEntity toEntity(User user) {
        return new UserEntity(
            user.userId().value(),
            user.name().firstName(),
            user.name().lastName(),
            user.password().value(),
            user.roleName().name()
        );
    }
}
