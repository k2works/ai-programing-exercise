package mrs.infrastructure.out.persistence.adapter;

import mrs.application.domain.model.auth.User;
import mrs.infrastructure.out.db.UserMapper;
import mrs.application.port.out.UserPort;
import org.springframework.stereotype.Component;

@Component
public class UserPersistenceAdapter implements UserPort {
    private final UserMapper userMapper;

    public UserPersistenceAdapter(UserMapper userMapper) {
        this.userMapper = userMapper;
    }

    @Override
    public User findByUserId(String userId) {
        return userMapper.findByUserId(userId);
    }
}