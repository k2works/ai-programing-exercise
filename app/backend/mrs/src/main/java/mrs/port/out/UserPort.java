package mrs.port.out;

import mrs.domain.model.auth.User;

public interface UserPort {
    User findByUserId(String userId);
}