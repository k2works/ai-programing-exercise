package mrs.application.port.out;

import mrs.application.domain.model.auth.User;

public interface UserPort {
    User findByUserId(String userId);
}