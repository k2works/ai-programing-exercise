package mrs.application.port.out;

import mrs.application.domain.model.auth.*;

public interface UserPort {
    User findById(UserId userId);
    void save(User user);
    void delete(User user);
}
