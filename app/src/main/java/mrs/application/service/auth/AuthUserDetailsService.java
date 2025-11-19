package mrs.application.service.auth;

import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.auth.UserId;
import mrs.application.port.out.UserPort;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
public class AuthUserDetailsService implements UserDetailsService {
    private final UserPort userPort;

    public AuthUserDetailsService(UserPort userPort) {
        this.userPort = userPort;
    }

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        User user = userPort.findById(new UserId(username));
        if (user == null) {
            throw new UsernameNotFoundException("User not found: " + username);
        }
        return new AuthUserDetails(user);
    }
}
