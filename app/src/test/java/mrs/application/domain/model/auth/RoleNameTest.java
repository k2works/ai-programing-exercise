package mrs.application.domain.model.auth;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class RoleNameTest {

    @Test
    void ADMIN役割が存在する() {
        RoleName role = RoleName.ADMIN;
        assertEquals("ADMIN", role.name());
    }

    @Test
    void USER役割が存在する() {
        RoleName role = RoleName.USER;
        assertEquals("USER", role.name());
    }
}
