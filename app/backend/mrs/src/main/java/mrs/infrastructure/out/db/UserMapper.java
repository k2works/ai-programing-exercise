package mrs.infrastructure.out.db;

import mrs.application.domain.model.auth.User;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface UserMapper {
    User findByUserId(@Param("userId") String userId);
}
