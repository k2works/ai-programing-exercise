package mrs.infrastructure.out.db;

import mrs.application.domain.model.User;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface UserMapper {
    User findById(@Param("userId") String userId);
}
