package com.example.production.infrastructure.out;

import com.example.production.domain.model.inventory.StocktakingStatus;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * 棚卸ステータスのTypeHandler
 */
@MappedTypes(StocktakingStatus.class)
public class StocktakingStatusTypeHandler extends BaseTypeHandler<StocktakingStatus> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, StocktakingStatus parameter, JdbcType jdbcType)
            throws SQLException {
        ps.setString(i, parameter.getDisplayName());
    }

    @Override
    public StocktakingStatus getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String value = rs.getString(columnName);
        return value == null ? null : StocktakingStatus.fromDisplayName(value);
    }

    @Override
    public StocktakingStatus getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String value = rs.getString(columnIndex);
        return value == null ? null : StocktakingStatus.fromDisplayName(value);
    }

    @Override
    public StocktakingStatus getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String value = cs.getString(columnIndex);
        return value == null ? null : StocktakingStatus.fromDisplayName(value);
    }
}
