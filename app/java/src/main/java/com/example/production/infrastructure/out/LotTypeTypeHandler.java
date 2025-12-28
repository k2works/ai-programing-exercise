package com.example.production.infrastructure.out;

import com.example.production.domain.model.quality.LotType;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * ロット種別のTypeHandler
 */
@MappedTypes(LotType.class)
public class LotTypeTypeHandler extends BaseTypeHandler<LotType> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, LotType parameter, JdbcType jdbcType)
            throws SQLException {
        ps.setString(i, parameter.getDisplayName());
    }

    @Override
    public LotType getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String value = rs.getString(columnName);
        return value == null ? null : LotType.fromDisplayName(value);
    }

    @Override
    public LotType getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String value = rs.getString(columnIndex);
        return value == null ? null : LotType.fromDisplayName(value);
    }

    @Override
    public LotType getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String value = cs.getString(columnIndex);
        return value == null ? null : LotType.fromDisplayName(value);
    }
}
