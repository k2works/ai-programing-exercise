package com.example.production.infrastructure.out;

import com.example.production.domain.model.purchase.ReceivingType;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

@MappedTypes(ReceivingType.class)
public class ReceivingTypeTypeHandler extends BaseTypeHandler<ReceivingType> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, ReceivingType parameter, JdbcType jdbcType)
            throws SQLException {
        ps.setString(i, parameter.getDisplayName());
    }

    @Override
    public ReceivingType getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String value = rs.getString(columnName);
        return value == null ? null : ReceivingType.fromDisplayName(value);
    }

    @Override
    public ReceivingType getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String value = rs.getString(columnIndex);
        return value == null ? null : ReceivingType.fromDisplayName(value);
    }

    @Override
    public ReceivingType getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String value = cs.getString(columnIndex);
        return value == null ? null : ReceivingType.fromDisplayName(value);
    }
}
