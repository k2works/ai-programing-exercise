package com.example.production.infrastructure.persistence;

import com.example.production.domain.model.subcontract.SupplyType;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

@MappedTypes(SupplyType.class)
public class SupplyTypeTypeHandler extends BaseTypeHandler<SupplyType> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, SupplyType parameter, JdbcType jdbcType)
            throws SQLException {
        ps.setString(i, parameter.getDisplayName());
    }

    @Override
    public SupplyType getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String value = rs.getString(columnName);
        return value == null ? null : SupplyType.fromDisplayName(value);
    }

    @Override
    public SupplyType getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String value = rs.getString(columnIndex);
        return value == null ? null : SupplyType.fromDisplayName(value);
    }

    @Override
    public SupplyType getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String value = cs.getString(columnIndex);
        return value == null ? null : SupplyType.fromDisplayName(value);
    }
}
