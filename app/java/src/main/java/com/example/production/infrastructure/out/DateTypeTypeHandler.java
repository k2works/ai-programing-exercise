package com.example.production.infrastructure.out;

import com.example.production.domain.model.calendar.DateType;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

@MappedTypes(DateType.class)
public class DateTypeTypeHandler extends BaseTypeHandler<DateType> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i,
            DateType parameter, JdbcType jdbcType) throws SQLException {
        ps.setString(i, parameter.getDisplayName());
    }

    @Override
    public DateType getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String value = rs.getString(columnName);
        return value == null ? null : DateType.fromDisplayName(value);
    }

    @Override
    public DateType getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String value = rs.getString(columnIndex);
        return value == null ? null : DateType.fromDisplayName(value);
    }

    @Override
    public DateType getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String value = cs.getString(columnIndex);
        return value == null ? null : DateType.fromDisplayName(value);
    }
}
