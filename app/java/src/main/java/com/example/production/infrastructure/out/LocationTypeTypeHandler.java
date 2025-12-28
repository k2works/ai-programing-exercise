package com.example.production.infrastructure.out;

import com.example.production.domain.model.location.LocationType;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

@MappedTypes(LocationType.class)
public class LocationTypeTypeHandler extends BaseTypeHandler<LocationType> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i,
            LocationType parameter, JdbcType jdbcType) throws SQLException {
        ps.setString(i, parameter.getDisplayName());
    }

    @Override
    public LocationType getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String value = rs.getString(columnName);
        return value == null ? null : LocationType.fromDisplayName(value);
    }

    @Override
    public LocationType getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String value = rs.getString(columnIndex);
        return value == null ? null : LocationType.fromDisplayName(value);
    }

    @Override
    public LocationType getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String value = cs.getString(columnIndex);
        return value == null ? null : LocationType.fromDisplayName(value);
    }
}
