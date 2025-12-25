package com.example.production.infrastructure.persistence;

import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

@MappedTypes(PurchaseOrderStatus.class)
public class PurchaseOrderStatusTypeHandler extends BaseTypeHandler<PurchaseOrderStatus> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, PurchaseOrderStatus parameter, JdbcType jdbcType)
            throws SQLException {
        ps.setString(i, parameter.getDisplayName());
    }

    @Override
    public PurchaseOrderStatus getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String value = rs.getString(columnName);
        return value == null ? null : PurchaseOrderStatus.fromDisplayName(value);
    }

    @Override
    public PurchaseOrderStatus getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String value = rs.getString(columnIndex);
        return value == null ? null : PurchaseOrderStatus.fromDisplayName(value);
    }

    @Override
    public PurchaseOrderStatus getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String value = cs.getString(columnIndex);
        return value == null ? null : PurchaseOrderStatus.fromDisplayName(value);
    }
}
