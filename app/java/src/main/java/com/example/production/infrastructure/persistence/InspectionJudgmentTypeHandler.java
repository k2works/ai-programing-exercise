package com.example.production.infrastructure.persistence;

import com.example.production.domain.model.quality.InspectionJudgment;
import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * 検査判定のTypeHandler
 */
@MappedTypes(InspectionJudgment.class)
public class InspectionJudgmentTypeHandler extends BaseTypeHandler<InspectionJudgment> {

    @Override
    public void setNonNullParameter(PreparedStatement ps, int i, InspectionJudgment parameter, JdbcType jdbcType)
            throws SQLException {
        ps.setString(i, parameter.getDisplayName());
    }

    @Override
    public InspectionJudgment getNullableResult(ResultSet rs, String columnName) throws SQLException {
        String value = rs.getString(columnName);
        return value == null ? null : InspectionJudgment.fromDisplayName(value);
    }

    @Override
    public InspectionJudgment getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
        String value = rs.getString(columnIndex);
        return value == null ? null : InspectionJudgment.fromDisplayName(value);
    }

    @Override
    public InspectionJudgment getNullableResult(CallableStatement cs, int columnIndex) throws SQLException {
        String value = cs.getString(columnIndex);
        return value == null ? null : InspectionJudgment.fromDisplayName(value);
    }
}
