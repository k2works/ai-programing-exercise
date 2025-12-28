package com.example.production.infrastructure.in.web.form;

import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;

/**
 * 場所登録・編集フォーム
 */
@Data
public class LocationForm {

    @NotBlank(message = "場所コードは必須です")
    @Size(max = 20, message = "場所コードは20文字以内で入力してください")
    private String locationCode;

    @NotBlank(message = "場所名は必須です")
    @Size(max = 100, message = "場所名は100文字以内で入力してください")
    private String locationName;

    @NotNull(message = "場所区分は必須です")
    private LocationType locationType;

    @Size(max = 20, message = "親場所コードは20文字以内で入力してください")
    private String parentLocationCode;

    /**
     * フォームをエンティティに変換（新規登録用）
     */
    public Location toEntity() {
        return Location.builder()
                .locationCode(this.locationCode)
                .locationName(this.locationName)
                .locationType(this.locationType)
                .parentLocationCode(this.parentLocationCode)
                .build();
    }

    /**
     * エンティティからフォームを生成
     */
    public static LocationForm from(Location location) {
        LocationForm form = new LocationForm();
        form.setLocationCode(location.getLocationCode());
        form.setLocationName(location.getLocationName());
        form.setLocationType(location.getLocationType());
        form.setParentLocationCode(location.getParentLocationCode());
        return form;
    }
}
