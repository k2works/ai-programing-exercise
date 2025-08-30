package mrs.architecture;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;

/**
 * 命名規約テスト
 * パッケージ構造と命名規則の一貫性を検証
 */
@AnalyzeClasses(packages = "mrs", importOptions = ImportOption.DoNotIncludeTests.class)
public class NamingConventionTest {

    /**
     * コントローラーは Controllerで終わること
     */
    @ArchTest
    static final ArchRule controllers_should_end_with_controller = classes()
            .that().resideInAPackage("mrs.infrastructure.in.web..")
            .should().haveSimpleNameEndingWith("Controller");

    /**
     * サービスクラスは Serviceで終わること
     */
    @ArchTest
    static final ArchRule services_should_end_with_service = classes()
            .that().resideInAPackage("mrs.application.service..")
            .should().haveSimpleNameEndingWith("Service");

    /**
     * 永続化アダプターは PersistenceAdapterで終わること
     */
    @ArchTest
    static final ArchRule persistence_adapters_should_end_with_persistence_adapter = classes()
            .that().resideInAPackage("mrs.infrastructure.out.persistence..")
            .and().areNotInterfaces()
            .and().doNotHaveSimpleName("package-info")
            .and().haveSimpleNameNotEndingWith("Mapper")  // MyBatis Mapperを除外
            .should().haveSimpleNameEndingWith("PersistenceAdapter");

    /**
     * MyBatis Mapperは Mapperで終わること
     */
    @ArchTest
    static final ArchRule mybatis_mappers_should_end_with_mapper = classes()
            .that().resideInAPackage("mrs.infrastructure.out.persistence..")
            .and().areInterfaces()
            .and().doNotHaveSimpleName("package-info")
            .should().haveSimpleNameEndingWith("Mapper");

    /**
     * 設定クラスは Configで終わること
     */
    @ArchTest
    static final ArchRule config_classes_should_end_with_config = classes()
            .that().resideInAPackage("mrs.infrastructure.config..")
            .and().areNotInterfaces()
            .and().doNotHaveSimpleName("package-info")
            .should().haveSimpleNameEndingWith("Config");

    /**
     * ドメインモデルは適切なパッケージに配置されること
     */
    @ArchTest
    static final ArchRule domain_models_should_be_in_model_package = classes()
            .that().resideInAPackage("mrs.application.domain.model..")
            .and().areNotInterfaces()
            .and().doNotHaveSimpleName("package-info")
            .should().bePublic();

    /**
     * 入力ポートは適切なパッケージに配置されること
     */
    @ArchTest
    static final ArchRule input_ports_should_be_in_port_in_package = classes()
            .that().haveSimpleNameEndingWith("UseCase")
            .should().resideInAPackage("mrs.application.port.in..");

    /**
     * 出力ポートは適切なパッケージに配置されること
     */
    @ArchTest
    static final ArchRule output_ports_should_be_in_port_out_package = classes()
            .that().haveSimpleNameEndingWith("Port")
            .should().resideInAPackage("mrs.application.port.out..");

    /**
     * セキュリティ関連クラスは適切なパッケージに配置されること
     */
    @ArchTest
    static final ArchRule security_classes_should_be_in_security_package = classes()
            .that().haveSimpleNameContaining("Jwt")
            .or().haveSimpleNameContaining("Authentication")
            .or().haveSimpleNameContaining("Security")
            .should().resideInAPackage("mrs.common.security..");

    /**
     * 例外クラスは Exceptionで終わること
     */
    @ArchTest
    static final ArchRule exceptions_should_end_with_exception = classes()
            .that().areAssignableTo(RuntimeException.class)
            .and().resideInAnyPackage("mrs..")
            .should().haveSimpleNameEndingWith("Exception");

    /**
     * Spring のコンポーネントアノテーションが適切に使用されていること
     */
    @ArchTest
    static final ArchRule controllers_should_be_annotated_with_rest_controller = classes()
            .that().resideInAPackage("mrs.infrastructure.in.web..")
            .and().haveSimpleNameEndingWith("Controller")
            .should().beAnnotatedWith("org.springframework.web.bind.annotation.RestController");

    /**
     * サービスクラスはServiceアノテーションを持つこと
     */
    @ArchTest
    static final ArchRule services_should_be_annotated_with_service = classes()
            .that().resideInAPackage("mrs.application.service..")
            .and().haveSimpleNameEndingWith("Service")
            .should().beAnnotatedWith("org.springframework.stereotype.Service");

    /**
     * 永続化アダプターはComponentアノテーションを持つこと
     */
    @ArchTest
    static final ArchRule persistence_adapters_should_be_annotated_with_component = classes()
            .that().resideInAPackage("mrs.infrastructure.out.persistence..")
            .and().haveSimpleNameEndingWith("PersistenceAdapter")
            .should().beAnnotatedWith("org.springframework.stereotype.Component");

    /**
     * MyBatis MapperはMapperアノテーションを持つこと
     */
    @ArchTest
    static final ArchRule mappers_should_be_annotated_with_mapper = classes()
            .that().resideInAPackage("mrs.infrastructure.out.persistence..")
            .and().areInterfaces()
            .and().haveSimpleNameEndingWith("Mapper")
            .should().beAnnotatedWith("org.apache.ibatis.annotations.Mapper");
}