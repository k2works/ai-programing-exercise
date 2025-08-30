package mrs.architecture;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses;

/**
 * レイヤー間の依存関係テスト
 * 各レイヤーの依存方向と境界を詳細に検証
 */
@AnalyzeClasses(packages = "mrs", importOptions = ImportOption.DoNotIncludeTests.class)
public class LayerDependencyTest {

    private static final String DOMAIN_PACKAGE = "mrs.application.domain..";
    private static final String SERVICE_PACKAGE = "mrs.application.service..";
    private static final String PORT_PACKAGE = "mrs.application.port..";
    private static final String INFRASTRUCTURE_PACKAGE = "mrs.infrastructure..";
    private static final String COMMON_PACKAGE = "mrs.common..";
    private static final String WEB_PACKAGE = "mrs.infrastructure.in.web..";

    /**
     * ドメイン層は他のアプリケーションレイヤーに依存してはいけない
     */
    @ArchTest
    static final ArchRule domain_should_not_depend_on_application_layer = noClasses()
            .that().resideInAPackage(DOMAIN_PACKAGE)
            .should().dependOnClassesThat()
            .resideInAnyPackage(
                    SERVICE_PACKAGE,
                    PORT_PACKAGE,
                    INFRASTRUCTURE_PACKAGE,
                    COMMON_PACKAGE
            );

    /**
     * ドメイン層は外部ライブラリに依存してはいけない
     */
    @ArchTest
    static final ArchRule domain_should_not_depend_on_external_libraries = noClasses()
            .that().resideInAPackage(DOMAIN_PACKAGE)
            .should().dependOnClassesThat()
            .resideInAnyPackage(
                    "org.springframework..",
                    "org.apache.ibatis..",
                    "io.jsonwebtoken..",
                    "jakarta.servlet..",
                    "org.slf4j.."
            );

    /**
     * アプリケーションサービス層はインフラ層に直接依存してはいけない
     */
    @ArchTest
    static final ArchRule application_service_should_not_depend_on_infrastructure = noClasses()
            .that().resideInAPackage(SERVICE_PACKAGE)
            .should().dependOnClassesThat()
            .resideInAnyPackage(INFRASTRUCTURE_PACKAGE);

    /**
     * ポートインターフェースは実装に依存してはいけない
     */
    @ArchTest
    static final ArchRule ports_should_not_depend_on_implementations = noClasses()
            .that().resideInAPackage(PORT_PACKAGE)
            .should().dependOnClassesThat()
            .resideInAnyPackage(
                    INFRASTRUCTURE_PACKAGE,
                    COMMON_PACKAGE
            );

    /**
     * Web層（コントローラー）はドメイン層に直接依存してはいけない
     */
    @ArchTest
    static final ArchRule web_should_not_depend_on_domain_directly = noClasses()
            .that().resideInAPackage(WEB_PACKAGE)
            .should().dependOnClassesThat()
            .resideInAnyPackage(DOMAIN_PACKAGE);

    /**
     * 永続化層（アダプター）はWeb層に依存してはいけない
     */
    private static final String PERSISTENCE_PACKAGE = "mrs.infrastructure.out.persistence..";

    @ArchTest
    static final ArchRule persistence_should_not_depend_on_web = noClasses()
            .that().resideInAPackage(PERSISTENCE_PACKAGE)
            .should().dependOnClassesThat()
            .resideInAnyPackage(WEB_PACKAGE);

    /**
     * 設定クラスはドメイン層に依存してはいけない
     */
    private static final String CONFIG_PACKAGE = "mrs.infrastructure.config..";

    @ArchTest
    static final ArchRule config_should_not_depend_on_domain = noClasses()
            .that().resideInAPackage(CONFIG_PACKAGE)
            .should().dependOnClassesThat()
            .resideInAnyPackage(DOMAIN_PACKAGE);

    /**
     * 各レイヤーは適切なパッケージにのみアクセス可能
     */
    @ArchTest
    static final ArchRule application_services_should_only_access_allowed_packages = classes()
            .that().resideInAPackage(SERVICE_PACKAGE)
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    DOMAIN_PACKAGE,                 // ドメインモデル
                    "mrs.application.dto..",        // データ転送オブジェクト
                    "mrs.application.exception..",  // 例外クラス
                    PORT_PACKAGE,                   // ポートインターフェース
                    "java..",                       // Java標準ライブラリ
                    "javax..",                      // Java拡張ライブラリ
                    "org.springframework..",        // Spring Framework
                    "edu.umd.cs.findbugs.."        // SpotBugs annotations
            );

    /**
     * Web コントローラーは適切なパッケージにのみアクセス可能
     */
    @ArchTest
    static final ArchRule web_controllers_should_only_access_allowed_packages = classes()
            .that().resideInAPackage(WEB_PACKAGE)
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    "mrs.application.dto..",        // データ転送オブジェクト
                    "mrs.application.exception..",  // 例外クラス
                    "mrs.application.mapper..",     // マッパー
                    "mrs.application.port.in..",    // 入力ポート
                    WEB_PACKAGE,                    // Web層内部（Mapper等）
                    "java..",                       // Java標準ライブラリ
                    "javax..",                      // Java拡張ライブラリ
                    "org.springframework..",        // Spring Framework
                    "org.springdoc..",              // SpringDoc OpenAPI
                    "io.swagger.v3..",             // Swagger annotations
                    "jakarta.servlet..",            // Servlet API
                    "org.slf4j..",                 // SLF4J logging
                    "edu.umd.cs.findbugs.."        // SpotBugs annotations
            );
}