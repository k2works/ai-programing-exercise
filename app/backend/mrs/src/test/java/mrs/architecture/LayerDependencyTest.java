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

    /**
     * ドメイン層は他のアプリケーションレイヤーに依存してはいけない
     */
    @ArchTest
    static final ArchRule domain_should_not_depend_on_application_layer = noClasses()
            .that().resideInAPackage("mrs.application.domain..")
            .should().dependOnClassesThat()
            .resideInAnyPackage(
                    "mrs.application.service..",
                    "mrs.application.port..",
                    "mrs.infrastructure..",
                    "mrs.common.."
            );

    /**
     * ドメイン層は外部ライブラリに依存してはいけない
     */
    @ArchTest
    static final ArchRule domain_should_not_depend_on_external_libraries = noClasses()
            .that().resideInAPackage("mrs.application.domain..")
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
            .that().resideInAPackage("mrs.application.service..")
            .should().dependOnClassesThat()
            .resideInAnyPackage("mrs.infrastructure..");

    /**
     * ポートインターフェースは実装に依存してはいけない
     */
    @ArchTest
    static final ArchRule ports_should_not_depend_on_implementations = noClasses()
            .that().resideInAPackage("mrs.application.port..")
            .should().dependOnClassesThat()
            .resideInAnyPackage(
                    "mrs.infrastructure..",
                    "mrs.common.."
            );

    /**
     * Web層（コントローラー）はドメイン層に直接依存してはいけない
     */
    @ArchTest
    static final ArchRule web_should_not_depend_on_domain_directly = noClasses()
            .that().resideInAPackage("mrs.infrastructure.in.web..")
            .should().dependOnClassesThat()
            .resideInAnyPackage("mrs.application.domain..");

    /**
     * 永続化層（アダプター）はWeb層に依存してはいけない
     */
    @ArchTest
    static final ArchRule persistence_should_not_depend_on_web = noClasses()
            .that().resideInAPackage("mrs.infrastructure.out.persistence..")
            .should().dependOnClassesThat()
            .resideInAnyPackage("mrs.infrastructure.in.web..");

    /**
     * 設定クラスはドメイン層に依存してはいけない
     */
    @ArchTest
    static final ArchRule config_should_not_depend_on_domain = noClasses()
            .that().resideInAPackage("mrs.infrastructure.config..")
            .should().dependOnClassesThat()
            .resideInAnyPackage("mrs.application.domain..");

    /**
     * 各レイヤーは適切なパッケージにのみアクセス可能
     */
    @ArchTest
    static final ArchRule application_services_should_only_access_allowed_packages = classes()
            .that().resideInAPackage("mrs.application.service..")
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    "mrs.application.domain..",     // ドメインモデル
                    "mrs.application.dto..",        // データ転送オブジェクト
                    "mrs.application.exception..",  // 例外クラス
                    "mrs.application.port..",       // ポートインターフェース
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
            .that().resideInAPackage("mrs.infrastructure.in.web..")
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    "mrs.application.dto..",        // データ転送オブジェクト
                    "mrs.application.exception..",  // 例外クラス
                    "mrs.application.mapper..",     // マッパー
                    "mrs.application.port.in..",    // 入力ポート
                    "mrs.infrastructure.in.web..",  // Web層内部（Mapper等）
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