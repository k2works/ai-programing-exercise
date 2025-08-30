package mrs.architecture;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;

/**
 * ヘキサゴナルアーキテクチャ（ポート&アダプター）のテスト
 * ポートとアダプターパターンの実装を検証
 */
@AnalyzeClasses(packages = "mrs", importOptions = ImportOption.DoNotIncludeTests.class)
public class HexagonalArchitectureTest {

    private static final String INPUT_PORT_PACKAGE = "mrs.application.port.in..";
    private static final String OUTPUT_PORT_PACKAGE = "mrs.application.port.out..";
    private static final String WEB_PACKAGE = "mrs.infrastructure.in.web..";
    private static final String PERSISTENCE_PACKAGE = "mrs.infrastructure.out.persistence..";
    private static final String SERVICE_PACKAGE = "mrs.application.service..";
    private static final String DOMAIN_MODEL_PACKAGE = "mrs.application.domain.model..";

    /**
     * 入力ポート（UseCase）はインターフェースであること
     */
    @ArchTest
    static final ArchRule input_ports_should_be_interfaces = classes()
            .that().resideInAPackage(INPUT_PORT_PACKAGE)
            .should().beInterfaces();

    /**
     * 出力ポート（Port）はインターフェースであること
     */
    @ArchTest
    static final ArchRule output_ports_should_be_interfaces = classes()
            .that().resideInAPackage(OUTPUT_PORT_PACKAGE)
            .should().beInterfaces();

    /**
     * 入力ポートは UseCaseで終わる命名規約
     */
    @ArchTest
    static final ArchRule input_ports_should_end_with_use_case = classes()
            .that().resideInAPackage(INPUT_PORT_PACKAGE)
            .should().haveSimpleNameEndingWith("UseCase");

    /**
     * 出力ポートは Portで終わる命名規約
     */
    @ArchTest
    static final ArchRule output_ports_should_end_with_port = classes()
            .that().resideInAPackage(OUTPUT_PORT_PACKAGE)
            .should().haveSimpleNameEndingWith("Port");

    /**
     * 入力アダプター（Controller）は入力ポートに依存すること
     */
    @ArchTest
    static final ArchRule input_adapters_should_depend_on_input_ports = classes()
            .that().resideInAPackage(WEB_PACKAGE)
            .and().haveSimpleNameEndingWith("Controller")
            .should().dependOnClassesThat()
            .resideInAPackage(INPUT_PORT_PACKAGE);

    /**
     * 出力アダプター（PersistenceAdapter）は出力ポートを実装すること
     */
    @ArchTest
    static final ArchRule output_adapters_should_implement_output_ports = classes()
            .that().resideInAPackage(PERSISTENCE_PACKAGE)
            .and().haveSimpleNameEndingWith("PersistenceAdapter")
            .should().dependOnClassesThat()
            .resideInAPackage(OUTPUT_PORT_PACKAGE);

    /**
     * アプリケーションサービスは入力ポートを実装すること
     */
    @ArchTest
    static final ArchRule application_services_should_implement_input_ports = classes()
            .that().resideInAPackage(SERVICE_PACKAGE)
            .and().haveSimpleNameEndingWith("Service")
            .should().dependOnClassesThat()
            .resideInAPackage(INPUT_PORT_PACKAGE);

    /**
     * アプリケーションサービスは出力ポートに依存すること
     */
    @ArchTest
    static final ArchRule application_services_should_depend_on_output_ports = classes()
            .that().resideInAPackage(SERVICE_PACKAGE)
            .and().haveSimpleNameEndingWith("Service")
            .should().dependOnClassesThat()
            .resideInAPackage(OUTPUT_PORT_PACKAGE);

    /**
     * ドメインモデルは単純なPOJOであること（フレームワーク依存なし）
     */
    @ArchTest
    static final ArchRule domain_models_should_be_pojos = classes()
            .that().resideInAPackage(DOMAIN_MODEL_PACKAGE)
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    "mrs.application.domain..",  // 自分自身のパッケージ
                    "java..",                     // Java標準ライブラリ
                    "javax..",                    // Java拡張ライブラリ
                    "edu.umd.cs.findbugs.."      // SpotBugs annotations（品質向上のため許可）
            );

    /**
     * 永続化アダプターはドメインモデルを知っていること
     */
    @ArchTest
    static final ArchRule persistence_adapters_should_know_domain = classes()
            .that().resideInAPackage(PERSISTENCE_PACKAGE)
            .and().haveSimpleNameEndingWith("PersistenceAdapter")
            .should().dependOnClassesThat()
            .resideInAPackage(DOMAIN_MODEL_PACKAGE);

    /**
     * Webアダプター（コントローラー）はドメインモデルに直接依存しないこと
     * UseCase経由でアクセスすること
     */
    @ArchTest
    static final ArchRule web_adapters_should_not_access_domain_directly = classes()
            .that().resideInAPackage(WEB_PACKAGE)
            .and().haveSimpleNameEndingWith("Controller")
            .should().onlyAccessClassesThat()
            .resideOutsideOfPackage(DOMAIN_MODEL_PACKAGE)
            .orShould().dependOnClassesThat()
            .resideInAnyPackage(
                    INPUT_PORT_PACKAGE,           // 入力ポート経由でアクセス
                    "mrs.common..",               // 共通コンポーネント
                    "java..",                     // Java標準
                    "javax..",                    // Java拡張
                    "org.springframework..",      // Spring Framework
                    "org.springdoc..",            // SpringDoc
                    "io.swagger.v3..",           // Swagger
                    "jakarta.servlet..",          // Servlet API
                    "org.slf4j..",               // Logging
                    "edu.umd.cs.findbugs.."      // SpotBugs annotations
            );
}