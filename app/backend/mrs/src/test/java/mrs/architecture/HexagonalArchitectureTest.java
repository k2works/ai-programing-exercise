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

    /**
     * 入力ポート（UseCase）はインターフェースであること
     */
    @ArchTest
    static final ArchRule input_ports_should_be_interfaces = classes()
            .that().resideInAPackage("mrs.application.port.in..")
            .should().beInterfaces();

    /**
     * 出力ポート（Port）はインターフェースであること
     */
    @ArchTest
    static final ArchRule output_ports_should_be_interfaces = classes()
            .that().resideInAPackage("mrs.application.port.out..")
            .should().beInterfaces();

    /**
     * 入力ポートは UseCaseで終わる命名規約
     */
    @ArchTest
    static final ArchRule input_ports_should_end_with_use_case = classes()
            .that().resideInAPackage("mrs.application.port.in..")
            .should().haveSimpleNameEndingWith("UseCase");

    /**
     * 出力ポートは Portで終わる命名規約
     */
    @ArchTest
    static final ArchRule output_ports_should_end_with_port = classes()
            .that().resideInAPackage("mrs.application.port.out..")
            .should().haveSimpleNameEndingWith("Port");

    /**
     * 入力アダプター（Controller）は入力ポートに依存すること
     */
    @ArchTest
    static final ArchRule input_adapters_should_depend_on_input_ports = classes()
            .that().resideInAPackage("mrs.infrastructure.in.web..")
            .and().haveSimpleNameEndingWith("Controller")
            .should().dependOnClassesThat()
            .resideInAPackage("mrs.application.port.in..");

    /**
     * 出力アダプター（PersistenceAdapter）は出力ポートを実装すること
     */
    @ArchTest
    static final ArchRule output_adapters_should_implement_output_ports = classes()
            .that().resideInAPackage("mrs.infrastructure.out.persistence..")
            .and().haveSimpleNameEndingWith("PersistenceAdapter")
            .should().dependOnClassesThat()
            .resideInAPackage("mrs.application.port.out..");

    /**
     * アプリケーションサービスは入力ポートを実装すること
     */
    @ArchTest
    static final ArchRule application_services_should_implement_input_ports = classes()
            .that().resideInAPackage("mrs.application.service..")
            .and().haveSimpleNameEndingWith("Service")
            .should().dependOnClassesThat()
            .resideInAPackage("mrs.application.port.in..");

    /**
     * アプリケーションサービスは出力ポートに依存すること
     */
    @ArchTest
    static final ArchRule application_services_should_depend_on_output_ports = classes()
            .that().resideInAPackage("mrs.application.service..")
            .and().haveSimpleNameEndingWith("Service")
            .should().dependOnClassesThat()
            .resideInAPackage("mrs.application.port.out..");

    /**
     * ドメインモデルは単純なPOJOであること（フレームワーク依存なし）
     */
    @ArchTest
    static final ArchRule domain_models_should_be_pojos = classes()
            .that().resideInAPackage("mrs.application.domain.model..")
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
            .that().resideInAPackage("mrs.infrastructure.out.persistence..")
            .and().haveSimpleNameEndingWith("PersistenceAdapter")
            .should().dependOnClassesThat()
            .resideInAPackage("mrs.application.domain.model..");

    /**
     * Webアダプター（コントローラー）はドメインモデルに直接依存しないこと
     * UseCase経由でアクセスすること
     */
    @ArchTest
    static final ArchRule web_adapters_should_not_access_domain_directly = classes()
            .that().resideInAPackage("mrs.infrastructure.in.web..")
            .and().haveSimpleNameEndingWith("Controller")
            .should().onlyAccessClassesThat()
            .resideOutsideOfPackage("mrs.application.domain.model..")
            .orShould().dependOnClassesThat()
            .resideInAnyPackage(
                    "mrs.application.port.in..",  // 入力ポート経由でアクセス
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