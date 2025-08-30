package mrs.architecture;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;
import static com.tngtech.archunit.library.Architectures.layeredArchitecture;

/**
 * メインのアーキテクチャテスト
 * ヘキサゴナルアーキテクチャの基本原則を検証
 */
@AnalyzeClasses(packages = "mrs", importOptions = ImportOption.DoNotIncludeTests.class)
public class ArchitectureTest {

    private static final String DOMAIN_PACKAGE = "mrs.application.domain..";
    private static final String APPLICATION_SERVICE_PACKAGE = "mrs.application.service..";
    private static final String APPLICATION_PORT_PACKAGE = "mrs.application.port..";
    private static final String APPLICATION_DTO_PACKAGE = "mrs.application.dto..";
    private static final String APPLICATION_EXCEPTION_PACKAGE = "mrs.application.exception..";
    private static final String APPLICATION_MAPPER_PACKAGE = "mrs.application.mapper..";
    private static final String INFRASTRUCTURE_PACKAGE = "mrs.infrastructure..";
    private static final String COMMON_PACKAGE = "mrs.common..";
    private static final String DOMAIN_LAYER_NAME = "Domain";
    private static final String APPLICATION_LAYER_NAME = "Application";
    private static final String INFRASTRUCTURE_LAYER_NAME = "Infrastructure";
    private static final String COMMON_LAYER_NAME = "Common";

    /**
     * レイヤード アーキテクチャの基本原則
     * 各レイヤーは指定された方向にのみ依存できる
     */
    @ArchTest
    static final ArchRule layered_architecture_should_be_respected = layeredArchitecture()
            .consideringOnlyDependenciesInLayers()
            
            // レイヤー定義
            .layer(DOMAIN_LAYER_NAME).definedBy(DOMAIN_PACKAGE)
            .layer(APPLICATION_LAYER_NAME).definedBy(APPLICATION_SERVICE_PACKAGE, APPLICATION_PORT_PACKAGE,
                    APPLICATION_DTO_PACKAGE, APPLICATION_EXCEPTION_PACKAGE, APPLICATION_MAPPER_PACKAGE)
            .layer(INFRASTRUCTURE_LAYER_NAME).definedBy(INFRASTRUCTURE_PACKAGE)
            .layer(COMMON_LAYER_NAME).definedBy(COMMON_PACKAGE)
            
            // 依存関係ルール
            .whereLayer(DOMAIN_LAYER_NAME).mayNotAccessAnyLayer()  // ドメイン層は他に依存しない
            .whereLayer(APPLICATION_LAYER_NAME).mayOnlyAccessLayers(DOMAIN_LAYER_NAME)  // アプリケーション層はドメイン層のみに依存
            .whereLayer(INFRASTRUCTURE_LAYER_NAME).mayOnlyAccessLayers(DOMAIN_LAYER_NAME, APPLICATION_LAYER_NAME, 
                    COMMON_LAYER_NAME)  // インフラ層は他の全レイヤーにアクセス可能
            .whereLayer(COMMON_LAYER_NAME).mayOnlyAccessLayers(DOMAIN_LAYER_NAME);  // 共通層はドメイン層のみに依存

    /**
     * ドメイン層の純粋性
     * ドメイン層は外部フレームワークに依存してはいけない
     */
    @ArchTest
    static final ArchRule domain_should_not_depend_on_framework = classes()
            .that().resideInAPackage(DOMAIN_PACKAGE)
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    DOMAIN_PACKAGE,               // 自分自身のパッケージ
                    "java..",                     // Java標準ライブラリ
                    "javax..",                    // Java拡張ライブラリ
                    "edu.umd.cs.findbugs.."      // SpotBugs annotations
            );

    /**
     * インフラ層の責任
     * インフラ層のクラスは適切な外部依存を持つことができる
     */
    @ArchTest
    static final ArchRule infrastructure_may_depend_on_spring = classes()
            .that().resideInAPackage(INFRASTRUCTURE_PACKAGE)
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    "mrs..",                      // 自分のアプリケーション
                    "java..",                     // Java標準ライブラリ
                    "javax..",                    // Java拡張ライブラリ
                    "org.springframework..",      // Spring Framework
                    "org.springdoc..",            // SpringDoc OpenAPI
                    "io.swagger.v3..",           // Swagger annotations
                    "io.jsonwebtoken..",          // JWT library  
                    "org.apache.ibatis..",        // MyBatis
                    "edu.umd.cs.findbugs.."      // SpotBugs annotations
            );

    /**
     * セキュリティ層の依存関係
     * セキュリティ関連クラスは適切なフレームワークに依存できる
     */
    private static final String COMMON_SECURITY_PACKAGE = "mrs.common.security..";

    @ArchTest
    static final ArchRule security_may_depend_on_security_frameworks = classes()
            .that().resideInAPackage(COMMON_SECURITY_PACKAGE)
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    DOMAIN_PACKAGE,               // ドメインモデル
                    COMMON_SECURITY_PACKAGE,      // セキュリティパッケージ内
                    "java..",                     // Java標準ライブラリ
                    "javax..",                    // Java拡張ライブラリ
                    "org.springframework..",      // Spring Framework
                    "io.jsonwebtoken..",          // JWT library
                    "jakarta.servlet..",          // Servlet API
                    "org.slf4j..",               // SLF4J logging
                    "edu.umd.cs.findbugs.."      // SpotBugs annotations
            );
}