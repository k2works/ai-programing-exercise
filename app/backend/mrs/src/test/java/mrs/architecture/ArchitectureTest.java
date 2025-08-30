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

    /**
     * レイヤード アーキテクチャの基本原則
     * 各レイヤーは指定された方向にのみ依存できる
     */
    @ArchTest
    static final ArchRule layered_architecture_should_be_respected = layeredArchitecture()
            .consideringOnlyDependenciesInLayers()
            
            // レイヤー定義
            .layer("Domain").definedBy("mrs.application.domain..")
            .layer("Application").definedBy("mrs.application.service..", "mrs.application.port..", "mrs.application.dto..", "mrs.application.exception..", "mrs.application.mapper..")
            .layer("Infrastructure").definedBy("mrs.infrastructure..")
            .layer("Common").definedBy("mrs.common..")
            
            // 依存関係ルール
            .whereLayer("Domain").mayNotAccessAnyLayer()  // ドメイン層は他に依存しない
            .whereLayer("Application").mayOnlyAccessLayers("Domain")  // アプリケーション層はドメイン層のみに依存
            .whereLayer("Infrastructure").mayOnlyAccessLayers("Domain", "Application", "Common")  // インフラ層は他の全レイヤーにアクセス可能
            .whereLayer("Common").mayOnlyAccessLayers("Domain");  // 共通層はドメイン層のみに依存

    /**
     * ドメイン層の純粋性
     * ドメイン層は外部フレームワークに依存してはいけない
     */
    @ArchTest
    static final ArchRule domain_should_not_depend_on_framework = classes()
            .that().resideInAPackage("mrs.application.domain..")
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    "mrs.application.domain..",  // 自分自身のパッケージ
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
            .that().resideInAPackage("mrs.infrastructure..")
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
    @ArchTest
    static final ArchRule security_may_depend_on_security_frameworks = classes()
            .that().resideInAPackage("mrs.common.security..")
            .should().onlyDependOnClassesThat()
            .resideInAnyPackage(
                    "mrs.application.domain..",   // ドメインモデル
                    "mrs.common.security..",      // セキュリティパッケージ内
                    "java..",                     // Java標準ライブラリ
                    "javax..",                    // Java拡張ライブラリ
                    "org.springframework..",      // Spring Framework
                    "io.jsonwebtoken..",          // JWT library
                    "jakarta.servlet..",          // Servlet API
                    "org.slf4j..",               // SLF4J logging
                    "edu.umd.cs.findbugs.."      // SpotBugs annotations
            );
}