package com.k2works.mrs;

import com.tngtech.archunit.core.importer.ImportOption;
import com.tngtech.archunit.junit.AnalyzeClasses;
import com.tngtech.archunit.junit.ArchTest;
import com.tngtech.archunit.lang.ArchRule;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.classes;

/**
 * ヘキサゴナルアーキテクチャの制約をテストするクラス
 */
@AnalyzeClasses(packages = "com.k2works.mrs", importOptions = ImportOption.DoNotIncludeTests.class)
class ArchitectureTest {

    @ArchTest
    static final ArchRule repositories_should_be_interfaces =
        classes().that().resideInAPackage("..domain.repository..")
            .should().beInterfaces()
            .allowEmptyShould(true);

    @ArchTest
    static final ArchRule usecases_should_be_interfaces =
        classes().that().resideInAPackage("..application.usecase..")
            .and().haveSimpleNameEndingWith("UseCase")
            .should().beInterfaces()
            .allowEmptyShould(true);
}