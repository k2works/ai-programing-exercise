plugins {
    java
    application
    jacoco
    checkstyle
    pmd
    id("org.springframework.boot") version "4.0.0"
    id("io.spring.dependency-management") version "1.1.7"
    id("com.github.spotbugs") version "6.0.27"
    id("com.avast.gradle.docker-compose") version "0.17.12"
}

group = "com.example.production"
version = "0.0.1-SNAPSHOT"

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(25)
    }
}

application {
    mainClass.set("com.example.production.ProductionManagementApplication")
}

repositories {
    mavenCentral()
}

// バージョン管理
val mybatisVersion = "4.0.0"
val testcontainersVersion = "1.20.4"
val springdocVersion = "2.8.4"

dependencies {
    // Spring Boot
    implementation("org.springframework.boot:spring-boot-starter-web")
    implementation("org.springframework.boot:spring-boot-starter-jdbc")
    implementation("org.springframework.boot:spring-boot-starter-validation")

    // MyBatis
    implementation("org.mybatis.spring.boot:mybatis-spring-boot-starter:$mybatisVersion")

    // Flyway
    implementation("org.flywaydb:flyway-core")
    implementation("org.flywaydb:flyway-database-postgresql")

    // OpenAPI / Swagger
    implementation("org.springdoc:springdoc-openapi-starter-webmvc-ui:$springdocVersion")

    // Database
    runtimeOnly("org.postgresql:postgresql")

    // Lombok
    compileOnly("org.projectlombok:lombok")
    annotationProcessor("org.projectlombok:lombok")

    // Test
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    testImplementation("org.springframework.boot:spring-boot-webmvc-test")
    testImplementation("org.springframework.boot:spring-boot-testcontainers")
    testImplementation("org.mybatis.spring.boot:mybatis-spring-boot-starter-test:$mybatisVersion")
    testImplementation(platform("org.testcontainers:testcontainers-bom:$testcontainersVersion"))
    testImplementation("org.testcontainers:junit-jupiter")
    testImplementation("org.testcontainers:postgresql")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")

    // SpotBugs
    spotbugsPlugins("com.h3xstream.findsecbugs:findsecbugs-plugin:1.13.0")
}

tasks.withType<Test> {
    useJUnitPlatform()
    finalizedBy(tasks.jacocoTestReport)
}

// JaCoCo
jacoco {
    toolVersion = "0.8.14" // Java 25 support
}

tasks.jacocoTestReport {
    dependsOn(tasks.test)
    reports {
        xml.required = true
        html.required = true
    }
    classDirectories.setFrom(
        files(classDirectories.files.map {
            fileTree(it) {
                exclude(
                    "**/Application.class",
                    "**/Application$*.class"
                )
            }
        })
    )
}

tasks.jacocoTestCoverageVerification {
    violationRules {
        rule {
            limit {
                minimum = "0.0".toBigDecimal() // 初期は0%、徐々に上げる
            }
        }
    }
}

// Checkstyle
checkstyle {
    toolVersion = "10.20.2"
    configFile = file("${rootDir}/config/checkstyle/checkstyle.xml")
    isIgnoreFailures = false
}

// SpotBugs (Java 25 対応: 4.9.7+)
spotbugs {
    ignoreFailures = false
    excludeFilter = file("${rootDir}/config/spotbugs/exclude.xml")
    toolVersion = "4.9.8"
}

tasks.withType<com.github.spotbugs.snom.SpotBugsTask> {
    reports.create("html") {
        required = true
    }
    reports.create("xml") {
        required = true // SonarQube 連携用
    }
}

// PMD (Java 25 対応: 7.16.0+)
pmd {
    toolVersion = "7.16.0"
    isConsoleOutput = true
    ruleSetFiles = files("${rootDir}/config/pmd/ruleset.xml")
    ruleSets = listOf() // ruleSetFilesを使用するため空に
    isIgnoreFailures = false
}

// Docker Compose 設定
dockerCompose {
    useComposeFiles.add("docker-compose.yml")
    isRequiredBy(tasks.named("bootRun"))
    waitForTcpPorts = true
    captureContainersOutputToFiles = project.file("build/docker-logs")
}

// bootRun 実行前に Docker Compose を起動
tasks.named("bootRun") {
    dependsOn("composeUp")
}
