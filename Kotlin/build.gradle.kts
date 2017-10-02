import org.gradle.api.tasks.JavaExec

plugins {
	kotlin("jvm")
}

repositories {
	jcenter()
	mavenCentral()
}

dependencies {
	compile(kotlin("stdlib"))
}

val targets = file("src/main/kotlin/uk/org/winder/pi_quadrature").listFiles().filter{it.name.endsWith(".kt") && it.name.startsWith("pi_")}.map{
	val root = it.name.replaceFirst(".kt", "")
	val className = "uk.org.winder.pi_quadrature." + root.capitalize() + "Kt"
	task<JavaExec>("run_" + root) {
		dependsOn("classes")
		classpath("build/classes/kotlin/main", "/home/users/russel/.sdkman/candidates/kotlin/current/lib/kotlin-runtime.jar")
		main = className
	}
}

task("runAll") {
	dependsOn(targets)
}
