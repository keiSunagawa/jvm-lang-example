.PHONY: genParser
genParser:
	java -jar ./antlr-4.7-complete.jar ./grammer/*.g4
	mkdir -p ./core/src/main/java/me/kerfume/jisp/antlr/
	mv ./grammer/*.java ./core/src/main/java/me/kerfume/jisp/antlr/
.PHONY: grun
grun:
	javac -cp "./antlr-4.7-complete.jar:./grammer" ./grammer/*.java
	mkdir -p ./grammer/me/kerfume/jisp/antlr/
	mv ./grammer/*.class ./grammer/me/kerfume/jisp/antlr/
	java -cp "./antlr-4.7-complete.jar:./grammer/" org.antlr.v4.gui.TestRig me.kerfume.jisp.antlr.Jisp stmts -tokens
.PHONY: compile
compile:
	java -jar jasmin.jar test.j
	java JispCode
