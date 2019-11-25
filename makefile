CC=ocamlopt
RM	= rm -f
DIR_SRC = ./src
DIR_BUILD=./build
EXEC = $(DIR_BUILD)/abr
SRC = $(wildcard ${DIR_SRC}/*.cmx)


all : $(EXEC)

$(EXEC): ${DIR_SRC}/Util.cmx ${DIR_SRC}/abr.cmx
	$(CC) -o $@ $^

${DIR_SRC}/Util.cmx: ${DIR_SRC}/Util.ml
	$(CC) -c $^

${DIR_SRC}/abr.cmx: ${DIR_SRC}/abr.ml
	cd src; $(CC) -c abr.ml


ast.dot: $(EXEC)
	./build/abr > ast.dot

ast: ast.dot
	dot -Tpdf ast.dot -o ast.pdf

compressor.dot: $(EXEC)
	./build/abr > compressor.dot

compressor: compressor.dot
	dot -Tpdf compressor.dot -o compressor.pdf

compressorMap.dot: $(EXEC)
	./build/abr > compressorMap.dot

compressorMap: compressorMap.dot 
	dot -Tpdf compressorMap.dot -o compressorMap.pdf


.PHONY: clean test
clean:
	$(RM) $(DIR_SRC)/*.cmo $(DIR_SRC)/*.cmi $(DIR_SRC)/*.cmx
	$(RM) *.cmo *.cmi
	$(RM) $(EXEC)

test:
	$(EXEC)
