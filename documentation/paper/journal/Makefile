CUTFROM=201
CUTTO=257
FILE=tes

default: compile

sml-tes-diagram.eps: sml-tes-diagram.dia
	dia --export=maximum.eps sml-tes-diagram.dia

compile: sml-tes-diagram.eps
	rubber -ps ${FILE}

pdf: compile
	ps2pdf -sPAPERSIZE=a4 ${FILE}.ps

all:
	make

clean:
	rm ${FILE}.aux\
           ${FILE}.bbl\
           ${FILE}.blg\
           ${FILE}.dvi\
           ${FILE}.log\
           ${FILE}.pdf\
           ${FILE}.ps\
           ${FILE}.toc\
           ${FILE}.lof\
           ${FILE}.idx\
           ${FILE}.ilg\
           ${FILE}.ind\
           ${FILE}.out

cleanAll:
	make clean

cut:
	pdftk ${FILE}.pdf cat ${CUTFROM}-${CUTTO} output ${FILE}-cut.pdf