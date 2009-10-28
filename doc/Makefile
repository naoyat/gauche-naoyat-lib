.PHONY: all gauche-naoyat-lib-refj.html

#TEXI2HTML = /usr/bin/texi2html ## 1.70
TEXI2HTML = /opt/local/bin/texi2html ## MacPorts, 1.82

TEXI_FILES = gauche-naoyat-lib-refj.texi \
	list.texi vector.texi \
	printf.texi matrix.texi math.texi \
	binary.texi graph.texi sound.texi \
	mac.texi test.texi misc.texi 

all: gauche-naoyat-lib-refj.html

gauche-naoyat-lib-refj.html: $(TEXI_FILES)
	$(TEXI2HTML) --init-file=texi2html.init --number --lang=ja gauche-naoyat-lib-refj.texi &> /dev/null

clean:
	rm -f *~ gauche-naoyat-lib-refj.html

