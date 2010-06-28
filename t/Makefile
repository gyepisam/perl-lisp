.PHONY: test clean

EQRLIB=/home/csc/webhome/equationresearch/library
TESTFILES=./*.t


test:
	env PERL5LIB=$(EQRLIB) prove *.t

clean:
	rm -f *~
