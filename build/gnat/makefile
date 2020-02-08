TOP			=	../..
SRC			=	$(TOP)/src
DOC_SRC		=	$(TOP)/doc

GEN			=	gen
DOC_GEN		=	$(GEN)/doc

DESTDIR		?=	/opt

GPRBUILD	=	gprbuild
GPRINSTALL	=	gprinstall
GPRCLEAN	=	gprclean
INSTALL		=	install
CAT			=	cat

UCA_MODE	?=	debug

# Sources for creating tags from.
TAG_SRCS	=	$(SRC)/uca.ads #\
				$(SRC)/uca-iterators.ads

########################################################################################################################

all: uca.gpr #tests.gpr

.PHONY: uca.gpr tests.gpr

# Add lib targets here.
uca.gpr:
	$(GPRBUILD) -p -XUCA_MODE=$(UCA_MODE) -Puca.gpr

########################################################################################################################
# Unit tests

tests: all tests.gpr 
	./gen/debug/tests/uca-test_runner

tests.gpr:
	$(GPRBUILD) -p -XUCA_MODE=$(UCA_MODE) -Ptests.gpr

# Add clean lib targets here

.PHONY: clean

clean:
	$(GPRCLEAN) -Puca.gpr
	$(GPRCLEAN) -Ptests.gpr

########################################################################################################################

.PHONY: todo

todo:
	grep -riw --color TODO $(SRC)

#########################################################################################################################

