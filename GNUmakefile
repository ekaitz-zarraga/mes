.PHONY: all check default 
CFLAGS=-std=c99 -O3 -finline-functions
#CFLAGS=-g

default: all

all: mes boot.mes

check: all
	./mes.test
	./mes.test ./mes
	./mes < test.mes

boot.mes: mes.mes scm.mes test.mes
	cat $^ > $@

boot: all
	./mes < boot.mes

run: all
	./mes < test.mes
