
TOP=..
include ../top.mk

all:
	$(GHC) --make -o test hs.hs cmm.cmm c.c -dcmm-lint -fforce-recomp
	./test

a:
	$(GHC) -O3 -c cmm.cmm -ddump-asm
