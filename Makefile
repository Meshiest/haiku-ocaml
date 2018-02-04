OCB_FLAGS = -lib str
OCB = ocamlbuild $(OCB_FLAGS)

all: clean build

clean:
	$(OCB) -clean

run:
	./haiku.byte

build:
	$(OCB) haiku.byte

