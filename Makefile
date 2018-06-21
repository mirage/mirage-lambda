.PHONY: test all clean

all:
	jbuilder build --dev

clean:
	jbuilder clean

test:
	jbuilder runtest --dev

block:
	dd if=/dev/zero of=disk.img count=1024
