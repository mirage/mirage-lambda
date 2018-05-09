.PHONY: test all clean

all:
	jbuilder build --dev

clean:
	jbuilder clean

test:
	jbuilder runtest --dev
