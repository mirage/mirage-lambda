.PHONY: test all

all:
	jbuilder build --dev

test:
	jbuilder runtest --dev
