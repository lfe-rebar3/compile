make:
	rebar3 compile

clean:
	rm -rf .rebar .rebar3 deps _build rebar.lock ebin/*

push:
	git push github master
	git push gitlab master

push-tags:
	git push github --tags
	git push gitlab --tags

push-all: push push-tags

build-github: clean
	rebar3 compile

build-gitlab: clean
	rebar3 as gitlab compile

build-hexpm: clean
	rebar3 as hexpm compile

build-all: build-github build-gitlab build-hexpm

publish: clean
	rebar3 as hexpm hex publish

