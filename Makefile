all:
	rebar compile

build:
	(rebar generate overlay_vars=vars/${ENV}.config overlay_vars=/etc/squadder/security.config)

clean:
	rebar clean

depends:
	rm -rf deps/*
	rebar get-deps

console: all build
	rel/hydra/bin/hydra console

cconsole: all build
	rel/hydra/bin/hydra console_clean