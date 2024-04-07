.PHONY: nix
nix:
	nix-shell -p gleam rebar3 watchexec nodejs

.PHONY: dev
dev:
	watchexec -r -e gleam -E DEVELOPMENT=true -- gleam run -m lustre/dev start </dev/null

.PHONY: build
build:
	gleam run -m lustre/dev build app	
