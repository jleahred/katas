.PHONY: nix
nix:
	nix-shell -p gleam rebar3 watchexec nodejs

.PHONY: run
run:
	watchexec -r -e gleam -E DEVELOPMENT=true -- gleam run -m lustre/dev start </dev/null
