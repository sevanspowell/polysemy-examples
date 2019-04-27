# Your ~/.ssh/nixbuild.config should look something like this:
# Host stash
#  StrictHostKeyChecking No
#  UserKnownHostsFile /dev/null
#  IdentityFile /home/steve/.ssh/nixbuild_rsa
#
# And that key should be a read-only, passwordless key.
#
# You also want your nix sandbox paths setup:
#
# NixOS /etc/nixos/configuration.nix: nix.sandboxPaths = ["/home/steve/.ssh"];
# Nix   /home/steve/.config/nix/nix.conf: sandbox-paths = ["/home/steve/.ssh"];
.PHONY: nix-shell
nix-shell:
	nix-shell -I ssh-config-file=$$HOME/.ssh/nixbuild.config --show-trace --pure

.PHONY: check-nix-shell
check-nix-shell:
	$(info Checking if running in a nix-shell)
ifndef IN_NIX_SHELL
	$(error Not in a nix-shell. Please run nix-shell and run make from there)
endif

.PHONY: hoogle
hoogle: check-nix-shell
	hoogle server --local -p 8080 &> /tmp/hoogle.log

.PHONY: lint
lint: check-nix-shell
	hlint lint src

.PHONY: clean
clean: check-nix-shell
	cabal new-clean

.PHONY: build
build: clean
	cabal new-build --ghc-option='-Werror'

.PHONY: test
test: build
ifneq "$(COVERAGE)" ""
	cabal new-configure --enable-tests --enable-coverage
endif
	cabal new-test --ghc-option='-Werror'

.PHONY: doctests
doctests:
	cabal new-test doctests --ghc-option='-Werror'

.PHONY: run
run: build
	cabal new-run

.PHONY: cabal-configure
cabal-configure: check-nix-shell
	cabal new-configure --enable-tests

.PHONY: repl
repl: cabal-configure
	cabal new-repl

.PHONY: interactive
interactive: cabal-configure
	ghcid -c "cabal new-repl --ghc-option='-Wwarn' lib:polysemy-examples"
