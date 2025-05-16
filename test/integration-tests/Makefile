all: test

.PHONY: clean test

clean:
	rm -rf ./node_modules

node_modules:
	nix-shell -p 'nodejs' --run 'npm install'

test: node_modules
	# PATH=$(dir $(GHC)):$(dir $(DEBUGGER)):$$PATH ./node_modules/.bin/mocha -f 'allow arbitrarily deep'
	# nix-shell -p nodejs --run 'PATH=$(dir $(GHC)):$(dir $(DEBUGGER)):$$PATH ./node_modules/.bin/mocha'
	nix-shell -p nodejs --run 'PATH=$(dir $(GHC)):$(dir $(DEBUGGER)):$$PATH ./node_modules/.bin/mocha --parallel'
