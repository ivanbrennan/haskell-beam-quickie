all: shoppingcart.db quickie

shoppingcart.db: db-setup.sql
	sqlite3 shoppingcart.db '.read db-setup.sql'

quickie: quickie.hs
	ghc --make quickie.hs -no-keep-hi-files -no-keep-o-files

.PHONY: watch
watch:
	ghcid "--command=ghci quickie.hs"

.PHONY: clean
clean:
	rm -f shoppingcart.db
	rm -f quickie
