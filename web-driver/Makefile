default:
	make clean
	make all
	make stats

all: haste/stuff.js ghcjs/stuff.js fay/stuff.js

clean:
	rm -rf haste/*
	rm -rf fay/*
	rm -rf ghcjs

stats:
	@echo ">>> Raw sizes"
	@du -h haste/stuff.js
	@du -h ghcjs/stuff.js
	@du -h fay/stuff.js
	@echo ">>> Closure compiler sizes"
	ccjs haste/stuff.js --compilation_level=ADVANCED_OPTIMIZATIONS > haste/stuff-ccjs.js 2> /dev/null
	ccjs ghcjs/stuff.js --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > ghcjs/stuff-ccjs.js 2> /dev/null
	ccjs fay/stuff.js --compilation_level=ADVANCED_OPTIMIZATIONS > fay/stuff-ccjs.js 2> /dev/null
	@du -h haste/stuff-ccjs.js
	@du -h ghcjs/stuff-ccjs.js
	@du -h fay/stuff-ccjs.js
	@echo ">>> GZip sizes"

	@rm -f haste/stuff-ccjs.js.gz
	@rm -f ghcjs/stuff-ccjs.js.gz
	@rm -f fay/stuff-ccjs.js.gz

	@gzip -k haste/stuff-ccjs.js
	@gzip -k ghcjs/stuff-ccjs.js
	@gzip -k fay/stuff-ccjs.js
	@du -h haste/stuff-ccjs.js.gz
	@du -h ghcjs/stuff-ccjs.js.gz
	@du -h fay/stuff-ccjs.js.gz

	@echo ">>> Zopfli sizes"

	@rm -f haste/stuff-ccjs.js.gz
	@rm -f ghcjs/stuff-ccjs.js.gz
	@rm -f fay/stuff-ccjs.js.gz

	@zopfli -i1000 -k haste/stuff-ccjs.js
	@zopfli -i1000 -k ghcjs/stuff-ccjs.js
	@zopfli -i1000 -k fay/stuff-ccjs.js
	@du -h haste/stuff-ccjs.js.gz
	@du -h ghcjs/stuff-ccjs.js.gz
	@du -h fay/stuff-ccjs.js.gz

	@echo ">>> JavaScript sizes"
	echo "console.log('Hello World')" > stuff.js
	@rm stuff.js.gz
	@du -h stuff.js
	@gzip -k stuff.js
	@du -h stuff.js.gz

fay/stuff.js: stuff.hs
	time stack exec fay stuff.hs
	mv stuff.js fay/stuff.js

haste/stuff.js: stuff.hs
	time hastec stuff.hs --outdir=./haste
	rm -f *.hi
	rm -f *.o
	mv stuff.js haste/

ghcjs/stuff.js: stuff.hs
	time ghcjs stuff.hs
	rm -f *.js_hi
	rm -f *.js_o
	mv stuff.jsexe ghcjs
	mv ghcjs/all.js ghcjs/stuff.js
