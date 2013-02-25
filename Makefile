all: build

build: hakyll
	./site build

hakyll: site.hs
	ghc --make site.hs
	./site clean

new:
	@./new_post.sh

publish: build
	git add .
	git stash save
	git checkout publish
	find . -maxdepth 1 ! -name '.' ! -name '.git*' ! -name '_site' -exec rm -rf {} +
	find _site -maxdepth 1 -exec mv {} . \;
	rmdir _site
	git add -A && git co "Publish" || true
	git push
	git push clever publish:master
	git checkout master
	git clean -fdx
	git stash pop || true

preview: hakyll
	./site clean
	./site preview -p 9000

clean: hakyll
	./site clean
	rm -f site

check: hakyll
	./site check
