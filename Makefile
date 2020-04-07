.PHONY: site clean watch watch-pandoc watch-browser-sync

framework := bootstrap
framework_dir := bootstrap-4.4.1-dist

pandoc_args += --template style/template.html
pandoc_args += --css css/$(framework).css
pandoc_args += --css css/mods.css
pandoc_args += -t html5 -s --mathjax --toc
pandoc_args += --syntax-definition style/elm.xml
pandoc_args += --syntax-definition style/pure.xml
pandoc_args += --filter pandoc-test

pendulum_sources = $(shell find src -name '*.purs')

site: docs/index.html docs/css/$(framework).css docs/js/$(framework).js docs/css/mods.css docs/js/pendulum.js docs/js/double-pendulum.js

clean:
	rm -rf docs

watch:
	@tmux new-session make --no-print-directory watch-pandoc \; \
		split-window -v make --no-print-directory watch-browser-sync \; \
		split-window -v entangled ./lit/*.md \; \
		select-layout even-vertical \;

watch-pandoc:
	@while true; do \
		inotifywait -e close_write src style lit Makefile style/*; \
		make site; \
	done

watch-browser-sync:
	browser-sync start -w -s docs

docs/index.html: lit/pendulum.md style/template.html Makefile
	@mkdir -p docs
	pandoc $(pandoc_args) $< -o $@

docs/css/$(framework).css: style/$(framework_dir)/css/$(framework).css
	@mkdir -p docs/css
	cp $< $@

docs/css/mods.css: style/mods.css
	@mkdir -p docs/css
	cp $< $@

docs/js/$(framework).js: style/$(framework_dir)/js/$(framework).js
	@mkdir -p docs/js
	cp $< $@

docs/js/pendulum.js: $(pendulum_sources) Makefile
	spago bundle-app --main Pendulum --to $@

docs/js/double-pendulum.js: $(pendulum_sources) Makefile
	spago bundle-app --main DoublePendulum --to $@

