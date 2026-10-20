# It's 2026 and it's a Makefile now. `make` rebuilds whatever is out of date into docs/,
# which is what GitHub Pages serves. Nothing runs on the server, commit docs/ and push.

NODE ?= node
SITE := $(NODE) tools/site.js

# The post index (and each post's older/newer links) is refreshed on every run, the files are
# only rewritten when something actually changed so the rules below can trust their timestamps
_ := $(shell $(SITE) index >&2)

POST_SRC := $(wildcard input/pages/*/content.markdown input/pages/*/content.html input/pages/*/comments.json input/pages/*/meta.json)
NAV      := $(wildcard build/nav/*.json)
LAYOUT   := input/source/layout.html tools/site.js
STATICS  := $(patsubst input/static/%.markdown,docs/%.html,$(wildcard input/static/*.markdown)) \
            $(patsubst input/static/%.html,docs/%.html,$(wildcard input/static/*.html))
RAW      := $(patsubst input/raw/%,docs/%,$(wildcard input/raw/*))

.PHONY: all rebuild serve new
all: build/posts.stamp docs/index.html docs/blog.html docs/rss.xml build/redirects.stamp $(STATICS) $(RAW)

# All posts in one go, but only the ones whose files are in $? get re-rendered
build/posts.stamp: $(POST_SRC) $(NAV) $(LAYOUT) input/source/post.html
	@$(SITE) posts $?
	@touch $@

docs/index.html: build/index.json input/source/home.html $(LAYOUT)
	$(SITE) home

docs/blog.html: build/index.json input/source/blog.html $(LAYOUT)
	$(SITE) blog

docs/rss.xml: build/index.json $(POST_SRC) tools/site.js
	$(SITE) rss
	@touch $@

build/redirects.stamp: build/index.json tools/site.js
	$(SITE) redirects
	@touch $@

docs/%.html: input/static/%.markdown $(LAYOUT)
	$(SITE) static $< $@

docs/%.html: input/static/%.html $(LAYOUT)
	$(SITE) static $< $@

docs/%: input/raw/%
	cp $< $@

rebuild:
	rm -rf build
	$(MAKE) all

# make new TITLE="Why everything is rubbish" [DATE=2026-11-03]
# A date in the future just means it won't appear until the first `make` after that date
new:
	$(SITE) new "$(TITLE)" $(DATE)

serve: all
	$(NODE) tools/serve.js
