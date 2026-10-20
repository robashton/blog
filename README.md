This is my blog, it's a bloody mess, I don't really care.

It's 2018 and it's still a bloody mess, I hope it works.

It's 2026 and it's a Makefile now.

    nix develop
    npm install
    make                                        # rebuild whatever is out of date into docs/
    make serve                                  # http://localhost:8000
    make new TITLE="Some title" [DATE=yyyy-mm-dd]

Posts live in `input/pages/<slug>/` (`meta.json`, `content.markdown`, `comments.json`; the old
ones are `content.html`). The URL comes from the title in `meta.json`, not the folder name, so
renaming a folder is harmless and renaming a title is not. Anything dated in the future doesn't
get built until the first `make` after that date.

`input/static/` is the projects/talks/etc pages, `input/source/` the templates, `input/raw/` gets
copied as-is, and `docs/css/site.css` is the stylesheet. `docs/` is what GitHub Pages serves,
so commit it and push. Nothing runs on the server.
