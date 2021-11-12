PORT = 8888
GOLLUM = \
gollum --mathjax --port $(PORT)

EMACS = emacs -Q --batch
INDEX = index.org
ORGFILES = $(shell find . -name '*.org')

publish: $(ORGFILES)
	$(EMACS) --load config/site.el $(INDEX) -f cc4s/publish-site

init:
	type -a gollum || gem install gollum

serve:
	python3 -m http.server $(PORT)

.PHONY: init serve publish
