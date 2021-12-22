-include config.mk
BUILD_DIR = user-manual/

PORT = 8888

EMACS_BIN ?= emacs
EMACS = $(EMACS_BIN) -Q --batch
INDEX = index.org
ORGFILES = $(shell find . -name '*.org')
ID_LOCATION_FILE = id-locations

SITEMAPS = algorithms/sitemap.org objects/sitemap.org

TANGLING_FILES = $(shell find . -name '*.org' | xargs grep -H tangle | awk -F: '{print $$1}')
TANGLING_FILES_DIR = .emacs/tangle
TANGLING_FILES_CACHE = $(patsubst %,$(TANGLING_FILES_DIR)/%,$(TANGLING_FILES))
$(TANGLING_FILES_DIR)/%: %
	mkdir -p $(@D)
	$(EMACS) $< -f org-babel-tangle && touch $@

publish: $(ORGFILES) tangle sitemaps
	$(EMACS) --load config/site.el $(INDEX) -f cc4s/publish-site

tangle: $(TANGLING_FILES_CACHE)

force:
	touch $(ORGFILES)
	$(MAKE) publish

refresh:
	$(EMACS) --load config/site.el $(INDEX) -f package-refresh-contents

clean:
	rm -r $(BUILD_DIR) $(ID_LOCATION_FILE) sitemap.org .emacs/org-timestamps* \
		$(SITEMAPS)

clean-emacs:
	rm -r .emacs

algorithms/sitemap.org: $(shell find algorithms/ | grep -v sitemap.org)
	./tools/create-sitemap.sh "Algorithm List" algorithms/ > $@

objects/sitemap.org: $(shell find objects/ | grep -v sitemap.org)
	./tools/create-sitemap.sh "Object List" objects/ > $@

sitemaps: $(SITEMAPS)


clean-all: clean clean-emacs

serve:
	python3 -m http.server $(PORT)

vim:
	mkdir -p ~/.vim/ftdetect
	mkdir -p ~/.vim/syntax
	wget https://raw.githubusercontent.com/alejandrogallo/org-syntax.vim/main/ftdetect/org.vim \
				-O ~/.vim/ftdetect/org.vim
	wget https://raw.githubusercontent.com/alejandrogallo/org-syntax.vim/main/syntax/org.vim \
				-O ~/.vim/syntax/org.vim

.PHONY: sitemaps
.PHONY: init serve publish tangle refresh clean clean-emacs clean-all force vim
