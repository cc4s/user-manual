BUILD_DIR = user-manual/

PORT = 8888

EMACS = emacs -Q --batch
INDEX = index.org
ORGFILES = $(shell find . -name '*.org')

TANGLING_FILES = $(shell find . -name '*.org' | xargs grep -H tangle | awk -F: '{print $$1}')
TANGLING_FILES_DIR = .emacs/tangle
TANGLING_FILES_CACHE = $(patsubst %,$(TANGLING_FILES_DIR)/%,$(TANGLING_FILES))
$(TANGLING_FILES_DIR)/%: %
	mkdir -p $(@D)
	$(EMACS) $< -f org-babel-tangle && touch $@
tangle: $(TANGLING_FILES_CACHE)

publish: $(ORGFILES) tangle
	$(EMACS) --load config/site.el $(INDEX) -f cc4s/publish-site

refresh:
	$(EMACS) --load config/site.el $(INDEX) -f package-refresh-contents

clean:
	rm -r .emacs/ $(BUILD_DIR)

serve:
	python3 -m http.server $(PORT)

.PHONY: init serve publish tangle refresh
