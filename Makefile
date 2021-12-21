-include config.mk
BUILD_DIR = user-manual/

PORT = 8888

EMACS_BIN ?= emacs
EMACS = $(EMACS_BIN) -Q --batch
INDEX = index.org
ORGFILES = $(shell find . -name '*.org')

TANGLING_FILES = $(shell find . -name '*.org' | xargs grep -H tangle | awk -F: '{print $$1}')
TANGLING_FILES_DIR = .emacs/tangle
TANGLING_FILES_CACHE = $(patsubst %,$(TANGLING_FILES_DIR)/%,$(TANGLING_FILES))
$(TANGLING_FILES_DIR)/%: %
	mkdir -p $(@D)
	$(EMACS) $< -f org-babel-tangle && touch $@

publish: $(ORGFILES) tangle
	$(EMACS) --load config/site.el $(INDEX) -f cc4s/publish-site

tangle: $(TANGLING_FILES_CACHE)

force:
	touch $(ORGFILES)
	$(MAKE) publish

refresh:
	$(EMACS) --load config/site.el $(INDEX) -f package-refresh-contents

clean:
	rm -r $(BUILD_DIR)

clean-emacs:
	rm -r .emacs

clean-all: clean clean-emacs

serve:
	python3 -m http.server $(PORT)

.PHONY: init serve publish tangle refresh clean clean-emacs clean-all force
