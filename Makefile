PORT = 8888
GOLLUM = \
gollum --mathjax --port $(PORT)

init:
	type -a gollum || gem install gollum

serve:
	$(GOLLUM) . &
	xdg-open http://127.0.0.1:$(PORT)

.PHONY: init serve
