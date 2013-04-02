help:
	@echo "Targets:"
	@cat Makefile | grep -i '^[a-z]*:' | sed 's/^/    /' | sed 's/://'

newissue:
	open "https://github.com/sordina/RSSHit/issues/new"
