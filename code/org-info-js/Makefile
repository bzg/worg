YUI=~/bin/yuicompressor-2.4.2.jar

MINIFY_OTPIONS=	 --preserve-semi --line-break 80

VERSION = `grep -e '\* Version: *[0-9.]' org-info-src.js | cut -sd ':' -f 2-`
TMPv = tmp-version.js
TMPs = tmp-min.js

all: minify


minify: version sed.txt
	@if [ -f $(TMPv) ] &&  [ -f $(TMPs) ]; then \
	  sed -f sed.txt $(TMPv) > $(TMPs); \
	  java -jar $(YUI) $(MINIFY_OTPIONS) $(TMPs) > org-info.js; \
	  rm $(TMPv); \
	  rm $(TMPs); \
	  echo "org-info.js successfully built."; \
	else \
	  echo "Failed to build. $(TMPv) and/or $(TMPs) are missing!"; \
	fi

version:
	@if [ -f $(TMPv) ] ||  [ -f $(TMPs) ]; then \
	  echo "$(TMPv) and/or $(TMPs) exist. Please remove them or adjust the Makefile!"; \
	else \
	  sed -e "s/###VERSION###/$(VERSION)/g" org-info-src.js > $(TMPv); \
	  touch $(TMPs); \
	fi

clean:
	@rm -f $(TMPv) $(TMPs)
	@echo "Temporary files removed."

