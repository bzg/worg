YUI= ~/bin/yuicompressor-2.4.2.jar

MINIFY_OTPIONS=	 --preserve-semi --line-break 80

all: org-info-src.js
	java -jar $(YUI) $(MINIFY_OTPIONS) org-info-src.js > org-info.js
