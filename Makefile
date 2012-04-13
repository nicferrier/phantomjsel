# package.el multi-file package install 

NAME=phantomjs
VERSION=0.0.8
DOC="Control phantomjs from Emacs"			
package_parts = phantomjs.el phantomjs-pkg.el ghostweb.js

all: $(NAME)-$(VERSION).tar


clean:
	rm -rf $(NAME)-$(VERSION).tar 
	rm -rf $(NAME)-$(VERSION) 
	rm -rf $(NAME)-pkg.el

$(NAME)-$(VERSION).tar: $(NAME)-$(VERSION)
	tar cf $@ $<

$(NAME)-$(VERSION): $(package_parts)
	mkdir $@
	cp $(package_parts) $@

$(NAME)-pkg.el:
	echo "(define-package \"$(NAME)\" \"$(VERSION)\" \"$(DOC)\")" > $@

# End
