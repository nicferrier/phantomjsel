# package.el multi-file package install 

NAME=phantomjs
VERSION=0.0.8
PACKAGE=$(NAME)-$(VERSION)
TARBALL=$(PACKAGE).tar 
DOC="Control phantomjs from Emacs"			
package_parts = phantomjs.el phantomjs-pkg.el ghostweb.js

all: tarball


# Install the tarball in a test package store
test: tarball
	emacs -Q --batch -l ./packagedir.el -l ./build.el -- $(TARBALL)

# Install the tarball in the user's emacs
install: tarball
	emacs --batch --script ./build.el -- $(TARBALL)

clean:
	rm -rf .elpa
	rm -rf $(TARBALL)
	rm -rf $(PACKAGE) 
	rm -rf $(NAME)-pkg.el

tarball: $(TARBALL)

$(TARBALL): $(PACKAGE)
	tar cf $@ $<

$(PACKAGE): $(package_parts)
	mkdir $@
	cp $(package_parts) $@

$(NAME)-pkg.el:
	echo "(define-package \"$(NAME)\" \"$(VERSION)\" \"$(DOC)\")" > $@

# End
