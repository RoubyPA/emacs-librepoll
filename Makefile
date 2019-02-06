## Makefile --- Emacs-librepoll Makefile.

# Copyright (C) 2019  Pierre-Antoine Rouby

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

EMACS = emacs -Q -q --batch -nw -f package-initialize
EMACS_COMPILE = -f emacs-lisp-byte-compile

SOURCES = librepoll.el
COMPILED_FILE += $(SOURCES:.el=.elc)

INST_DIR = $(HOME)/.emacs.d/librepoll

.PHONY: clean install compile

all: compile

compile: $(SOURCES) $(COMPILED_FILE)

%.elc: %.el
	$(info Compiling    $@)
	@$(EMACS) $< $(EMACS_COMPILE)

install: compile
	$(info Install)
	@mkdir -p $(INST_DIR)
	@cp -v $(SOURCES) $(INST_DIR)/$(SOURCES)
	@cp -v $(COMPILED_FILE) $(INST_DIR)/$(COMPILED_FILE)
	@cp -v LICENSE $(INST_DIR)/LICENSE
	@cp -v README.org $(INST_DIR)/README.org
	@echo ""
	@echo "Is possible you need to add following line in your '.emacs':"
	@echo "        (load \"$(INST_DIR)/$(COMPILED_FILE)\")"



clean:
	$(info Removing)
	@rm -f *.elc -v

## Makefile ends here
