# RTG development utility commands
#
# @file
# @version 0.1

GC_ROOTS=.nix-gc-roots/
SHELLS= ./default.nix nix/shell*.nix

roots: $(GC_ROOTS)
	for i in $(SHELLS); do \
		root=$(GC_ROOTS)/$(echo $$i | sed 's/.*\/\(.*\)\.nix/\1/'); \
		if [ ! -f $$root ]; then \
			nix-instantiate --add-root $$root $$i; \
		else \
			echo "GC root $$root already exists, skipping."; \
		fi \
	done

overwrite-roots: $(GC_ROOTS)
	for i in $(SHELLS); do \
		root=$(GC_ROOTS)/$(echo $$i | sed 's/.*\/\(.*\)\.nix/\1/'); \
		echo "Overwriting GC root $$root..."; \
		nix-instantiate --add-root $$root $$i; \
	done

clean:
	rm -f $(GC_ROOTS)/*

# end
