#------------------------------------------------------------
# Makefile para producir el texto de la tesis en formato en .pdf y .odt
#
# Fuente:
# https://www.innoq.com/en/blog/markdown-with-zotero-workflow
#
# Extensiones para Pandoc:
# https://pandoc.org/MANUAL.html#extension-rebase_relative_paths
#
#------------------------------------------------------------

OUTPUT=out

SOURCE=dedicatoria.md \
	   intro/intro.md \
	   cap*/*.md \
	   glosario.md \
	   bibliografia.md

CITATIONSTYLE=maqueta/chicago-author-date.csl

BIBLIO=maqueta/fuentes.bib

DEFAULTS=maqueta/defaults.yaml

TARGET=$(OUTPUT).odt $(OUTPUT).pdf



PANDOC=pandoc \
			 --from=markdown+rebase_relative_paths \
			 -s --bibliography $(BIBLIO) \
			 --csl=$(CITATIONSTYLE) \
			 --defaults $(DEFAULTS) \
			 $(SOURCE)


# create odt
$(OUTPUT).odt: $(SOURCE) $(METADATA)
	$(PANDOC) --output=$(OUTPUT).odt

# create pdf
$(OUTPUT).pdf: $(SOURCE) $(METADATA)
	$(PANDOC) --output=$(OUTPUT).pdf


all: $(TARGET)

clean:
	rm -rf $(OUTPUT).odt $(OUTPUT).pdf


.PHONY: all clean $(TARGET)
.DEFAULT_GOAL := all
