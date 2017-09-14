ldrift
======

Quantify the strength of selection and drift in linguistic timeseries.

# Corpora



# Dependencies

The following software should be installed, along with any required libraries:

R : ggplot2, dplyr, grid, gridExtra

tsinfer: https://github.com/mnewberry/tsinfer

sr: https://github.com/Schraiber/selection

The data input files should be present -- either the COHA dataset accessible
from make.sh, or the make.sh output (local/data/vvd-COHA).

# Execution

If local/data/vvd-COHA is not present, run make.sh to generate it from the COHA
dataset.  Either edit make.sh to point to the correct path to COHA data
(COHA/Word_lemma_PoS), set the LDRIFT_PREFIX environment variable to point to
the COHA data, or create a symbolic link named 'data' pointing to the
Word_lemma_PoS directory.

When local/data/vvd-COHA exists, run Rscript past-tense-code.R

