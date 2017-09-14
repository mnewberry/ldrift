ldrift
======

Quantify the strength of selection and drift in linguistic timeseries.

## Dependencies

### Software

The softwar requires a UNIX environment such as Linux or Mac OS X with the
following software, and required libraries:
- java
- R with libraries ggplot2, dplyr, grid, gridExtra
- tsinfer available at https://github.com/mnewberry/tsinfer
- sr available at: https://github.com/Schraiber/selection

The rhyming analysis additionally requires:
- The OCaml compiler (verified to work with ocaml 4.02.3)
- OCaml libraries pcre (verified to work with 7.1.6) and batteries (verified to
  work with 2.3.1)

### Corpora

- The Corpus of Historical American English (COHA), available from https://corpus.byu.edu/coha/
- The Penn Parsed Corpora of Historical English is available from http://www.ling.upenn.edu/hist-corpora/

## Execution

### Generating random drift

To generate random timeseries under drift, edit `simulate-bridge.sh` to inform
it of the location of `sr`, then run it.  

The specific data used for Figure 1 can be generated using the seed:

    ./sr -R -e 1329052268 -b 0.1,0.8,0,2

### Past-tense verbs

Symlink the directory containing the COHA distribution to `data/`, or edit
`make-past-tense.sh` (and `pastrhymes.ml` for rhyming analysis) to inform it of
the location of the directory containing COHA's part-of-speech-tagged files,
i.e. `COHA/Word_lemma_PoS/`.

Run `make-past-tense.sh` to generate `local/data/vvd-COHA`. (~10m)

Edit the first line of `past-tense.R` to specify the location of `tsinfer`,
then run it (as `Rscript past-tense.R`) to generate the
`local/out/past-tense-binned.csv` and `local/out/past-tense-results.csv` used
to produce Figure 2. (~1m)

To conduct the rhyming analysis, run `make-past-tense.sh` to retreive the CMU
rhyming dictionary.  Run `ocamlbuild -pkgs pcre,batteries pastrhymes.native` to
compile `pastrhymes.native`.  Run `pastrhymes.native` to generate the output,
`local/out/rhyme.schemes.tsv`, `local/out/rhyme.timeseries.vvd.tsv`, and
`local/out/rhyme.timeseries.tsv`.  (~60m)

### do-support

Follow the instructions in `make-do-support.README` to generate
`local/data/do-PPCHE`, which involve cloning another repository and linking it
to corpus data.

As with past-tense verbs, edit the first line of `do-support.R` to specify the location of `tsinfer`,
then run it (as `Rscript do-support.R`) to generate the
`local/out/do-support-binned.csv` and `local/out/do-support-results.csv` used
to produce Figure 3. (~1m)

### Negation

Follow the instructions in `make-negation.README` to generate
`local/data/neg-data.csv`, which involve cloning another repository and linking
it to corpus data.

As with past-tense verbs, edit the first line of `negation.R` to specify the
location of `tsinfer`, then run it (as `Rscript negation.R`) to generate the
`local/out/neg-binned.csv` and `local/out/neg-results.csv` used to produce
Figure 4. (<1m)


