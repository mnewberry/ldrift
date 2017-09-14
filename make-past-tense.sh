#!/bin/bash

# Specify the path to directory containing POS-tagged COHA files
LDRIFT_PREFIX=${LDRIFT_PREFIX:-data/COHA/Word_lemma_PoS/}
  # -change this if you have COHA somewhere else other than this
  # relative directory

OUT=local/data/vvd-COHA
mkdir -p local/data/
echo "Writing tokens from $LDRIFT_PREFIX to $OUT ..."

# Write header to output file
echo -e "genre\tyear\tid\ttoken\tlemma" > $OUT

# 1. Find all verbs in COHA with past tense VVD as the most likely POS tag
  # This does not preclude other potential tags, e.g. Tag1_Tag2_Tag3. Each
  # subsequent tag has at least a 10% chance according to the CLAWS
  # tagger (p.c. Mark Davies). For a more conservative approach, we can use 
  # only those tokens where we are  more than 90% confident that VVD is the
  # right tag by changing the second grep command to something like 
  #		grep -av "_vvd\|vvd_"
  # This will exclude cases where there is less than a 90% change of being
  # the right POS tag.
# 2. Filter out './' and '.txt:' from grep output and suffix with tab.
# 3. Remove the PoS column
  # -Could possible leave these for better information
# 4. Lower case everything
# 5. Write to output
find $LDRIFT_PREFIX -name "*.txt" -type f \
      -execdir grep -aH "vvd" {} \; \
  | sed "s/^\.\///" | sed "s/\.txt:/\t/" \
  | grep -av "_vvd" | sed "s/_/\t/g" \
  | cut -f 1-5 \
  | tr '[:upper:]' '[:lower:]' \
  >> $OUT

sed -i 's/smelt\tsmelt$/smelt\tsmell/' $OUT
sed -i 's/clove\tclove$/clove\tcleave/' $OUT
