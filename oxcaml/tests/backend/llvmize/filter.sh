#!/bin/sh

# remove trailing whitespace at the ends of lines
sed "s/[ \t]*\n/\n"/ | \
sed "s/[ \t]*$//" | \

# hide flaky parts of identifiers
sed -r 's/_O(.*)_[0-9]+(_code)?/_O\1_HIDE_STAMP/' | \
# sed 's/__/./' | \

# hide target triple
sed -r 's/target triple[^\n]*//' | \

# hide calling convention (which depends on fp config)
sed -r 's/(oxcaml_fpcc|oxcaml_nofpcc)/oxcamlcc/'
