#!/bin/bash
# This quickly removes the pipe deliminited section, replacing it with tabs. No idea why it's like that
# but trying to import into R and then not keeping a tidy table or splitting it twice is needless complexity

# Okay, so parsing the bacteria/archaea and keeping a 'nice' 8 level taxonomy is too specific
# not sure why Fred uses this format but it's bloody annoying! There are standards for this...
# anyway, since we're not that interested in bacteria for this project, I'll just remove them
# from all levels except super_kingdom :) This means no distinction at other levels...

## It also inserts tab spaces in to the Bacteria/Archaea
## As 8 and 4 taxonomy levels, respectively, adds needless complexity
## NA also confounds reading in R (or any viewer), so we need to fill those columns up to match
## *|*|*|* also needs expanding to * * * * * * * *
## Special cases in Bacteria and Archaea - Synecococcus sp....

#0f722ca96578378f1ac226ea1c547ea667fb6a3c in Sample_13 has 6 '*' not 4 or 8 just y'know
# to be absolutely f'ing annoying.

for i in {1..17}
do
    file1=`ls -1 $i\_*.csv`
    echo "Formatting Table $file1"
    # Remove all pipe '|' symbols
    sed 's/|/\t/g' ${file1} > ${file1}\_out
    # Add eight levels to Bacteria
    sed -i 's/\tBacteria\t.*\t.*\t.*\t/\tBacteria\tBacteria\tBacteria\tBacteria\tBacteria\tBacteria\tBacteria\tBacteria\t/' ${file1}\_out
    # Add eight levels to Archaea
    sed -i 's/\tArchaea\t.*\t.*\t.*\t/\tArchaea\tArchaea\tArchaea\tArchaea\tArchaea\tArchaea\tArchaea\tArchaea\t/' ${file1}\_out
    # Do the same for not applicable? - There are ten here because they are also missing %ID and Accession
	sed -i 's/\tNA\tNA\tNA/\t0.0\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA\tNA/g' ${file1}\_out
	# And the same for where there are 4 '*' - presumably unassigned at that level...but Arch/Bac or why not 8 stars!? FFS
	# To make this easier - make all 8 '*' to 4 '*' and then back to 8 '*'
	# We have to make a case not to pick up the other 4 '*' after assigend taxonomies... EUGH!
	# Hence the grouping and keeping of the %ID column, to force 4 '*' groups without Taxonomies...
	# note how sed does not have '\d+' you have to use '[[:digit:]]\+' instead ! WTF
	sed -i 's/\t\*\t\*\t\*\t\*\t\*\t\*\t\*\t\*\t/\t\*\t\*\t\*\t\*\t/' ${file1}\_out
	sed -i 's/\([[:digit:]]\+\.[[:digit:]]\+\)\t\*\t\*\t\*\t\*\t/\1\t\*\t\*\t\*\t\*\t\*\t\*\t\*\t\*\t/g' ${file1}\_out
done