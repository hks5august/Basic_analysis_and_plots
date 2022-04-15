#! /bin/sh
# Transpose a matrix: assumes all lines have same number
# of fields
#input file should be tab separated (csv) and output file contains spaces as delimiter
#usage: chmod tpose.sh
#./tpose.sh input >output
exec awk -F"," '
NR == 1 {
	n = NF
	for (i = 1; i <= NF; i++)
	row[i] = $i
	next
}
{
if (NF > n)
	n = NF
	for (i = 1; i <= NF; i++)
		row[i] = row[i] "," $i
}
END {
	for (i = 1; i <= n; i++)
	print row[i]
}' ${1+"$@"}
