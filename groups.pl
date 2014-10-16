#!/usr/bin/perl
open(FILE, $ARGV[0]);
while (<FILE>)
{ 
$txt = $_;

# This ugly regex searches for one or more country codes followed by a /, followed by another country code
# It allows for numerical footnotes to intervene
$regex='(?:(?:AU|BN|CA|CL|JP|MX|MY|NZ|PE|SG|US|VN)[0-9]*\/)+(?:AU|BN|CA|CL|JP|MX|MY|NZ|PE|SG|US|VN)[0-9]*';

	print "$_\n" for @match = $txt =~ m/$regex/g;
}

# Remember the output needs to be stripped of numbers, easily done using tr
# e.g., tr -d '[0-9]' < groups.txt > groups_nofn.txt
# Then convert to CSV with tr '/' ',' < groups_nofn.txt > groups.csv
