#!/usr/bin/perl
open(FILE, $ARGV[0]);
while (<FILE>)
{ 
$txt = $_;

# This ugly regex searches for one or more country codes followed by a mandatory /, space, or colon, followed by another optional country code
# Differs from groups.pl in that here we capture single country codes, which may be useful for measure of isolation
# It allows for numerical footnotes to intervene
$regex='(?:(?:AU|BN|CA|CL|JP|MX|MY|NZ|PE|SG|US|VN)[0-9]*[\/ :]+)+(?:(?:AU|BN|CA|CL|JP|MX|MY|NZ|PE|SG|US|VN)[0-9]*)*';

	print "$_\n" for @match = $txt =~ m/$regex/g;
}

# Remember the output needs to be stripped of numbers, easily done using tr
# e.g., tr -d '[0-9]' < countries.txt > countries_nofn.txt
# Then convert to CSV with tr '/' ',' < countries_nofn.txt > countries.csv
