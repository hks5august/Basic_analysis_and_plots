#!/usr/bin/perl -w
#system "perl ~/progs/time.pl";
if($ARGV[0] eq ""){print "\nUsage:   perl hashmatch.pl test1 test2 colnum1 colnum2\n\n";}
$i=$ARGV[2] - 1;$j=$ARGV[3] - 1; chomp $i;chomp $j;
open(FH,"$ARGV[1]");
while($line=<FH>){
	chomp $line;
	@arr=split("\t",$line);
	push(@{$hash{$arr[$j]}},"$line");
}
open(FH1,"$ARGV[0]");
while($line1=<FH1>){
	chomp $line1;@arr1=split("\t",$line1);
	if(exists($hash{$arr1[$i]})){
		foreach $element(@{$hash{$arr1[$i]}}){
			print "$element\t##\t$line1\n";
		}			
	}	   
}
#system "perl ~/progs/time.pl";
