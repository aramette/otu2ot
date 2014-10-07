print "the program must be copied in the right DIR!!!\n";
#"put all filenames ending by .txt in an array
#"open each file in the array\n";
	#search for the expression
	#if match, save the filenames
	#determine at which lines it happens
#do it for multiple hits in the same file
#filename: line...

####get all TXT files into an array

opendir(DIR, ".");
@files = grep(/\./,readdir(DIR));	#@files contains all files .txt for TXT files
closedir(DIR);

######## search in the file names only

sub filenam {
print "\n"; 
print "\nthe string is found in the filenames of:\n"; 
$count=0;
foreach $file (@files) {
   if ($file=~ m/$pat/i) {print "$file\n";$count++;
   }   # end of if
 } # end of foreach
print "Nber of elements found: $count\n";

} #end of sub

################ search in each file 
#report on the lines in each file
#take each file in the DIR, open it, grep it, report if found using an array

sub withinfile{
@filepositive=();
 
   foreach $f (@files) {	#for each TXT file in the DIR
	$Nlines=0;	
	$Nentry=0;
	@start=(); 
	
        open(FILE,$f);
	while($line =<FILE>) {			#for each line in the FILE
		$Nlines++;			#for each line add one unit
		if($line =~ /$pat/i) {
		push(@filepositive,$f);
		print "Found in file: $f\t on line: $Nlines\n";		#report that is found in the file
		push(@start,$Nlines);		#@start contains the lines where the pattern was found		
		} #END If

	} #end of while
        close(FILE);
     } #end of foreach

} #end of sub
############## MAIN SCRIPT
print "Search in all names (a) or in each file (b) in this directory?..... ";
$dec=<STDIN>;
chomp $dec;
print "String to search for in all files in this directory?..... ";
$pat=<STDIN>;
chomp $pat;

if  ($dec eq 'a') {&filenam;}
elsif ($dec eq 'b') {&withinfile;}
else {print "error!\n";}
<>;
