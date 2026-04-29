$const_mode = 0;

while (<>) {
    s/-/_/g unless (/^-?\d+/);
    s/\*/_/g;
    s/ABS/_ABS/g;
    if (/^CONSTB/) {
	$const_mode = 1;
	print "consts: \n";
    } elsif (/^CONSTE/) {
	$const_mode = 0;
	print "db 0\nENTRY\n";
    } elsif ($const_mode == 1) {
	chomp;
	print "db '$_ '\n";
    } elsif (/\(([-\w]+)\)/) {
	print "$1\n";
    } elsif (/\(([-\w]+) (\d+)\)/) {
	print "$1($2)\n";
    } elsif (/\(([-\w]+) ([-*\w]+)\)/) {
	print "$1($2)\n";
    } elsif (/\(([-\w]+) ([-*\w]+)\)/) {
	print "$1($2)\n";
    } elsif (/\(([-\w]+) (\d+) (\d+)\)/) {
	print "$1 $2, $3\n";
    } elsif (/\(([-\w]+) ([-*\w]+) (\d+)\)/) {
	print "$1 $2, $3\n";
    } else {
	print;
    }
}
