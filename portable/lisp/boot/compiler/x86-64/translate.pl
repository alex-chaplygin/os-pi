$const_mode = 0;

while (<>) {
    if ($const_mode == 0) {
	s/-/_/g unless (/^-?\d+/);
	s/\*/_/g unless (/let\*/);
	s/ABS/_ABS/g;
    }
    if (/^CONSTB/) {
	$const_mode = 1;
	print "consts: \n";
    } elsif (/^CONSTE/) {
	$const_mode = 0;
	print "db 0\nENTRY\n";
    } elsif ($const_mode == 1) {
	chomp;
	s/\\(w|d|D|s|S|\+|\^|\*|\.|\?)/\\\\\1/g;
	s/#\\\\(\.|\?|w|d|D|s|S|\^|\*|\+)/#\\\1/g;
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
