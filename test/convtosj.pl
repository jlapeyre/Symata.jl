#!/usr/bin/env perl

# Convert test files written in Julia to SJulia
# This just means removing macros @ex and @testex.
# And using "T " to denote a test
# And doing a couple of symbol rewrites for binary infix operators.

# Note, there are several, mayb 10 or 20 things that were edited by
# hand after this conversion. So, if we rerun this and overwrite the
# files, we will break the things fixed by hand.
# Eg. plain Julia code in these test files has to be wrapped in :( ) or
# otherwise changed.

$outdir = "../sjtest";

@files = `ls *.jl`;

foreach $file (@files) {
    chomp($file);
    open my $fh , '<', $file;
    $ofile = $file;
    $ofile =~  s/\.jl$/.sj/;
    open my $oh , '>', "$outdir/$ofile" or die "cant open";
    print("Doing $file\n");
    while(<$fh>) {
        next if /using Base/;
        s/SJulia\.//g;
        s/\@testex/T/g;
        s/\@ex//g;
#        s/\=>/->/;  We can't use this because we want  sjulia >  f = :( (x) -> x^2 )
        s/\.>/:>/g;
        s/\@test/T/g;  # Some of these slipped in. Strange.
        print $oh $_;
    }
    close($fh);
    close($oh)    
}

