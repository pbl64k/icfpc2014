#!/usr/bin/perl

use warnings;
use strict;

system('./m.sh');

for my $FN (<*.scm>)
{
    print('Building '.$FN.'...'."\n");
    my $TN = $FN;
    $TN =~ s/\.scm$/.gcc/;
    system('./scimitar.exe <'.$FN.' >'.$TN);
}

for my $FN (<*.ghc80>)
{
    print('Building '.$FN.'...'."\n");
    my $TN = $FN;
    $TN =~ s/\.ghc80$/.ghc/;
    system('./ghc80.exe <'.$FN.' >'.$TN);
}

print('Done.'."\n");

1
