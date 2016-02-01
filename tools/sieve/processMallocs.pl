#!/usr/bin/perl -w

# args:
#   backtrace file
#   malloc file
#   executable

my ($backTraceF, $mallocF, $execF) = @ARGV;

# read the backtrace file
open(my $BF, "<", $backTraceF) or die "cannot open < $backTraceF: $!";
my $stack = "X";
printf("Finding backtraces and stacks from $backTraceF.\n");
my $idx = 0;
while(defined($line = <$BF>)) {
    chop($line);
    if ($line =~ /VA:/) {
	if ($stack ne "X") {
	    push @{ $mCalls{$idx}}, $stack;  # add previous call stack to mCalls
	}
	$idx++;
	$stack = " ";
	my @ws = split(' ',$line);
	chop($ws[1]);
	my $desc = sprintf("MALLOC Address:%s Size:%s\n", $ws[1], $ws[3]);
	#print $desc;
	push @{$mAddrs{$ws[1]}}, $idx;
	push @{$mDesc{$idx}}, $desc;	
    } else {
	my $sysLine = "addr2line -e $execF $line";
	my @out = `$sysLine`;
	my $codeLoc = $out[0];
	if (!($codeLoc =~ /\?:0/)) { # strip out addres we couldn't find
	    $stack .= " ".$codeLoc; 
	}
    } 
}
push @{$mCalls{$idx}}, $stack;  # get the last one
close($BF);

printf(" captured %d mallocs\n", $idx); 

# read the malloc file
open(my $MF, "<", $mallocF) or die "cannot open < $mallocF: $!";
my $c = 0;
my $matched = 0;
printf("Matching backtraces and mallocs from $mallocF.\n");
$obj = myS->new();
while(defined($line = <$MF>)) {
    if (!($line =~ /#/)) {
	chop($line);
	#printf("$line\n");
	my @ws = split(' ',$line);
	
	if ($ws[3]+$ws[4] > 0) {  # only try to match accessed
				  # regions, because we get a lot of
				  # "double" reports, due to the
				  # _malloc()/malloc() being flagged
				  # twice
	    $c++;
	    if (defined($mAddrs{$ws[0]}) && scalar(@{$mAddrs{$ws[0]}}) > 0) {
		my $idx = $mAddrs{$ws[0]}[0];
		#printf("  matched %s %d %d\n", $ws[0], $mAddrs{$ws[0]}[0], scalar(@{$mAddrs{$ws[0]}}));

		$den = $ws[5];
		printf("  Malloc %d: %s", $c, $mDesc{$idx}[0]);
		printf("   Touches: %d\n", $ws[3]+$ws[4]);
		printf("   Density: %s\n", $den);
		printf("   Call Stack:\n%s", $mCalls{$idx}[0]);
		
		chop($mDesc{$idx}[0]);
		
		my $name = "m".$c;
		$obj->Name($name);
		$obj->Id($name, $c);
		$obj->Desc($name, $mDesc{$idx}[0]);
		$obj->Density($name, $den);
		$obj->Touches($name, $ws[3]+$ws[4]);
		$obj->Call($name, $mCalls{$idx}[0]);

		
		shift @{$mAddrs{$ws[0]}};
		shift @{$mDesc{$idx}};
		shift @{$mCalls{$idx}};
		$matched++;
	    } else {
		#printf("  no match %s\n", $ws[0]);
	    }
	}
    }
}
close($MF);

printf(" %d / %d matched\n", $matched, $c);

#sort by density
my @sortedItems = sort {$obj->Density($b) <=> $obj->Density($a)} $obj->NameList();

printf("Sorted by Density:\n");
$c = 1;
foreach my $i (@sortedItems) {
    printf("#%d. %s\n", $c++, $obj->Desc($i));
    printf(" Density: %s touches/byte\n", $obj->Density($i));
    printf(" Call Stack:\n");
    printf(" %s", $obj->Call($i));
}


########################################################################
package myS;


sub new {
    my $class = shift;

    my $self = {};

    bless $self, $class;
    return $self;
}

#
# Either adds a new name object or returns name object;
#
sub Name {
    my $self = shift;
    my $name = shift;

    if (not defined $self->{$name}) {
        $self->{$name}->{ID} = undef;
        $self->{$name}->{DEN} = -1;
        $self->{$name}->{DESC} = "";
	$self->{$name}->{TOUCHES} = -1;
	$self->{$name}->{CALL} = "";
    }
    return $self->{$name};
}

sub NameList {
    my $self = shift;

    return sort keys %{$self};
}

sub Id {
    my $self = shift;
    my $name = shift;
    my $id = shift;

    my $nameObj = $self->Name($name);
    if (defined $id) {
        $nameObj->{ID} = $id;
    }
    return $nameObj->{ID};
}

sub Desc {
    my $self = shift;
    my $name = shift;
    my $desc = shift;

    my $nameObj = $self->Name($name);
    if (defined $desc) {
        $nameObj->{DESC} = $desc;
    }
    return $nameObj->{DESC};
}

sub Density {
    my $self = shift;
    my $name = shift;
    my $den = shift;

    my $nameObj = $self->Name($name);
    if (defined $den) {
        $nameObj->{DEN} = $den;
    }
    return $nameObj->{DEN};
}

sub Touches {
    my $self = shift;
    my $name = shift;
    my $touches = shift;

    my $nameObj = $self->Name($name);
    if (defined $touches) {
        $nameObj->{TOUCHES} = $touches;
    }
    return $nameObj->{TOUCHES};
}

sub Call {
    my $self = shift;
    my $name = shift;
    my $call = shift;

    my $nameObj = $self->Name($name);
    if (defined $call) {
        $nameObj->{CALL} = $call;
    }
    return $nameObj->{CALL};
}

