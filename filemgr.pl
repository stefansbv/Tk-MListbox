#!/usr/bin/perl -w

# MListbox demonstration application.

# Author: Hans J. Helgesen, December 1999.
#
# Before March 2000:
#
# Please send comments, suggestions and error reports to 
# hans_helgesen@hotmail.com.
#
# From March 2000: hans.helgesen@novit.no
#
use Tk;
use Tk::MListbox;


my $intro = <<EOT;
This is a very simple file manager application that demonstrates the use of MListbox.

* To resize any of the columns, drag the vertical bar to the RIGHT of the column.
* To move any of the columns, drag the column header left or right.
* To sort the table, click on any of the column headers. A new click will reverse the sort order.
* To hide/show any of the columns, check/uncheck the options below.
* To see another directory, double click on a directory below.
EOT
    
my @displayed;

my $mw = new MainWindow;

# Show some "help" text.
$mw->Label(-text=>$intro,-justify=>'left')->pack(-anchor=>'w');

# Put the exit button and all column checkbuttons in a frame.
my $f = $mw->Frame(-bd=>2,-relief=>'groove')
    ->pack(-anchor=>'w', -expand=>0,-fill=>'x');
$f->Button(-text=>'Exit',-command=>sub{exit})
    ->pack(-side=>'right',-anchor=>'e');
$i=0;
foreach (qw/Mode Nlink Uid Gid Size Mtime Name/) {
    $displayed[$i]=1;
    $f->Checkbutton(-text=>"$_   ",
		    -variable=>\$displayed[$i],
		    -command=>[\&hideOrShow, $i])->pack(-side=>'left');
    $i++;
}

# Create the MListbox widget.
# Specify alternative comparison routine for integers and date.
#
my $ml = $mw->Scrolled('MListbox',
		       -scrollbars => 'oe',
		       -bd=>2,-relief=>'sunken',
		       -columns=>[[-text=>'Mode',-textwidth=>10],
				  [-text=>'NLink', -textwidth=>3,
				   -comparecmd => sub {$_[0] <=> $_[1]}],
				  [-text=>'Uid'],
				  [-text=>'Gid'],
				  [-text=>'Size',
				   -comparecmd => sub {$_[0] <=> $_[1]}],
				  [-text=>'Mtime',
				   -comparecmd => \&compareDate],
				  [-text=>'Name']])
    ->pack (-expand=>1, -fill=>'both', -anchor=>'w');

# bind does not work as it should do, the callback will get a
# column widget as it first argument, not the MListbox widget.....
#
$ml->bind("<Double-Button-1>", \&doubleClick);

# Start by showing the current directory.
directory (".");

MainLoop;

#----------------------------------------------------------
#
sub directory
{
    my ($dir) = @_;

    chdir($dir);
    
    my $pwd = `pwd`; chomp $pwd;
    $mw->title ("Directory: $pwd");
    
    # Empty $ml
    $ml->delete(0,'end');
    
    opendir (DIR, ".") or die "Cannot open '.': $!\n";
    
    foreach my $name (readdir(DIR)) {	
	my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size,
	    $atime, $mtime, $ctime, $blksize, $blocks) = stat($name);
	
	my $type = do {
	    if (-l $name) {
		$mode = 0777;
		'l';
	    } elsif (-f $name) {
		'-';
	    } elsif (-d $name) {
		'd';
	    } elsif (-p $name) {
		'p';
	    } elsif (-b $name) {
		'b';
	    } elsif (-c $name) {
		'c';
	    } else {
		' ';
	    }};
	    
	$gid = getgrgid ($gid);
	$uid = getpwuid ($uid);

	$mtime = localtime($mtime);
	$mode = $type . convMode ($mode);

	$ml->insert('end', [$mode,$nlink,$uid,$gid,$size,$mtime,$name]);
    }
}

# Hide or show a column, depends on the value of $displayed[columnindex].
sub hideOrShow
{
    my ($index) = @_;
    if ($displayed[$index]) {
	$ml->columnShow($index);
    } else {
	$ml->columnHide($index);
    }	    
}

sub doubleClick
{
    print "doubleClick: @_\n";
    my @sel = $ml->curselection;
    if (@sel == 1) {
	my ($mode, $name) = ($ml->getRow($sel[0]))[0,6];
	if ($mode =~ m/^d/) {   # Directory?
	    directory ($name);
	}
    }
}

# Converts a numeric file mode to the format provided by the ls command.
#
sub convMode 
{
    my $mode = shift;
    my $result = '';

    $result .= ($mode & 0400) ? 'r' : '-';
    $result .= ($mode & 0200) ? 'w' : '-';
    if ($mode & 0100) {
	if ($mode & 04000) {
	    $result .= 's';
	} else {
	    $result .= 'x';
	}
    } else {
	$result .= '-';
    }

    $result .= ($mode & 040) ? 'r' : '-';
    $result .= ($mode & 020) ? 'w' : '-';
    if ($mode & 010) {
	if ($mode & 02000) {
	    if (($mode & 02010) || 
		($mode & 02030) ||
		($mode & 02050) ||
		($mode & 02070))
	    {
		$result .= 's';
	    } else {
		$result .= 'l';
	    }
	} else {
	    $result .= 'x';
	}
    } else {
	$result .= '-';
    }

    $result .= ($mode & 04) ? 'r' : '-';
    $result .= ($mode & 02) ? 'w' : '-';
    $result .= ($mode & 01) ? 'x' : '-';

    return $result;
}

# Callback for date comparison. Expects that the dates are on the format
# "day mon dd hh:mm:ss yyyy", for example "Tue Dec  7 12:13:11 1999".
#
sub compareDate
{
    my ($d1, $d2) = @_;
    convertDate($d1) cmp convertDate($d2);
}
sub convertDate
{
    my ($str) = @_;
    my ($wday,$mon,$day,$hour,$min,$sec,$year) = 
	($str =~ m/(\S*)\s*(\S*)\s*(\d*)\s*(\d\d):(\d\d):(\d\d)\s*(\d\d\d\d)/);

    my $month=0;
    foreach (qw/Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec/) {
	if ($mon eq $_) {
	    last;
	} else {
	    $month++;
	}
    }
    return sprintf ("%04d%02d%02d%02d%02d%02d", 
		    $year,$month,$day,$hour, $min, $sec);
}




