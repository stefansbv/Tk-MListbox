#
# MListbox
# 
# Author: Hans J. Helgesen, December 1999.
#
# Before March 2000:
#
# Please send comments, suggestions and error reports to 
# hans_helgesen@hotmail.com.
#
# From March 2000: hans.helgesen@novit.no
#
#

# This module contains four classes: CListbox, MLColumn, HButton and MListbox. 
# Only MListbox is intended for external use.
#
# For documentation on MListbox methods and configuration, see comments
# in the MListbox package (search for Tk::MListbox in this file).
#

##############################################################################
# CListbox is similar to an ordinary listbox, but with the following 
# differences:
# - Calls an -updatecommand whenever something happens to it.
# - Horizontal scanning is disabled, calls -xscancommand to let parent widget
#   handle this.
{
    package Tk::CListbox;
    require Tk::Listbox;
    
    use vars qw(@ISA);
    
    @ISA = qw/Tk::Derived Tk::Listbox/;
    
  Tk::Widget->Construct('CListbox');
    
    sub Populate 
    { 
	shift->ConfigSpecs(-updatecommand => ['CALLBACK'],
			   -xscancommand => ['CALLBACK']);
    }

sub selectionSet {
    my ($w)=@_;
    $w->Callback(-updatecommand=>$w->can('SUPER::selectionSet'),@_);
}
sub selectionClear {
    my ($w)=@_;
    $w->Callback(-updatecommand=>$w->can('SUPER::selectionClear'),@_);
}
sub selectionAnchor {
    my ($w)=@_;
    $w->Callback(-updatecommand=>$w->can('SUPER::selectionAnchor'),@_);
}
sub activate {
    my ($w)=@_;
    $w->Callback(-updatecommand=>$w->can('SUPER::activate'),@_);
}
sub see {
    my ($w)=@_;
    $w->Callback(-updatecommand=>$w->can('SUPER::see'),@_);
}
sub yview {
    my ($w)=@_;
    $w->Callback(-updatecommand=>$w->can('SUPER::yview'),@_);     
}
sub scan {
    my ($w,$type,$x,$y) = @_;
    # Disable horizontal scanning.
    if ($type eq 'mark') {
	$w->{'ml_scanmark_x'} = $x;
    }
    $w->Callback(-updatecommand=>$w->can('SUPER::scan'),
		 $w,$type,$w->{'ml_scanmark_x'},$y);

    $w->Callback(-xscancommand=>$type,$x);
}
}

##############################################################################
# HButton is like an ordinary Button, but with an addition option:
#      -pixelwidth
#
# The new configure method makes sure the pixelwidth is always retained.
#
{
    package Tk::HButton;
    require Tk::Button;
    use vars qw(@ISA);
	
    @ISA = qw/Tk::Derived Tk::Button/;
    
  Tk::Widget->Construct('HButton');
    
    sub Populate
    {
	shift->ConfigSpecs(-pixelwidth => ['PASSIVE']);
    }

sub configure
{
    my ($w) = shift;
    my (@ret) = $w->SUPER::configure(@_);
    unless (@ret) {
	if (defined(my $pixels = $w->cget('-pixelwidth'))) {
	    $w->GeometryRequest($pixels,$w->height);
	}
    }
    return @ret;
}
}

###############################################################################
# MLColumn implements a single column in the MListbox. A column consists of a 
# heading (a Button), the listbox (CListbox), and the vertical borderline 
# (a very slim Canvas).
#
# The MLColumn implements the resizing of the column.
#
{
    package Tk::MLColumn;
    
    use base qw(Tk::Frame);
    Construct Tk::Widget 'MLColumn';
    
    sub Populate {
	my ($w, $args) = @_;
	$w->SUPER::Populate($args);
	
	# The column is a frame containing a heading (HButton), and a listbox
	# below. The right edge of the frame is a Canvas containing
	#a black line.
	
	my $c = $w->Canvas(-height => 1,
			   -takefocus => 0)
	    ->pack(-side => 'right',-fill=>'y',-anchor => 'w');
	
	
	my $f = $w->Frame
	    ->pack(-side=>'left', -anchor=>'e',-fill=>'y',-expand=>1);
	
	my $b = $f->HButton(-takefocus=>0,
			    -padx=>0,
			    -width=>1,
			    -borderwidth=>1)
	    ->pack(-side=>'top', -anchor=>'n', -fill=>'x');
	
	my $l = $f->CListbox(-selectborderwidth=>0,
			     -highlightthickness=>0,
			     -relief=>'flat',
			     -bd=>0,
			     -exportselection=>0,
			     -takefocus=>0)
	    ->pack(-side=>'top', -anchor=>'n', -fill=>'both', -expand=>1);
	
	
	$c->Tk::bind("<B1-Motion>", [$w=>'adjustMotion']);
	$c->Tk::bind("<ButtonRelease-1>", 
		     [$w=>'Callback','-configurecommand']);
	$w->Advertise("listbox" => $l);
	$w->Advertise("separator" => $c);
	$w->Advertise("heading" => $b);
	$w->Advertise("frame" => $f);
	
	$w->Delegates (DEFAULT => $l);
	
	$w->ConfigSpecs
	    (-comparecommand => ['CALLBACK', undef,undef,sub{$_[0] cmp $_[1]}],
	     -comparecmd => '-comparecommand',
	     -configurecommand => ['CALLBACK'],
	     -updatecommand  => [$l],
	     -background => ['METHOD'],
	     -font       => ['DESCENDANTS'],
	     -text       => [$b],
	     -separatorwidth => [{-width => $c}, undef,undef,1],
	     -separatorcolor => [{-background => $c}, undef,undef,'black'],
	     -resizeable => ['METHOD',undef,undef,1],
	     -sortable   => ['PASSIVE'],
	     -textwidth  => '-width',
	     -width      => [$l, $h],
	     DEFAULT     => [$l] );
    }

#----------------------------------------------------------------------------
# MLColumn configuration methods.
# 
sub background
{
    my ($w,$value) = @_;
    return $w->Subwidget('listbox')->cget(-background) unless defined $value;
    foreach (qw/listbox heading/) {
	$w->Subwidget($_)->configure(-background=>$value);
    }
}
sub resizeable 
{
    my ($w,$value) = @_;
    return $w->{'lb_resizeable'} unless defined $value;

    $w->Subwidget("separator")->configure 
	(-cursor => ($value ? 'sb_h_double_arrow' : 'xterm'));
    
    $w->{'lb_resizeable'} = $value;
}

#----------------------------------------------------------------------------
# MLColumn exported methods.
#
sub compare
{
    my ($w,$a,$b) = @_;
    $w->Callback(-comparecommand => $a, $b);
}

sub setWidth
{
    my ($w, $pixels) = @_;
    $pixels -= $w->Subwidget("separator")->width;
    
    return unless $pixels >= 0;
    
    $w->Subwidget("listbox")
	->GeometryRequest($pixels,$w->Subwidget("listbox")->height);
    $w->Subwidget("heading")->configure(-pixelwidth=>$pixels);
}


#----------------------------------------------------------------------------
# MLColumn internal methods.
# 

# Adjust size of column.
sub adjustMotion
{
    my ($w) = @_;    
    $w->setWidth($w->pointerx - $w->rootx);
}

}
############################################################################

package Tk::MListbox;

use strict;
use vars qw($VERSION);

$VERSION = '1.09';

use Tk;
use Tk::Pane;
use strict;
use Carp;

require Tk::Frame;

use base qw(Tk::Frame);
Construct Tk::Widget 'MListbox';

sub ClassInit
{
    my ($class,$mw) = @_;
    $mw->bind($class,'<Configure>',['yscrollCallback']);
}

sub Populate {
    my ($w, $args) = @_;
    
    $w->SUPER::Populate($args);   
    $w->{'ml_columns'} = []; 
    $w->{'ml_sortcol'} = -1;
    $w->{'ml_sort_descending'} = 0;
    $w->{'ml_top'} = 0;
    $w->{'ml_bottom'} = 0;

    my $pane = $w->Pane(-sticky=>'nsew')->pack(-expand=>1,-fill=>'both');
    $w->Advertise("pane" => $pane);
    $w->ConfigSpecs(-columns => ['METHOD'],
		    -moveable=> ['PASSIVE', undef, undef, 1],
		    -sortable => ['PASSIVE', undef, undef, 1],
		    -borderwidth => ['SELF', undef, undef, 0],
		    -bd => '-borderwidth',
		    -relief => ['SELF', undef, undef, 'flat'],
		    -xscrollcommand => [$pane],
		    -yscrollcommand => ['CALLBACK'],
		    -configurecommand => ['CALLBACK'],
		    -selectmode => ['METHOD',undef,undef,'browse'],
		    -height => ['METHOD',undef,undef,'10'],
		    DEFAULT => ['CHILDREN']);
}

sub xview { shift->Subwidget("pane")->xview(@_) }

#---------------------------------------------------------------------
# Configuration methods (call via configure). 

# Define the columns in the MListbox. 
sub columns
{
    my ($w, $cols) = @_;

    return $w->{'ml_columns'} unless defined $cols;

    $w->columnDelete(0,'end');
    map {$w->columnInsert('end',@$_)} @$cols;
}
    
# This method handles configoptions that should be propagated to 
# all MLColumn subwidgets. Can't use the "DEFAULT=>['CHILDRE'], since
# CHILDREN also includes the Pane subwidget.

sub childrenConfigure
{
    my ($w,$option,$value) = @_;
    return $w->{"ml_$option"} unless defined $value;
    foreach (@{$w->{'ml_columns'}}) {
	$_->configure("-$option"=>$value);
    }
    $w->{"ml_$option"} = $value;
}
sub selectmode { shift->childrenConfigure('selectmode',@_)}
sub height { shift->childrenConfigure('height',@_)}


#---------------------------------------------------------------------
# Exported methods.
#
# I: Methods for column configuration/access.
#

# Insert a column. $index should be a number or 'end'. 
sub columnInsert    
{
    my ($w, $index, %args) = @_;
    
    $index = $w->columnIndex($index,1);
    my %opts = ();
    
    # Copy these options from the megawidget.
    foreach (qw/-selectmode -resizeable -background 
	        -sortable
	        -separatorcolor -separatorwidth/) 
    {
	$opts{$_} = $w->cget($_) if defined $w->cget($_);
    }
    # All options (and more) might be overridden by %args.
    map {$opts{$_} = $args{$_}} keys %args;
    
    my $c = $w->Subwidget("pane")->MLColumn 
	(%opts,			  
	 -yscrollcommand => [ $w => 'yscrollCallback'],
	 -configurecommand => [$w => 'Callback','-configurecommand',$w],
	 -xscancommand => [ $w => 'xscan' ],
	 -updatecommand => [ $w => 'selectionUpdate']);
    
    # Fill the new column with empty values, making sure all columns have
    # the same number of rows.
    unless (scalar(@{$w->{'ml_columns'}}) == 0) {
	foreach (1..$w->size) {
	    $c->insert('end','');
	}
    }
    
    $c->Subwidget("heading")->bind("<Button-1>", [ $w => 'dragOrSort', $c]);
    
    my $carr = $w->{'ml_columns'};
    splice(@$carr,$index,0,$c);

    # Update the selection to also include the new column.
    map {$w->selectionSet($_,$_)} $w->curselection;

    # Copy all bindings that are created by calls to 
    # bindRows, bindColumns and/or bindSeparators.
    #
    foreach my $subwidget (qw/listbox heading separator/) {
	foreach (keys %{$w->{'ml_bindings'}->{$subwidget}}) {
	    $c->Subwidget($subwidget)->Tk::bind
		($_, [$w => 'bindCallback',
		      $w->{'ml_bindings'}->{$subwidget}->{$_},
		      $index]);
	}
    }
    
    if (Tk::Exists($w->{'ml_columns'}->[$index+1])) {
	$w->columnShow($index, -before=>$index+1);
    } else {
	$w->columnShow($index);
    }

    return $c;
}

# Implements horizontal scanning. 
sub xscan
{
    my ($w,$type,$x) = @_;

    if ($type eq 'dragto') {
	my $dist = $w->{'ml_scanmark_x'} - $w->pointerx;
	
	# Looks like there is a bug in Pane: If no -xscrollcommand
	# is defined, xview() fails. This is fixed by this hack:
	#
	my $p = $w->Subwidget("pane");
	unless (defined ($p->cget(-xscrollcommand))) {
	    $p->configure(-xscrollcommand => sub {});
	}
	$p->xview('scroll',$dist,'units');
    }
    $w->{'ml_scanmark_x'} = $w->pointerx;
}

# Converts a column index to a numeric index. $index might be a number,
# 'end' or a reference to a column widget (see columnGet).
#
sub columnIndex
{
    my ($w, $index, $after_end) = @_;

    if ($index =~ m/^\s*(\d+)\s*$/) {
	return $1;
    }    
    if ($index eq 'end') {
	if (defined $after_end) {
	    return $#{$w->{'ml_columns'}} + 1;
	} else {
	    return $#{$w->{'ml_columns'}};
	}
    }    
    if (ref $index) {
	foreach (0..$#{$w->{'ml_columns'}}) {
	    if ($index eq $w->{'ml_columns'}->[$_]) {
		return $_;
	    }
	}
    } 
    croak "Invalid column index: $index\n";
}

# Delete a column.
#
sub columnDelete
{
    my ($w, $first, $last) = @_;

    for (my $i=$w->columnIndex($first); $i<=$w->columnIndex($last); $i++) {
	$w->columnGet($i)->destroy;
    }
    @{$w->{'ml_columns'}} = map{Exists($_) ? $_ : ()} @{$w->{'ml_columns'}};
}


sub columnHide
{
    my ($w, $first, $last) = @_;
    $last = $first unless defined $last;

    for (my $i=$w->columnIndex($first); $i<=$w->columnIndex($last); $i++) {
	$w->columnGet($i)->packForget;
    }
}

sub columnShow
{
    my ($w, $index, %args) = @_;
    
    my $c = $w->columnGet($index);
    
    my @packopts = (-anchor=>'w',-side=>'left',-fill=>'both');
    if (defined($args{'-before'})) {
	push (@packopts, '-before'=>$w->columnGet($args{'-before'}));
    } elsif (defined($args{'-after'})) {
	push (@packopts, '-after'=>$w->columnGet($args{'-after'}));
    }
    
    $c->pack(@packopts);
}

sub columnGet
{
    my ($w, $from, $to) = @_;
    if (defined($to)) {
	$from= $w->columnIndex($from);
	$to = $w->columnIndex($to);
	return @{$w->{'ml_columns'}}[$from..$to];
    } else {
	return $w->{'ml_columns'}->[$w->columnIndex($from)];
    }
}


sub columnConfigure
{
    my ($w, $index, %args) = @_;
    $w->columnGet($index)->configure(%args);
}

sub columnPackInfo
{
    my ($w) = @_;

    map {$w->columnIndex($_) . ':' . $_->width} 
       sort {$a->rootx <=> $b->rootx}
          map {$_->ismapped ? $_ : ()} @{$w->{'ml_columns'}};
}    
    
sub columnPack
{
    my ($w, @packinfo) = @_;
    $w->columnHide(0,'end');
    foreach (@packinfo) {
	my ($index, $width) = split /:/;
	$w->columnShow ($index);
	if (defined($width)) {
	    $w->columnGet($index)->setWidth($width);
	}
    }
}

#---------------------------------------------------------------------
# Exported methods.
#
# II: Methods for row access.
#
sub delete
{
    my $w = shift;
    foreach (@{$w->{'ml_columns'}}) {
	my $saved_width = $_->width;
        $_->delete(@_);
	if ($_->ismapped) {
	    $_->setWidth($saved_width);
	}
    }
    $w->yscrollCallback;
}
    
sub insert
{
    my ($w, $index, @data) = @_;
    my ($rownum, $colnum);
    
    my $rowcnt = $#data;
    
    # Insert data into one column at a time, calling $listbox->insert
    # ONCE for each column. (The first version of this widget call insert
    # once for each row in each column).
    # 
    foreach $colnum (0..$#{$w->{'ml_columns'}}) {	
	my $c = $w->{'ml_columns'}->[$colnum];
	
	# The listbox might get resized after insert/delete, which is a 
	# behaviour we don't like....
	my $saved_width = $c->width;

	my @coldata = ();

	foreach (0..$#data) {
	    if (defined($data[$_][$colnum])) {
		push @coldata, $data[$_][$colnum];
	    } else {
		push @coldata, '';
	    }
	}
	$c->insert($index,@coldata);
	
	if ($c->ismapped) {
	    # Restore saved width.
	    $c->setWidth($saved_width);
	} 
    }    
    $w->yscrollCallback;
}

sub getRow
{
    my @result = map {$_->get(@_)} @{shift->{'ml_columns'}};
    if (wantarray) {
	@result;
    } else {
	$result[0];
    }
}
    
sub get
{
    my @result = ();
    my ($colnum,$rownum) = (0,0);
    
    foreach (@{shift->{'ml_columns'}}) {
	my @coldata = $_->get(@_);
	$rownum = 0;
	map {$result[$rownum++][$colnum] = $_} @coldata;
	$colnum++;
    }
    @result;
}

sub sort 
{
    my ($w, $descending, @indexes) = @_;
    
    # Hack to avoid problem with older Tk versions which do not support
    # the -recurse=>1 option.
    $w->Busy;   # This works always (but not very good...)
  Tk::catch {$w->Busy(-recurse=>1)};# This works on newer Tk versions,
                                    # harmless on old versions.
      
      
    @indexes = (0..$#{$w->{'ml_columns'}}) unless defined @indexes;

    # Convert all indexes to integers.
    map {$_=$w->columnIndex($_)} @indexes;
    
    # This works on Solaris, but not on Linux???
    # Store the -comparecommand for each row in a local array. In the sort,
    # the store command is called directly in stead of via the MLColumn
    # subwidget. This saves a lot of callbacks and function calls.
    #
   # my @cmp_subs = map {$_->cget(-comparecommand)} @{$w->{'ml_columns'}};
    
    # If sort order is not defined
    unless (defined $descending) {
	if ($#indexes == 0 &&
	    $w->{'ml_sortcol'} == $indexes[0] &&
	    $w->{'ml_sort_descending'} == 0)
	{
	    # Already sorted on this column, reverse sort order.
	    $descending = 1;
	} else {
	    $descending = 0;
	}
    }

    # To retain the selection after the sort we have to save information
    # about the current selection before the sort. Adds a dummy column
    # to the two dimensional data array, this last column will be true
    # for all rows that are currently selected.
    my $dummy_column = scalar(@{$w->{'ml_columns'}});

    my @data = $w->get(0,'end');
    foreach ($w->curselection) {
	$data[$_]->[$dummy_column] = 1;  # Selected...
    }
    
    @data = sort {
	local $^W = 0;
	foreach (@indexes) {
	    my $res = do {
		if ($descending) {
		    # Call via cmp_subs works fine on Solaris, but no
		    # on Linux. The column->compare method is much slower...
		    #
		    # &{$cmp_subs[$_]} ($b->[$_],$a->[$_]);
		    $w->{'ml_columns'}->[$_]->compare($b->[$_],$a->[$_]);
		} else {
		    # &{$cmp_subs[$_]} ($a->[$_],$b->[$_]);
		    $w->{'ml_columns'}->[$_]->compare($a->[$_],$b->[$_]);
		}
	    };
	    return $res if $res;
	}
	return 0;
    } @data;
    

    
    # Replace data with the new, sorted list.
    $w->delete(0,'end');
    $w->insert(0,@data);

    my @new_selection = ();
    foreach (0..$#data) {
	if ($data[$_]->[$dummy_column]) {
	    $w->selectionSet($_,$_);
	}
    }

    $w->{'ml_sortcol'} = $indexes[0];
    $w->{'ml_sort_descending'} = $descending;
    
    $w->Unbusy; #(-recurse=>1);
}


#-----------------------------------------------------------------------
# Internal methods.

sub yscrollCallback 
{
    my ($w,$top,$bottom) = @_;

    unless ($w->cget(-yscrollcommand)) {
	return;
    }

    unless (defined($top)) {
	# Called internally
	my $c = $w->firstVisible;
	if (Exists($c) && $c->ismapped){
	    ($top,$bottom) = $c->yview;
	} else {
	    ($top,$bottom) = (0,1);
	}
    } 
    
    if ($top != $w->{'ml_top'} || $bottom != $w->{'ml_bottom'}) {
	$w->Callback(-yscrollcommand=>$top,$bottom);
	$w->{'ml_top'} = $top;
	$w->{'ml_bottom'} = $bottom;
    }
}

sub selectionUpdate
{
    my ($w, $code,$l,@args) = @_;

    if (@args) {
	foreach (@{$w->{'ml_columns'}}) {
	    &$code($_->Subwidget("listbox"), @args);
	}
    } else {
	&$code($w->{'ml_columns'}->[0]->Subwidget("listbox"));
    }
}


# This method implements sorting and dragging & drop of a column
#
sub dragOrSort
{
    my ($w, $c) = @_;
 
    unless ($w->cget(-moveable)) {
	if ($c->cget(-sortable)) {
	    $w->sort (undef, $c);
	}
	return;
    }
    
    my $h=$c->Subwidget("heading");  # The heading button of the column.
    
    my $start_mouse_x = $h->pointerx;
    my $y_pos = $h->rooty;  # This is constant through the whole operation.
    my $width = $h->width;
    my $left_limit = $w->rootx - 1;
    
    # Find the rightmost, visible column
    my $right_end = 0;
    foreach (@{$w->{'ml_columns'}}) {
	if ($_->rootx + $_->width > $right_end) {
	    $right_end = $_->rootx + $_->width;
	}
    }	    
    my $right_limit = $right_end + 1;
   
    # Create a "copy" of the heading button, put it in a toplevel that matches
    # the size of the button, put the toplevel on top of the button.
    my $tl=$w->Toplevel; 
    $tl->overrideredirect(1);
    $tl->geometry(sprintf("%dx%d+%d+%d",
			  $h->width, $h->height, $h->rootx, $y_pos));

    my $b=$tl->HButton
	(map{defined($_->[4]) ? ($_->[0]=>$_->[4]) : ()} $h->configure)
	    ->pack(-expand=>1,-fill=>'both');
    
    # Move the toplevel with the mouse (as long as Button-1 is down).
    $h->bind("<Motion>", sub {
	my $new_x = $h->rootx - ($start_mouse_x - $h->pointerx);
	unless ($new_x + $width/2 < $left_limit ||
		$new_x + $width/2 > $right_limit) 
	{
	    $tl->geometry(sprintf("+%d+%d",$new_x,$y_pos));
	}
    });

    $h->bind("<ButtonRelease-1>", sub {
	my $rootx = $tl->rootx;
	my $x = $rootx + ($tl->width/2);
	$tl->destroy;    # Don't need this anymore...
	$h->bind("<Motion>",'');  # Cancel binding

	if ($h->rootx == $rootx) {	
	    # Button NOT moved, sort the column....
	    if ($c->cget(-sortable)) {
		$w->sort(undef, $c);
	    }
	    return;
	}
		
	# Button moved.....
	# Decide where to put the column. If the center of the dragged 
	# button is on the left half of another heading, insert it -before 
	# the column, otherwise insert it -after the column.
	foreach (@{$w->{'ml_columns'}}) {
	    if ($_->ismapped) {
		my $left = $_->rootx;
		my $right = $left + $_->width;
		if ($left <= $x && $x <= $right) {
		    if ($x - $left < $right - $x) {
			$w->columnShow($c,-before=>$_);
		    } else {
			$w->columnShow($c,'-after'=>$_);
		    }
		    $w->update;
		    $w->Callback(-configurecommand => $w);
		}
	    }
	}
    });
    
}

# The following methods take care of external bindings. It makes little sense
# to call $mlistbox->bind(...) without specifying a subwidget (header, listbox,
# separator). In stead of the ordinary bind, the following methods should
# be used:
#
#     bindRows($sequence,$callback) 
#     bindColumns($sequence,$callback)
#     bindSeparators($sequence,$callback)
#
# bindRows() adds the specified binding to all listboxes in the widget. 
# bindColumns() adds the binding to all column headers in the widget.
# bindSeparators() adds the binding to all separators in the widget.
#
# These methods store the binding information, and if you create a new
# column by calling $mlistbox->columnInsert, all bindings created by
# these methods are automatically copied to the new column.
#
# The callback is called with the MListbox widget as first argument, and
# the index of the column where the event occured as the second argument.
#
# NOTE that these methods does not support all of Tk's callback formats.
# The following are supported:
#
#     \&subname
#     sub { code }
#     [ \&subname, arguments...]
#     [ sub { code }, arguments...]
#
#
sub bindSubwidgets
{
    my ($w,$subwidget,$sequence,$callback) = @_;
    my $column_index = 0;
    
    unless (defined $sequence) {
	return (keys %{$w->{'ml_bindings'}->{$subwidget}});
    }
    unless (defined $callback) {
	$callback = $w->{'ml_bindings'}->{$subwidget}->{$sequence};
	$callback = '' unless defined $callback;
	return $callback;
    }
    if ($callback eq '') {
	foreach (@{$w->{'ml_columns'}}) {
	    $_->Subwidget($subwidget)->Tk::bind($sequence,'');
	}
	delete $w->{'ml_bindings'}->{$subwidget}->{$sequence};
	return '';
    }
    foreach (@{$w->{'ml_columns'}}) {
	$_->Subwidget($subwidget)
	    ->Tk::bind($sequence,
		       [$w => 'bindCallback',$callback, $column_index++]);
    }
    $w->{'ml_bindings'}->{$subwidget}->{$sequence} = $callback;
    return '';
}

sub bindCallback
{
    my ($w,$cb, $ci) = @_;
    if (ref $cb eq 'ARRAY') {
	my ($code,@args) = @$cb;
	return $w->$code($ci,@args);
    } else {
	return $w->$cb($ci);
    }
}

sub bind { shift->bindRows(@_) }
sub bindRows {  shift->bindSubwidgets('listbox',@_) }
sub bindColumns {  shift->bindSubwidgets('heading',@_) }
sub bindSeparators {  shift->bindSubwidgets('separator',@_) }


# Many of the methods in this package are very similar: They call
# the same method for the first (visible) column widget.
sub firstVisible
{
    my $w=shift;
    foreach(@{$w->{'ml_columns'}}) {
	return $_ if $_->ismapped;
    }
    return $w->{'ml_columns'}->[0];
}

sub curselection { shift->firstVisible->curselection(@_)}
sub activate { shift->firstVisible->activate(@_)}
sub index { shift->firstVisible->index(@_)}
sub nearest { shift->firstVisible->nearest(@_)}
sub see { shift->firstVisible->see(@_)}
sub selectionAnchor { shift->firstVisible->selectionAnchor(@_)}
sub selectionSet { shift->firstVisible->selectionSet(@_)}
sub selectionClear { shift->firstVisible->selectionClear(@_)}
sub selectionIncludes { shift->firstVisible->selectionIncludes(@_)}
sub size { shift->firstVisible->size(@_)}
sub yview { shift->firstVisible->yview(@_)}


1;

__END__


=head1 NAME 

Tk::MListbox - Multicolumn Listbox.

=head1 SYNOPSIS

  use Tk::MListbox;
  $ml = $parent->MListbox (<options>);

=head1 DESCRIPTION

Tk::MListbox is a multicolumn Listbox widget with builtin capabilites for
sorting, resizing and repositioning of the columns.

Sorting is done by clicking on one of the column headings in the 
widget. The first click will sort the data with the selected column
as key, a new click will reverse the sort order.

The columns may be resized by dragging a separator line which
is drawn between each column.

A column's position in the widget might be changed by dragging 
it's heading left or right.

Tk::MListbox is used in a way similar to the standard Listbox, but in 
stead of scalar values MListbox operates on lists of data. In addition
to methods for accessing the data in the MListbox, the widget offer 
methods for manipulation of the individual columns.


=head1 AUTHOR

Hans Jorgen Helgesen, hans_helgesen@hotmail.com (from March 2000: hans.helgesen@novit.no)

=head1 SEE ALSO

L<Tk::Listbox>

=head1 STANDARD OPTIONS

B<-background> B<-foreground> B<-relief> B<-takefocus>
B<-borderwidth>	B<-heigh> B<-selectbackground>	B<-cursor>
B<-highlightbackground> B<-selectborderwidth> B<-xscrollcommand>
B<-exportselection> B<-highlightcolor> B<-selectforeground>
B<-yscrollcommand> B<-font> B<-highlightthickness> B<-setgrid>

See L<Tk::options> for details of the standard options.

=head1 REQUIREMENTS

Tk::MListbox requires Tk::Pane.
(and basic Perl/Tk of course....)

=head1 WIDGET SPECIFIC OPTIONS

=over 4

=item -columns => I<list>

Defines the columns in the widget. Each element in the list 
describes a column. See the B<COLUMNS> section below.

=item -configurecommand => I<callback>

The -configurecommand callback will be called whenever the layout of the
widget has changed due to user interaction. That is, the user changes the
width of a column by dragging the separator, or moves a column by dragging
the column header. 

This option is useful if the application wants to store the widget layout 
for later retrieval. The widget layout can be obtained by the callback
by calling the method columnPackInfo().

=item -moveable => I<boolean>

A value of B<1> indicates that it is okay for the user to move
the columns by dragging the column headers. B<0> disables this
function.

Default: B<1>

=item -resizeable => I<boolean>

A value of B<1> indicates that it is okay for the user to resize
the columns by dragging the column separators. B<0> disables 
this function.

Default: B<1>

Note that you can also specify -resizeable on a column
by column basis. See the B<COLUMNS> section below.

=item -selectmode => I<string>

Should be "single", "browse", "multiple", or "extended".

Default is "browse". See L<Tk::Listbox>.

=item -separatorcolor => I<string>

Specifies the color of the separator lines 
(the vertical lines that separates the columns). 

Default: B<black>

Note that you can also specify -separatorcolor on a column
by column basis. See the B<COLUMNS> section below.

=item -separatorwidth => I<integer>

Specifies the width in pixels of the separator lines 
(the vertical lines that separates the columns). 

Default: B<1>

Note that you can also specify -separatorwidth on a column
by column basis. See the B<COLUMNS> section below.

=item -sortable => I<boolean>

A value of B<1> indicates that it is okay for the user to sort
the data by clicking column headings. B<0> disables this function.

Default: B<1>

Note that you can also specify -sortable on a column
by column basis. See I<COLUMNS> below.

=head1 COLUMNS

The MListbox widget is a collection of I<MLColumn> widgets. 
Each MLColumn contains a Listbox, a heading and the separator bar.
The columns are created and maintained through the -columns 
option or the column methods of the widget. The columns are indexed
from 0 and up. Initially, column 0 is the leftmost column of the
widget. The column indices B<are not changed> when the columns
are moved or hidden. The only ways to change the column indices 
are to call columnInsert(), columnDelete() or configure(-column).

Each column has its own set of options which might be passed to 
MListbox::configure(-columns), MListbox::insert(),
MListbox::columnConfigure() or MLColumn::configure().

The following code snippets are all equal:

1.  $ml=$mw->MListbox(-columns=>[[-text=>'Heading1',
                                  -sortable=>0],
                                 [-text=>'Heading2']]);

2.  $ml=$mw->MListbox;
    $ml->columnInsert(0,-text=>'Heading1', -sortable=>0);
    $ml->columnInsert(0,-text=>'Heading2');

3.  $ml=$mw->MListbox;
    $c=$ml->columnInsert(0,-text=>'Heading1');
    $ml->columnInsert(0,-text=>'Heading2');
    $c->configure(-sortable=>0);

4.  $ml=$mw->MListbox;
    $ml->columnInsert(0,-text=>'Heading1');
    $ml->columnInsert(0,-text=>'Heading2');
    $ml->columnConfigure(0,-sortable=>0);

(See the columnConfigure() method below for details on column options).

All column methods expects one or two column indices as arguments.
The column indices might be an integer (between 0 and the number
of columns minus one), 'end' for the last column, or a reference
to the MLColumn widget (obtained by calling MListbox->columnGet() 
or by storing the return value from MListbox->columnInsert()).

=head1 WIDGET METHODS

=over 4

=item $ml->bindColumns(I<sequence>,I<callback>)

Adds the binding to all column headers in the widget. See the section
BINDING EVENTS TO MLISTBOX below.

=item $ml->bindRows(I<sequence>,I<callback>)

Adds the binding to all listboxes in the widget. See the section
BINDING EVENTS TO MLISTBOX below.

=item $ml->bindSeparators(I<sequence>,I<callback>)

Adds the binding to all separators in the widget. See the section
BINDING EVENTS TO MLISTBOX below.

=back

=head2 COLUMN METHODS

(Methods for accessing and manipulating individual columns
in the MListbox widget)

=item $ml->columnConfigure(I<index>,I<option>=>I<value>...)

Set option values for a specific column.
Equal to $ml->columnGet(I<index>)->configure(...).

The following column options are supported:

=over 4

=item

-comparecommand => I<callback>

Specifies a callback to use when sorting the MListbox with this
column as key. The callback will be called with two scalar arguments,
each a value from this particular column. The callback should 
return an integer less than, equal to, or greater than 0, depending
on how the tow arguments are ordered. If for example the column
should be sorted by numerical value:

    -comparecommand => sub { $_[0] <=> $_[1]}

The default is to sort the columns alphabetically.

=item

-text => I<string>

Specifies the text to be used in the heading button of the column.

=item

-resizeable => I<boolean>

A value of B<1> indicates that it is okay for the user to resize
this column by dragging the separator. B<0> disables this function.

Default: B<1>

=item

-separatorcolor => I<string>

Specifies the color of the separator line, default is B<black>.

=item

-separatorwidth => I<integer>

Specifies the width of the separator line in pixels. Default is B<1>.

=item

-sortable => I<boolean>

A value of B<1> indicates that it is okay for the user to sort
the data by clicking this column's heading. B<0> disables this 
function.

Default: B<1>

=back

=item $ml->columnDelete(I<first>,I<last>)

If I<last> is omitted, deletes column I<first>. If I<last> is
specified, deletes all columns from I<first> to I<last>, inclusive.

All previous column indices greater than I<last> (or I<first> if
I<last> is omitted) are decremented by the number of columns 
deleted.

=item $ml->columnGet(I<first>,I<last>)

If I<last> is not specified, returns the MLColumn widget specified by I<first>.
If both I<first> and I<last> are specified, returns an array containing all
columns from I<first> to I<last>.

=item $ml->columnHide(I<first>,I<last>)

If I<last> is omitted, hides column I<first>. If I<last> is
specified, hides all columns from I<first> to I<last>, inclusive.

Hiding a column is equal to calling $ml->columnGet(I<index>)->packForget. 
The column is B<not> deleted, all data are still available, 
and the column indices remain the same.

See also the columnShow() method below.

=item $ml->columnIndex(I<index>)

Returns an integer index for the column specifed by I<index>.

=item $ml->columnInsert(I<index>,I<option>=>I<value>...)

Creates a new column in the MListbox widget. The column will 
get the index specified by I<index>. If I<index> is 'end', the
new column's index will be one more than the previous highest
column index.

If column I<index> exists, the new column will be placed
to the B<left> of this column. All previous column indices 
equal to or greater than I<index> will be incremented by one.

Returns the newly created MLColumn widget.

(See the columnConfigure() method above for details on column options).

=item $ml->columnPack(I<array>)

Repacks all columns in the MListbox widget according to the 
specification in I<array>. Each element in I<array> is a string
on the format B<index:width>. I<index> is a column index, I<width> 
defines the columns width in pixels (may be omitted). The columns 
are packed left to right in the order specified by by I<array>.
Columns not specified in I<array> will be hidden.

This method is most useful if used together with the 
columnPackInfo() method.

=item $ml->columnPackInfo

Returns an array describing the current layout of the MListbox
widget. Each element of the array is a string on the format
B<index:width> (see columnPack() above). Only indices of columns that 
are currently shown (not hidden) will be returned. The first element
in the returned array represents the leftmost column.

This method may be used in conjunction with columnPack() to save
and restore the column configuration. 

=item $ml->columnShow(I<index>,I<option>=>I<value>)

Shows a hidden column (see the columnHide() method above). 
The column to show is specified by I<index>.

By default, the column is pack'ed at the rigthmost end of the
MListbox widget. This might be overridden by specifying one of
the following options:

=over 4

=item 

-after => I<index>

Place the column B<after> (to the right of) the column specified
by I<index>.

=item 

-before => I<index>

Place the column B<before> (to the left of) the column specified
by I<index>.

=back

=head2 ROW METHODS

(Methods for accessing and manipulating rows of data)

Many of the methods for MListbox take one or more indices as 
arguments. See L<Tk::Listbox> for a description of row indices.

=item $ml->delete(I<first>,I<last>)

Deletes one or more row elements of the MListbox. I<First> and I<last>
are indices specifying the first and last elements in the range to 
delete. If I<last> isn't specified it defaults to I<first>, 
i.e. a single element is deleted. 

=item $ml->get(I<first>,I<last>)

If I<last> is omitted, returns the content of the MListbox row
indicated by I<first>. If I<last> is specified, the command returns
a list whose elements are all of the listbox rows between 
I<first> and I<last>.

The returned elements are all array references. The referenced
arrays contains one element for each column of the MListbox.

=item $ml->getRow(I<index>)

In scalar context, returns the value of column 0 in the MListbox
row specified by I<index>. In list context, returns the content
of all columns in the row as an array.

This method is provided for convenience, since retrieving a single
row with the get() method might produce some ugly code.

The following two code snippets are equal:

   1. @row=$ml->getRow(0);

   2. @row=@{($ml->get(0))[0]};


=item $ml->sort(I<descending>, I<columnindex>...)

Sorts the content of the MListbox. If I<descending> is a B<true> 
value, the sort order will be descending. The default is ascending
sort.

If I<columnindex> is specified, the sort will be done with the 
specified column as key. You can specify as many I<columnindex>
arguments as you wish. Sorting is done on the first column, then
on the second, etc...

The default is to sort the data on all columns of the listbox, 
with column 0 as the first sort key, column 1 as the second, etc.

=head1 OTHER LISTBOX METHODS

Most other Tk::Listbox methods works for the MListbox widget.
This includes the methods activate, cget, curselection, index,
nearest, see, selectionXXX, size, xview, yview.

See L<Tk::Listbox>.

=head1 ADVERTISED SUBWIDGETS

=item pane

All MListbox columns are packed inside a Tk::Pane (this is done
to enable horizontal scrolling).

=back 

Apart from "pane", the MListbox widget has no subwidgets, 
except for the variable number of MLColumns, which obviously 
cannot be advertised. The MLColumn widgets might be obtained 
by calling the columnGet() or columnInsert() methods.

The MLColumn widget (which represents a single column in the
MListbox) advertises the following subwidgets:

=over 4

=item listbox

The individual Listbox. Note that this is B<not> the standard
Tk::Listbox, but a derived version (CListbox). Several of the
widget's methods will not work as expected.

=item separator

The column separator line. This is a Canvas.

=item heading

The column heading. This is a "HButton" (a Button with an additional
-pixelwidth option).

=item frame

A Frame which contains the "listbox" and the "heading"
subwidgets (but not the "separator").

=back

Example: If you want to change the background color of the
heading of column 4:

    $ml->columnGet(4)->Subwidget("heading")
        ->configure(-background=>'blue');

=head1 BINDING EVENTS TO MLISTBOX

Calling $ml->bind(...) probably makes little sense, since the call does not
specify whether the binding should apply to the listbox, the header button 
or the separator line between each column.

In stead of the ordinary bind, the following methods should be used:

=over 4

=item $ml->bind(I<sequence>,I<callback>)

Synonym for $ml->bindRows(I<sequence>,I<callback>).

=item $ml->bindRows(I<sequence>,I<callback>)

Synonym for $ml->bindSubwidgets('listbox',I<sequence>,I<callback>)

=item $ml->bindColumns(I<sequence>,I<callback>)

Synonym for $ml->bindSubwidgets('heading',I<sequence>,I<callback>)

=item $ml->bindSeparators(I<sequence>,I<callback>)

Synonym for $ml->bindSubwidgets('separator',I<sequence>,I<callback>)

=item $ml->bindSubwidgets(I<subwidget>,I<sequence>,I<callback>)

Adds the binding specified by I<sequence> and I<callback> to all subwidgets
of the given type (should be 'listbox', 'heading' or 'separator'). 

The binding is stored in the widget, and if you create a new column 
by calling $ml->columnInsert(), all bindings created by $ml->bindSubwidgets()
are automatically copied to the new column.

The callback is called with the MListbox widget as first argument, and
the index of the column where the event occured as the second argument.

NOTE that $ml->bindSubwidgets() does not support all of Tk's callback formats.
The following are supported:

     \&subname
     sub { code }
     [ \&subname, arguments...]
     [ sub { code }, arguments...]

If I<sequence> is undefined, then the return value is a list whose elements 
are all the sequences for which there exist bindings for I<subwidget>.

If I<sequence> is specified without I<callback>, then the callback currently 
bound to sequence is returned, or an empty string is returned if there is no
binding for sequence.

If I<sequence> is specified, and I<callback> is an empty string, then the
current binding for sequence is destroyed, leaving sequence unbound. 
An empty string is returned.

An empty string is returned in all other cases.

=back

=cut















