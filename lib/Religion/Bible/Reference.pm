package Religion::Bible::Reference;

use warnings;
use strict;

use Exporter;
@Religion::Bible::Reference::ISA = qw(Exporter);

@Religion::Bible::Reference::EXPORT = qw(bibref);

=head1 NAME

Religion::Bible::Reference - canonicalize shorthand bible references

=head1 VERSION

version 0.00_01

 $Id$

=cut

our $VERSION = '0.00_01';

=head1 SYNOPSIS

 use Religion::Bible::Reference;

 my $quote = bibref("jn8:32");

 print "($quote)";   # (John 8:32)
 print $quote->book; # John

=head1 DESCRIPTION

This module converts simple text descriptions of bible references and ranges
into objects that stringify into a canonical form.

=head1 FUNCTIONS

=head2 bibref($ref_string)

This function is exported by default, and constructs a new
Religion::Bible::Reference

=cut

sub bibref { __PACKAGE__->new(@_); }

=head1 METHODS

=head2 Religion::Bible::Reference->new($ref_string)

This method acts just like the exported C<bibref> function.

=cut

sub new {
	my ($class, $ref_string) = @_;
	(my $book = $ref_string) =~ s/\s*(\d+(?::\d+(?:-\d+(?::\d+)?)?)?)\Z//;
	my $range = $1;

	my %bibref;
	return unless $bibref{book}  = $class->canonicalize_book($book);
	return unless $bibref{range} = $class->parse_range($range);
	bless \%bibref => $class;
}

=head2 $self->stringify

This method returns a string representing the reference, using the canonical
book name.

=cut

sub stringify {
	my ($self) = @_;
	$self->{book} . ' ' . $self->_stringify_range;
}

sub _stringify_range {
	my ($self) = @_;
	$self->{range}{begin_chapter}
	. ($self->{range}{begin_verse} ? ":$self->{range}{begin_verse}" : '')
	. ($self->{range}{end_chapter}
		? ($self->{range}{end_chapter} == $self->{range}{begin_chapter})
			? '-'
			: "-$self->{range}{end_chapter}:"
		: '')
	. ($self->{range}{end_verse} ? "$self->{range}{end_verse}" : '')
}

my %book_chapters = (
	'Genesis'   => [],
	'Exodus'    => [],
	'Leviticus' => [],
	'Numbers'   => [],
	'Deuteronomy' => [],
	'Joshua'    => [],
	'Judges'    => [],
	'Ruth'      => [],
	'1 Samuel'  => [],
	'2 Samuel'  => [],
	'1 Kings'   => [],
	'2 Kings'   => [],
	'1 Chronicles' => [],
	'2 Chronicles' => [],
	'Ezra'      => [],
	'Nehemiah'  => [],
	'Esther'    => [],
	'Job'       => [],
	'Psalms'    => [],
	'Proverbs' => [],
	'Ecclesiastes' => [],
	'Song of Solomon' => [],
	'Isaiah' => [],
	'Jeremiah' => [],
	'Lamentations' => [],
	'Ezekiel' => [],
	'Daniel' => [],
	'Hosea' => [],
	'Joel' => [],
	'Amos' => [],
	'Obadiah' => [],
	'Jonah' => [],
	'Micah' => [],
	'Nahum' => [],
	'Habakkuk' => [],
	'Zephaniah' => [],
	'Haggai' => [],
	'Zechariah' => [],
	'Malachi' => [],
	# New Testament
	'Matthew' => [ qw(10 20 30 40 50) ],
	'Mark'    => [ qw(11 21 31 41 51) ],
	'Luke'    => [ qw(12 22 32 42 52) ],
	'John'    => [ qw(13 23 33 43 53) ],
	'Acts'    => [],
	'Romans'  => [],
	'1 Corinthians' => [],
	'2 Corinthians' => [],
	'Galatians' => [],
	'Ephesians' => [],
	'Philippians' => [],
	'Colossians' => [],
	'1 Thessalonians' => [],
	'2 Thessalonians' => [],
	'1 Timothy' => [],
	'2 Timothy' => [],
	'Titus' => [],
	'Philemon' => [],
	'Hebrews' => [],
	'James' => [],
	'1 Peter' => [],
	'1 John' => [],
	'2 John' => [],
	'3 John' => [],
	'Jude' => [],
	'Revelation' => [],
);

my %book_abbrev = (
	'Genesis'   => [ qr/\AGen?\Z/i ],
	'Exodus'    => [ qr/\AEx(?:o|od)\Z/i ],
	'Leviticus' => [ qr/\ALev?\Z/i ],
	'Numbers'   => [ qr/\ANum?\Z/i],
	'Deuteronomy' => [ qr/\A(?:De|Dt|Deu|Deut)\Z/i ],
	'Joshua'    => [ qr/\AJosh?\Z/i ],
	'Judges'    => [ qr/\AJu?dg\Z/i ],
	'Ruth'      => [ qr/\ARu\Z/i ],
	'1 Samuel'  => [ qr/\A1 ?Sam?\Z/i ],
	'2 Samuel'  => [ qr/\A2 ?Sam?\Z/i ],
	'1 Kings'   => [ qr/\A1 ?K(?:i|gs)\Z/i ],
	'2 Kings'   => [ qr/\A2 ?K(?:i|gs)\Z/i ],
	'1 Chronicles' => [ qr/\A1 ?Ch(?:r(?:on)?)?\Z/i ],
	'2 Chronicles' => [ qr/\A2 ?Ch(?:r(?:on)?)?\Z/i ],
	'Ezra'      => [ qr/\AEzr\Z/i ],
	'Nehemiah'  => [ qr/\ANeh?\Z/i ],
	'Esther'    => [ qr/\AEs(?:t|th)?\Z/i ],
	'Job'       => [ ],
	'Psalms'    => [ qr/\APs(?:a|alms?|s)?\Z/i ], # Psalm, Pss, Psalms, PSA
	'Proverbs'  => [ qr/\APr(?:o|ov)\Z/i ],
	'Ecclesiastes' => [ qr/\A(?:Eccl?|Qoh)\Z/i ],
	'Song of Solomon' => [ qr(Ss So Song Sos Cant), "Song of Songs" ],
	'Isaiah'    => [ qw(is isa) ],
	'Jeremiah'  => [ 'jer' ],
	'Lamentations' => [ qw(la lam) ],
	'Ezekiel'   => [ qw(eze ezek) ],
	'Daniel'    => [ qw(da dan) ],
	'Hosea'     => [ qw(ho hos) ],
	'Joel'      => [ ],
	'Amos'      => [ 'am' ],
	'Obadiah'   => [ qw(ob obad oba) ],
	'Jonah'     => [ 'jon' ],
	'Micah'     => [ 'mic' ],
	'Nahum'     => [ qw(na nah) ],
	'Habakkuk'  => [ 'hab' ],
	'Zephaniah' => [ qw(zep zeph) ],
	'Haggai'    => [ 'hag' ],
	'Zechariah' => [ qw(zec zech) ],
	'Malachi'   => [ 'mal' ],
	# New Testament
	'Matthew'   => [ qr/(?:st\.\s*)?m(?:at)?t?/i ],
	'Mark'      => [ qr/(?:st\.\s*)?m(?:ar)?k?/i ],
	'Luke'      => [ qr/(?:st\.\s*)?lu?ke?/i     ],
	'John'      => [ qr/(?:st\.\s*)?j(?:oh)?n?/i ],
	'Acts'      => [ 'ac' ],
	'Romans'    => [ qw(ro rom) ],
	'1 Corinthians' => [ qr/\A1 ?Co(?:r|rinth)?\Z/i ],
	'2 Corinthians' => [ qr/\A2 ?Co(?:r|rinth)?\Z/i ],
	'Galatians' => [ qw(ga gal) ],
	'Ephesians' => [ qw(ep eph) ],
	'Philippians' => [ qw(php phil) ],
	'Colossians'  => [ qw(co col) ],
	'1 Thessalonians' => [ qr/\A1 ?Th(?:ess(?:allonians)?)?\Z/i ],
	'2 Thessalonians' => [ qr/\A2 ?Th(?:ess(?:allonians)?)?\Z/i ],
	'1 Timothy' => [ qr/\A1 ?Ti(?:m|mothy)?\Z/i ],
	'2 Timothy' => [ qr/\A2 ?Ti(?:m|mothy)?\Z/i ],
	'Titus'     => [ 'tit' ],
	'Philemon'  => [ qw(phm philem) ],
	'Hebrews'   => [ 'heb' ],
	'James'     => [ 'jas' ],
	'1 Peter' => [ qr/\A1 ?Pe(?:t(?:er)?)?\Z/i ],
	'2 Peter' => [ qr/\A2 ?Pe(?:t(?:er)?)?\Z/i ],
	'1 John' => [ qr/\A1 ?J(?:oh)?n\Z/i ],
	'2 John' => [ qr/\A2 ?J(?:oh)?n\Z/i ],
	'3 John' => [ qr/\A3 ?J(?:oh)?n\Z/i ],
	'Jude' => [],
	'Revelation' => [ qw(re rev apoc apocalypse) ],
);

=head2 $class->canonicalize_book($book_abbrev)

If possible, this method returns the canonical name of the book whose
abbreviation was passed.

=cut

# mdxi suggests that I could have a list of pre-limiting regex, something like
# this:
# [ qr/\A(?:1|First)/, [ '1 Kings', '1 Samuel' ...
# so that if a passed string matches the regex, it's only checked against those
# entries in the associated list; good idea, for future revision

sub canonicalize_book {
	my ($class, $book_abbrev) = @_;
	return $book_abbrev if $book_abbrev{$book_abbrev};
	my $lc_abbrev = lc($book_abbrev);
	for my $book (keys %book_abbrev) {
		return $book if lc($book) eq $lc_abbrev;
		for (@{$book_abbrev{$book}}) {
			if (ref $_) { return $book if $book_abbrev =~ m/$_/; }
			       else { return $book if $lc_abbrev eq lc($_);  }
		}
	}
	return;
}

=head2 $class->parse_range($range_string)

This method returns a hash reference describing the range described in the
passed string.

=cut

sub parse_range {
	my ($class, $range) = @_;
	my ($bc, $bv, $ec, $ev) =
		$range =~ /\A
			(\d+)       # beginning chapter
			(?::(\d+)   # maybe colon, then beginning verse
			(?:-        # maybe a dash
			(?:(\d+):)? # maybe end chapter, ending in colon
			(?:(\d+))   # and maybe the end verse
			?)?)? 
		\Z/x;

	$ec = $bc if ($ev and not $ec);
	{
		begin_chapter => $bc,
		begin_verse   => $bv,
		  end_chapter => $ec,
		  end_verse   => $ev
	}
}

=head1 AUTHOR

Ricardo Signes, C<< <rjbs@cpan.org> >>

=head1 TODO

=over 4

=item * validate ranges against book/chapter lengths

=item * clean up regex/lists

=item * create (un)loadable modules of books and abbreviations

=back

=head1 BUGS

Please report any bugs or feature requests to
C<bug-religion-bible-reference@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.  I will be notified, and then you'll automatically be
notified of progress on your bug as I make changes.

=head1 COPYRIGHT

Copyright 2005 Ricardo Signes, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;
