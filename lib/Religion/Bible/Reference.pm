package Religion::Bible::Reference;

use warnings;
use strict;

use Exporter;
@Religion::Bible::Reference::ISA = qw(Exporter);
@Religion::Bible::Reference::EXPORT = qw(bibref);

use Religion::Bible::Reference::Standard;

=head1 NAME

Religion::Bible::Reference - canonicalize shorthand bible references

=head1 VERSION

version 0.00_03

 $Id$

=cut

our $VERSION = '0.00_03';

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
	return unless $class->validate_verse(
		$bibref{book}, $bibref{range}{begin_chapter}, $bibref{range}{begin_verse}
	);
	if (defined $bibref{range}{end_chapter}) {
		return unless $class->validate_verse(
			$bibref{book}, $bibref{range}{end_chapter}, $bibref{range}{end_verse}
		)
	}
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


my %book_chapters;
my %book_abbrev;

sub _register_book_set {
	my ($class, $package) = @_;
	my $standard = $package->_books;
	%book_chapters = (%book_chapters, %{$standard->{chapters}});
	%book_abbrev   = (%book_abbrev,   %{$standard->{abbrev}});
}

__PACKAGE__->_register_book_set("Religion::Bible::Reference::Standard");

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

=head2 $class->validate_verse($book, $chapter, $verse)

This method returns true if the given book, chapter, and verse exists;
otherwise it returns false.

=cut

sub validate_verse {
	my ($self, $book, $chapter, $verse) = @_;
	return unless exists $book_chapters{$book};
	return unless defined $book_chapters{$book}[$chapter - 1];
	return unless $book_chapters{$book}[$chapter - 1] >= $verse;
	return 1
}

=head1 AUTHOR

Ricardo Signes, C<< <rjbs@cpan.org> >>

=head1 TODO

=over 4

=item * allow Text::Abbrev instead of registered abbrevs

=item * clean up regex/lists

=item * make public the interface to load modules of books and abbreviations

=item * make an interface to unload modules

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
