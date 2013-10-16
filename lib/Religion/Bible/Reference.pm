use warnings;
use strict;
package Religion::Bible::Reference;
{
  $Religion::Bible::Reference::VERSION = '0.015';
}
# ABSTRACT: canonicalize shorthand bible references

use Sub::Exporter -setup => {
  exports => [ qw(bibref) ],
  groups  => { default => [ qw(bibref) ] },
};

my %book_chapters;
my %book_abbrev;
my %book_short;

BEGIN {
  for my $attr (qw(book chapter ranges)) {
    no strict 'refs';
    *$attr = sub {
      return $_[0]->{$attr} if @_ == 1;
      return $_[0]->{$attr} = $_[1];
    };
  }
}

use Religion::Bible::Reference::Standard;


sub bibref { __PACKAGE__->new(@_); }


# ok:
# jn8
# jn8:32
# jn8:30-32
# jn8:25-28,30-32
# jn8:1,3-4,6

sub _parse_ref {
  my ($class, $ref_string) = @_;
  my $range_regex  = qr/\d+(?::(?:\d[-,]?)+)?/;

  (my $book  = $ref_string) =~ s/\s*($range_regex)\z//;
  my $ranges = $1;

  return (book => $book, ranges => $ranges);
}

sub new {
  my ($class, $ref_string) = @_;

  my %bibref = $class->_parse_ref($ref_string);

  my $self;

  return unless $self->{book}  = $class->canonicalize_book($bibref{book});

  bless $self => $class;

  return unless my $range = $self->_parse_ranges($bibref{ranges});

  $self->{chapter} = $range->{chapter};
  $self->{ranges}  = $range->{ranges};

  return unless $class->_validate_ranges(
    $self->{book},
    $self->{chapter},
    $self->{ranges},
  );

  return $self;
}

sub _validate_ranges {
  my ($class, $book, $chapter, $ranges) = @_;

  foreach my $range (@$ranges) {
    return unless $class->validate_verse($book, $chapter, $range->[0]);
    return unless $class->validate_verse($book, $chapter, $range->[1]);
  }
  return 1;
}

sub _parse_ranges {
  my ($self, $string) = @_;

  my ($chapter, $rest) = $string =~ /\A(\d+)(?::(.+))?\z/;

  return unless $chapter;
  return { chapter => $string,
           ranges => [[ 1, $book_chapters{$self->{book}}[$chapter - 1] ]] }
           unless $rest;

  my @range_strings = split /,\s?/, $rest;

  my @range;

  for my $rs (@range_strings) {
    my ($start, $end) = $rs =~ /\A(\d+)(?:-(\d+))?\z/;
    return unless $start;
    push @range, [ $start, (defined $end ? $end : $start) ];
  }

  return { chapter => $chapter, ranges => \@range };
}


sub stringify {
  my ($self) = @_;
  my $string = $self->{book}
             . q{ }
             . $self->{chapter};

  return unless @{ $self->{ranges} };

  $string .=
    ':' . join(', ', map { $self->_stringify_range($_) } @{ $self->{ranges} })
  ;
}

sub _stringify_range {
  my ($self, $range) = @_;

  map { $_->[0] == $_->[1] ? $_->[0] : "$_->[0]-$_->[1]" } $range
}

sub _register_book_set {
  my ($class, $package) = @_;

  my @books = $package->_books;
  for my $book (@books) {
    my $full = $book->{full};
    $book_chapters{ $full } = $book->{verses};
    $book_abbrev  { $full } = $book->{abbreviations};
    $book_short   { $full } = $book->{short};
  }
}


sub stringify_short {
  my ($self) = @_;

  my $string = $book_short{ $self->{book} }
             . q{ }
             . $self->{chapter};

  return unless @{ $self->{ranges} };

  $string .=
    ':' . join(', ', map { $self->_stringify_range($_) } @{ $self->{ranges} })
  ;
}

__PACKAGE__->_register_book_set("Religion::Bible::Reference::Standard");


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


sub validate_verse {
  my ($self, $book, $chapter, $verse) = @_;
  return unless exists $book_chapters{$book};
  return unless defined $book_chapters{$book}[$chapter - 1];
  return unless $book_chapters{$book}[$chapter - 1] >= $verse;
  return 1
}


sub iterator {
  my ($self) = @_;

  my $iterator = {
    book    => $self->book,
    chapter => $self->chapter,
    ranges  => [ @{ $self->ranges } ],
  };

  bless $iterator => 'Religion::Bible::Reference::Iterator';
}

package Religion::Bible::Reference::Iterator;
{
  $Religion::Bible::Reference::Iterator::VERSION = '0.015';
}

sub next { ## no critic # honestly, next is a great method for an iterator
  my ($self) = @_;
  return unless @{ $self->{ranges} };

  $self->{position} ||= $self->{ranges}[0][0];
  my $position = $self->{position};

  if ($position == $self->{ranges}[0][1]) {
    shift @{ $self->{ranges} };
    undef $self->{position};
  } else {
    $self->{position}++;
  }
  return wantarray ? (@$self{qw(book chapter)}, $position) : $position;
}


1;

__END__

=pod

=head1 NAME

Religion::Bible::Reference - canonicalize shorthand bible references

=head1 VERSION

version 0.015

=head1 SYNOPSIS

 use Religion::Bible::Reference;

 my $quote = bibref("jn8:32");

 print "($quote)";   # (John 8:32)
 print $quote->book; # John

=head1 DESCRIPTION

This module converts simple text descriptions of bible references and ranges
into objects that stringify into a canonical form.

B<WARNING!>  This module is mostly an idea and not so much a guaranteed
interface or well-tested implementation.  If you're interested in either of
those existing, you should let me know.

=head1 METHODS

=head2 new

  my $ref = Religion::Bible::Reference->new($ref_string)

This method acts just like the exported C<bibref> function.

=head2 stringify

  $self->stringify

This method returns a string representing the reference, using the canonical
book name.

=head2 stringify_short

  my $str = $self->stringify_short

This method returns a string representing the reference, using the short book
name.

In other words, John 8:32 would be Jn 8:32.  All short forms should safely
round-trip back via parsing.

=head2 canonicalize_book

  my $book = $class->canonicalize_book($book_abbrev)

If possible, this method returns the canonical name of the book whose
abbreviation was passed.

=head2 validate_verse

  $class->validate_verse($book, $chapter, $verse)

This method returns true if the given book, chapter, and verse exists;
otherwise it returns false.

=head2 iterator

  my $iterator = $bibref->iterator;

  while (my $verse = $iterator->next) {
    my $text = retrieve($verse);
    print "$text\n";
  }

=head1 FUNCTIONS

=head2 bibref

  my $ref = bibref($ref_string)

This function is exported by default, and constructs a new
Religion::Bible::Reference

Reference strings must be a book followed by a list of chapters, verses, or
ranges.  The following are all valid ranges:

  Pro 23:12, 23:15-17
  st.jn8:32
  Song of Solomon 8:7-8
  2 John 1

=head1 TODO

=over 4

=item *

allow L<Text::Abbrev> instead of registered abbrevs

=item *

clean up regex/lists

=item *

make public the interface to load modules of books and abbreviations

=item *

make an interface to unload modules

=back

=head1 AUTHOR

Ricardo SIGNES <rjbs@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2005 by Ricardo SIGNES.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
