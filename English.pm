package Regexp::English;

use strict;
use base qw( Exporter );
use vars qw( @export @EXPORT_OK %EXPORT_TAGS $VERSION );

use overload
	'""' => \&compile;

# REGEX: where the raw regex is stored
# STORE: where bound references are stored (see remember())
# STACK: used to nest groupings
use constant REGEX => 0;
use constant STORE => 1;
use constant STACK => 2;

$VERSION = '0.21';

# the key is the name of the method to be created
# 	symbol is the regex token this represents
#	plural is the name of the shortcut method for $symbol+, i.e. \w+
#	non is the name of the negated token, its shortcut, and a plural, if needed
my %chars = (
	word_char => {
		symbol => '\w',
		plural => 'word_chars',
		non => [ 'non_word_char', '\W', 'non_word_chars' ],
	},
	whitespace_char => {
		symbol => '\s',
		plural => 'whitespace_chars',
		non => [ 'non_whitespace_char', '\S', 'non_whitespace_chars' ],
	},
	digit => {
		symbol => '\d',
		plural => 'digits',
		non => [ 'non_digit', '\D', 'non_digits' ],
	},
	word_boundary => {
		symbol => '\b',
		non => [ 'non_word_boundary', '\B' ],
	},
	end_of_string => {
		symbol => '\Z',
		non => [ 'very_end_of_string', '\z' ],
	},
	beginning_of_string => {
		symbol => '\A',
	},
	end_of_previous_match => {
		symbol => '\G',
	},

# XXX: non for these ?
	tab => {
		symbol => '\t',
		plural => 'tabs',
	},
# XXX: should imply /s modifier
	newline => {
		symbol => '\n',
		plural => 'newlines',
	},
	carriage_return => {
		symbol => '\r',
		plural => 'carriage_returns',
	},
	form_feed => {
		symbol => '\f',
		plural => 'form_feeds',
	},
	alarm => {
		symbol => '\a',
		plural => 'alarms',
	},
	escape => {
		symbol => '\e',
		plural => 'escapes',
	},
	start_of_line => {
		symbol => '^',
	},
	end_of_line => {
		symbol => '$',
	},
);

sub chars {
	my $symbol = shift;
	return sub { 
		# cannot use $_[0] here, as it trips the overload
		# that can mess with remember/end groups
		return $symbol unless @_;

		my $self = shift;
		unless( ref $self and (UNIVERSAL::isa($self, 'Regexp::English')) ) {
			$self = $self->new();
		}
		$self->[REGEX] .= $symbol;
		return $self;
	};
}

my @char_tags;
foreach my $char (keys %chars) {
	push @char_tags, $char;
	install( $char, chars($chars{$char}{symbol}) );

	if ($chars{$char}{plural}) {
		install( $chars{$char}{plural}, chars( $chars{$char}{symbol} . '+'));
		push @char_tags, $chars{$char}{plural};
	}

	if ($chars{$char}{non}) {
		my ($nonname, $symbol, $pluralname) = @{ $chars{$char}{non} };
		install( $nonname, chars( $symbol ) );
		push @char_tags, $nonname;
		if ($pluralname) {
			install( $pluralname, chars( $symbol . '+' ) ) ;
			push @char_tags, $pluralname;
		}
	}
}

# tested in t/quantifiers
# XXX:
#	the syntax for minimal/optional is slightly awkward
my %quantifiers = (
	zero_or_more => '*',
	multiple => '+',
	minimal => '?',
	optional => '?',
);

foreach my $quantifier (keys %quantifiers) {
	install( $quantifier, 
		standard( '(?:','', $quantifiers{$quantifier} .')' ), 1);
}

# tested in t/groupings
my %groupings = (
	comment => '(?#',
	group => '(?:',
	followed_by => '(?=',
	not_followed_by => '(?!',
	after => '(?<=',
	not_after => '(?<!',
);

foreach my $group (keys %groupings) {
	install( $group, standard( $groupings{$group}, '', ''), 1);
}

sub standard {
	my ($group, $sep, $symbol) = @_;
	$group	||= '(?:';
	$sep	||= '';
	$symbol	||= ')';

	return sub {
		if (UNIVERSAL::isa($_[0], 'Regexp::English')) {
			my $self = shift;
			$self->[REGEX] .= $group;

			if (@_) {
				$self->[REGEX] .= join("$sep", @_) . $symbol;
			} else {
				push @{ $self->[STACK] }, $symbol;
			}
			return $self;
		}
		return $group . join($sep, @_) . $symbol;
	};
}

# can't be used with standard because of quotemeta()
sub literal {
	my $self = shift;
	if (@_ > 1) {
		$self->[REGEX] .= '(?:' . join('', map { quotemeta($_) } @_) . ')';
	} else {
		$self->[REGEX] .= quotemeta(+shift);
	}
	return $self;
}

sub install {
	my ($name, $sub, $export) = @_;
	no strict 'refs';
	*{$name} = $sub;
	if ($export) {
		push @export, "&$name";
	}
	push @EXPORT_OK, "&$name";
}

install( 'or', sub {
	if (UNIVERSAL::isa($_[0], 'Regexp::English')) {
		my $self = shift;
		if (@_) {
			$self->[REGEX] .= '(?:' . join('|', @_) . ')';
		} else {
			$self->[REGEX] .= '|';
		}
		return $self;
	}
	return '(?:' . join('|', @_) . ')';
}, 1);

install( 'class', standard( '[', '', ']' ), 1 );
# XXX - not()

sub remember {
	my $self = shift;

	$self = $self->new() unless ref $self 
		and UNIVERSAL::isa($self, 'Regexp::English');

	# the first element may be a reference, so stick it in STORE
	if (ref($_[0]) eq 'SCALAR') {
		push @{ $self->[STORE] }, shift;
	}

	# if there are other arguments, add them to REGEX
	if (@_) {
		$self->[REGEX] .= '(' . join('', @_) . ')';

	# otherwise, this is the opening op of a multi-call remember block
	# XXX: might store calling line for verbose debugging
	} else {
		$self->[REGEX] .= '(';
		push @{ $self->[STACK] }, ')';
	}

	return $self;
}

sub end {
	my ($self, $levels) = @_;
	$levels = 1 unless defined $levels;

	unless (defined $self->[STACK] and @{ $self->[STACK] }) {
		require Carp;
		Carp::confess("end() called without remember()");
		return $self;
	}

	for (1 .. $levels) {
		last unless @{ $self->[STACK] };
		$self->[REGEX] .= pop @{ $self->[STACK] };
	}

	return $self;
}

sub new {
	bless([ '', [] ], $_[0]);
}

sub match {
	my $self = shift;
	$self->[REGEX] = $self->compile();
	if (@{ $self->[STORE] }) {
		return $self->capture($_[0] =~ $self->[REGEX]);
	} else {
		if (wantarray()) {
			return $_[0] =~ $self->[REGEX];
		} else {
			return ($_[0] =~ $self->[REGEX])[0] || undef;
		}
	}
}

sub capture {
	my $self = shift;
	for my $ref (@{ $self->[STORE] }) {
		$$ref = shift @_;
	}
	if (wantarray()) {
		return map { $$_ } @{ $self->[STORE] };
	} else {
		return ${ ${ $self->[STORE] }[0] };
	}
}

sub compile {
	my $self = shift;

	if (defined $self->[STACK]) {
		my $num = @{ $self->[STACK] };
		$self->end($num) if $num;
	}
	return qr/$self->[REGEX]/;
}

sub debug {
	my $self = shift;
	return $self->[REGEX];
}

%EXPORT_TAGS = (
	all			=> [ @char_tags, @export ],
	chars       => \@char_tags,
	standard    => \@export,
);

1;

__END__

=head1 NAME

Regexp::English - Perl module to create regular expressions more verbosely

=head1 SYNOPSIS

	use Regexp::English;

	my $re = Regexp::English
		-> start_of_line
		-> literal('Flippers')
		-> literal(':')
		-> optional
			-> whitespace_char
		-> end
		-> remember
			-> multiple
				-> digit;
	
	while (<INPUT>) {
		if (my $match = $re->match($_)) {
			print "$match\n";
		}
	}

=head1 DESCRIPTION

Regexp::English provides an alternate regular expression syntax, one that is
slightly more verbose than the standard mechanisms.  In addition, it adds a few
convenient features, like incremental expression building and bound captures.

Nearly every regular expression available in Regexp::English can be accessed
through a method, though some are also (or only) available as functions.  These
methods can roughly be divided into several categories: characters,
quantifiers, groupings, and miscellaneous.  The division wouldn't be so rough
if the latter had a better name.

All methods return the Regexp::English object, so method calls can be chained,
as in the example above.  Though there is a C<new()> method, any character
method can be used to create an object, as can C<remember()>.

Matches are performed with the C<match()> method.  Alternately, if a
Regexp::English object is used as if it were a compiled regular expression, it
will be automagically compiled behind the scenes.

=head2 Characters

Character methods correspond to standard regular expression characters and
metacharacters, for the most part.  As a little bit of syntactic sugar, most of
these methods have plurals, negations, and negated plurals.  This is more clear
looking at them.  Though these are designed to be called on a new
Regexp::English object while building up larger regular expressions, they may
be used as class methods to access regular expression atoms, which are then
used in larger regular expressions.  This isn't entirely pretty, but it ought
to work just about everywhere.

=over 4

=item * C<literal()>

Matches the provided literal string.  Note that anything provided will be
passed through C<quotemeta()> automatically.  If you're getting strange
results, it's probably because of this.

=item * C<class()>

Creates and matches a character class of the provided characters.  Note that
there is currently no validation of the character class, so you can create an
uncompilable regular expression if you're not careful.

=item * C<word_char()>

Matches any word character, respecting the current locale.  By default, this
matches alphanumerics and the underscore, corresponding to the C<\w> token.
The related C<word_chars()> matches at least one word character, and
C<non_word_char()> and C<non_word_chars()> match anything that is not an
alphanumeric or underscore one or at least one of these characters,
respectively.

=item * C<whitespace_char()>

Matches any whitespace character, corresponding to the C<\s> token.  The
corresponding plural is C<whitespace_chars()>, with negations of
C<non_whitespace_char()> and C<non_whitespace_chars()>.

=item * C<digit()>

Matches any numeric digit, corresponding to the C<\d> token  The plural is
C<digits()>, with negations of C<non_digit()> and C<non_digits()>.

=item * C<tab()>

Matches a tab character (C<\t>).  The plural is C<tabs()>, and there is no
negation.

=item * C<newline()>

Matches a newline character (C<\n>).  The plural is C<newlines()>, and there is
no negation.  This should imply the C</s> modifier, but it does not yet do so.

=item * C<carriage_return()>

Matches a carriage return character (C<\r>).  The plural is
C<carriage_returns()>, and there is no negation.

=item * C<form_feed()>

Matches a form feed character (C<\f>).  The plural is C<form_feeds()>, and
there is no negation.

=item * C<alarm()>

Matches an alarm character (C<\a>).  The plural is C<alarms()>, and there is no
negation.

=item * C<escape()>

Matches an escape character (C<\e>).  The plural is C<escapes()>, and there is
no negation.

=item * C<start_of_line()>

Matches the start of a line, just like the C<^> anchor.

=item * C<beginning_of_string()>

Matches the beginning of a string, much like the C<^> anchor.

=item * C<end_of_line()>

Matches the end of a line, just like the C<$> anchor.

=item * C<end_of_string()>

Matches the end of a string, much like the C<$> anchor, treating newlines
appropriately depending on the C</s> or C</m> modifier.

=item * C<very_end_of_string()>

Matches the very end of a string, just as the C<\z> token.  This does not
ignore a trailing newline (if it exists).  

=item * C<end_of_previous_match()>

Matches the point at which a previous match ended, in a C<\g>lobally-matched
regular expression.  This corresponds to the C<\G> token and is related to
C<pos()>.

=item * C<word_boundary()>

Matches the zero-width boundary between a word character and a non-word
character, corresponding to the C<\b> token.  There is no plural, but the
negation is C<non_word_boundary()>.

=back

=head2 Quantifiers

Quantifiers provide a mechanism to specify how many items to expect, in general
or specific terms.  These may be exported into the calling package's namespace
with the C<:standard> argument to the C<use()> call, but the preferred
interface is to use them as method calls.  This is slightly more complicated,
but cleaner conceptually.  The interface may change slightly in the future, if
someone comes up with something even better.

By default, quantifiers operate on the I<next> arguments, not the previous
ones.  (It is much easier to program this way.)  For example, to match multiple
digits, one might write:

	my $re = Regexp::English->new
		->multiple
			->digits;

The indentation should make this more clear.

Quantifiers persist until a match is attempted or the corresponding C<end()>
method is called.  As C<match()> calls C<end()> internally, all active
quantifiers will be closed when a match is attempted.  There is currently no
way to re-open a quantifier even if you add to a Regexp::English object.  This
is a non-trivial problem (as the author understands it), and there's no good
solution for it in normal regular expressions anyway.

If you have imported the quantifiers, you can pass the quantifiables as
arguments:

	use Regexp::English qw( :standard );

	my $re = Regexp::English->new
		->multiple('a');

The open quantifier will automatically be closed for you.  Though this syntax
is slightly more visually appealing, it does involve exporting quite a few
methods into your namespace, and is thus not the default.  Besides that, if you
get in this habit, you'll eventually have to use the C<:all> tag.  Better to
get used to the method calls, or to push Vahe to write Regexp::Easy.  :)

=over 4

=item * C<zero_or_more()>

Matches as many items as possible.  Note that "possible" includes matching zero
items.  Note also that "item" means "whatever it's told to match".  By default,
this is greedy.

=item * C<multiple()>

Matches at least one item, but as many as possible.  By default, this is
greedy.

=item * C<optional()>

Marks an item as optional -- that is, the pattern will match with or without
the item.

=item * C<minimal()>

This quantifier modifies either C<zero_or_more()> or C<multiple()>, and
disables greediness, asking for as few matches as possible. 

=back

=head2 Groupings

Groupings function much the same as quantifiers, though they have semantic
differences.  The most important similarity is that they can be used with the
function or the method interface.  Obviously, the method interface is
preferable, but see the documentation for C<end()> for more information.

Groupings generally correspond to advanced Perl regular expression features
like lookaheads and lookbehinds.  If you find yourself using them on a regular
basis, you're probably ready to graduate to hand-rolled regular expressions (or
to contribute code to improve Regexp::English :).

=over 4

=item * C<comment()>

Marks the item as a comment, which has no bearing on the match and really
doesn't give you anything here either.  Don't let that stop you, though.

=item * C<group()>

Groups items together (often to use a single quantifier on them) without
actually capturing them.  This isn't very useful either, because the
Quantifiers handle this for you.

=item * C<followed_by()>

Marks the item as a zero-width positive look-ahead assertion.  This means that
the pattern must match the item after the previous bits, but the item is not considered part of the
matched string.

=item * C<not_followed_by()>

Marks the item as a zero-width negative look-ahead assertion.  This means that
the pattern must not match the item after the previous bits, but the item is still not considered part
of the matched string.

=item * C<after()>

Marks the item as a zero-width positive look-behind assertion.  This means the
pattern must match the item before the following bits.  Super funky, and may
have subtle bugs -- look-behinds tend to need fixed width items, and
Regexp::English currently doesn't enforce this.

=item * C<not_after()>

Marks the item as a zero-width negative look-behind assertion.  This means the
pattern must not match the item before the following bits.  This is also
susceptible to the fixed-width rule.

=back

=head2 Miscellaneous

These subroutines don't really fit anywhere else.  They're useful, and mostly
cool.

=over 4

=item * C<new()>

Creates a new Regexp::English object.  Though some methods do this for you
automagically if you need one, this is the best way to start a regular
expression.

=item * C<match()>

Compiles and attempts to match the Regexp::English object against a passed-in
regular expression.  If there are any captured variables, they'll be returned.
Otherwise, a true or false value will be returned.

=item * C<remember()>

Causes Regexp::English to remember an item which will then be returned or
otherwise made available when calling C<match()>.  Normally, these items are
returned from C<match()> in order of their declaration within the regular
expression.  They may also be bound to variables.  Pass in a reference to a
scalar as the first argument and the scalar will automagically be populated
with the matched value on each subsequent match.  That means you can write:

	my ($first, $second);

	my $re = Regexp::English->new
		->remember(\$first)
			->multiple('a')
			->remember(\$second)
				->word_char;

	foreach my $match (qw( aab aaac ad )) {
		if ($re->match($match)) {
			print "$second\t$first\n";
		}
	}

This will print:

	b	aaab
	c	aac
	d	ad

Pretty cool, no?

=item * C<end()>

Ends an open Quantifier or Grouping.  If you pass no arguments, it will end
only the most recently opened item.  If you pass a numeric argument, it will
end that many recently opened items.  It does not currently check to see if you
pass in a number, so only pass in numbers, or be prepared for odd results.

=item * C<compile()>

Compiles and returns the pattern-in-progress, ending any and all open
Quantifier or Groupings.  This uses C<qr//>.  Note if you attempt anything that
could stringify the object, this method is called.  This appears to include
treating a Regexp::English object as a regular expression.  Nifty.

=item * C<or()>

Provides alternation capabilities.  This has been improved in version 0.21 to
the point where it is actually useful.  The preferred interface is very similar
to Grouping calls:

	my $re = Regexp::English->new
		->group
			->digit
			->or
			->word_char;

Wrapping the entire alternation in C<group()> or some other Grouping method is
highly recommended, as you might want to use a Quantifier or something more
complex:

	my $re = Regexp::English->new
		->remember
				->literal('root beer')
			->or
				->literal('milkshake')
		->end;

If you find this onerous, you can also pass arguments to C<or()>, which will be
grouped together in non-capturing braces.  Note that you will have to import
the appropriate functions or fully qualify them.  Calling these functions as
class methods is not currently guaranteed to work reliably.  It may never be
guaranteed to work reliably.  Properly indented, the method interface looks
nicer anyway, but you have two options:

	my $functionre = Regexp::English->new
		->or( Regexp::English::digit, Regexp::English::word_char );
	
	my $classmethodre = Regexp::English->new
		->or( Regexp::English->digit, Regexp::English->word_char );


=item * C<debug()>

Returns the regular expression so far.  This can be handy if you know what
you're doing.

=back

=head1 EXPORTS

By default, nothing is exported.  This is an object oriented module, and this
is how it should be.  You can import the Quantifier and Grouping subroutines by
providing the C<:standard> argument to the C<use()> line, and the Character
methods with the C<:chars> tag.

	use Regexp::English qw( :standard :chars );

You could also use the C<:all> tag:

	use Regexp::English qw( :all );

This interface may change slightly in the future.  If you find yourself
exporting things, you should look into Vahe Sarkissian's upcoming Regexp::Easy
module.  This is probably news to him, too.  :)

=head1 TODO

=over 4

=item * Add C<not()>

=item * More error checking

=item * Add a few tests here and there

=item * Add POSIX character classes ?

=item * Delegate to Regexp::Common ?

=item * Allow other language backends (probably just add documentation for this)

=item * Improve documentation

=back

=head1 AUTHOR

chromatic, E<lt>chromatic@wgz.orgE<gt>, with many suggestions from Vahe
Sarkissian E<lt>vsarkiss@pobox.comE<gt> and Damian Conway
E<lt>damian@cs.monash.edu.auE<gt>

=head1 COPYRIGHT

Copyright 2001-2002 by chromatic.

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=head1 SEE ALSO

L<perlre>

=cut
