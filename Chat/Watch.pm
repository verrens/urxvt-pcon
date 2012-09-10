package Chat::Watch;
use strict;
use warnings;
use Scalar::Util qw/weaken/;
use Carp;

sub new (@) {#{{{
    my ($class, $term, $hdl) = @_;
    croak "Terminal obj ref expected!" unless ref($term);
    croak "Handler sub expected!" unless ref $hdl eq 'CODE';
    my $I = bless { }, $class or croak "Where am I?";
    weaken (my $MyOwn = $I);
    #
    warn "Creating $I";
    # Dup of current screen ctx
    my @brows; my $bbase;
    (my $reset = sub {
            ($bbase, @brows) = (0,
                map { s/\s+$//; $_ }
                map $term->special_decode($term->ROW_t($_)),
                    (0..$term->nrow-1)); # ($term->top_row ..
            () # Don`t leaking
        })->();
    my %cbs = (
        line_update => sub {
            my $r_t = $term->special_decode($term->ROW_t(my $r = pop));
            $r_t =~ s/\s+$//; # FIXME ???
            # TODO Efficient?
            return if $r_t eq ($brows[my $b = $bbase+$r] ||= '');
            # $handler->($row_number, $text, $oldtext)
            #   if return true, destroy myself.
            local $_ = $r_t;
            $hdl->($MyOwn, $r, $r_t, $brows[$b])
                and $MyOwn->destroy;
            $brows[$b] = $r_t;
            ()
        },
        reset => $reset,
        # TODO FIXME resize handling
#         resize_all_windows => $reset,
        scroll_back => sub { shift; $bbase += shift; () },
    );
    do {
        local $term->{_pkg} = "$MyOwn"; # Stupid hack
        $term->enable( map { $_ => $cbs{$_} } keys %cbs );
        $I->{_destroy} = sub {
            warn "Destroying $MyOwn";
            local $term->{_pkg} = "$MyOwn"; # Stupid, so stupid hack..
            $term->disable(keys %cbs);
            1
        }
    };
    weaken $reset;
    return $I
}#}}}

sub destroy { ($_[0]->{_destroy} || sub { warn "Who are I am? (@_)" })->() }

sub DESTROY { shift->destroy }

1;
