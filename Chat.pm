# Chatter, copyright (c) 2011-2012 Afonin Denis <verrens@cpan.org>
# NO PACKAGE NAME IN THERE! Running at the stash.
use strict;
use warnings;
use POSIX qw(strftime);
# Подстелю соломку (confess)
use Carp;
use Symbol qw(delete_package);
use IO::Pty;
use Fcntl;
# О да!
use Chat::Watch;
#
use Scalar::Util qw/weaken/;

## TODO#{{{
# * DIRTY!!!!!! (devide et empera)
# * FIXME (hl 3, 1)->(0) -> error; reboot; (hl ..)->(0) -> ok
#
# * Moduling!
# * Help
# }}}

my $loaded = time;
my $version = 44; # TODO Normal package version

## urxvt-term related (dirty!)#{{{
# after 5, sub { warn 'xexexe'; heap xe=>sel }, leave
def leave => sub () { term->leave };
def name => sub (;$) { term->resource("name", $_[0]||()) };
def prompt => sub (;$) { con -edit_arg => -prompt => @_ };
def lsterms => sub { map $_->resource("name"), urxvt::termlist };
def after => sub ($&) {#{{{
        my ($sec, $fun) = @_;
        urxvt::timer
        ->new
        ->start (urxvt::NOW + $sec)
        ->cb ( $fun ) };#}}}
def timer => sub (&$) {#{{{
        my ($fun, $sec) = @_;
        urxvt::timer
          ->new
          ->interval ($sec) ->cb ( $fun ) };
#     # set urgent flag (and ring bell, of couse!)#}}}
def bell => sub () { term->scr_bell; term->want_refresh };
def tty_lock => sub {#{{{
    term->{ev_saved} = term->pty_ev_events(
        urxvt::EV_NONE ) };#}}}
def tty_unlock => sub { term->pty_ev_events( term->{ev_saved} ) };
def sel => sub () { term->selection };
def selarea => sub () { term->selection_beg, term->selection_end };
def enc => sub (@) { map term->locale_encode($_), @_ };
def dec => sub (@) { map term->locale_decode($_), @_ };
def line => sub ($) { term->line(shift) };
def color => sub ($;$) { #{{{
        # Set screen line color
        # Returns old color map, eg:
        #  my $saved = color $line, $color
        #  ...
        #  color $line, $saved # Restore original colors
        # FIXME Ugggly ;-[
        my ($line, $col) = @_;
        my $srend = term->ROW_r($line)
            or return;
        term->ROW_r($line, (ref $col eq 'ARRAY'
                ?  $col
                : ([map { $col } @{$srend}])));
        term->want_refresh;
        $srend };#}}}
def lineattr => sub ($&@) { #{{{
    # $saved_color = lineattr $line_num, sub { shift | urxvt::RS_Bold }
    # color $line_nun, $saved_color
    my ($l, $f, @a) = @_;
    my $s = term->ROW_r($l)
        or return;
    term->ROW_r($l, [map $f->($s->[$_], $_, @a)|$s->[$_], 0..$#$s]);
    term->want_refresh;
    $s };#}}}
def selattr => sub (&@) {#{{{
    # $restore = selattr sub { shift | urxvt::RS_Bold }
    # ...; $restore->()
    my ($yb, $xb, $ye, $xe, $f, @a) = (selarea(), @_);
    return unless $xe > $xb and $ye >= $yb;
    my @s = map lineattr($_, sub {
            my ($v, $x, $y) = @_;
            # FIXME One-line selection
            $y==$yb && $x>=$xb || $y==$ye && $x<$xe || $y>$yb && $y<$ye
                and $f->($v, $x, $y, @a) }, $_), $yb..$ye;
    sub { color($yb+$_, $s[$_]) for 0..($ye-$yb); undef @s } };#}}}
def selhl => sub (;$) { my $a=shift || urxvt::RS_Uline;
    selattr(sub { shift | $a }) };
def clipboard => sub ($;$) {#{{{
    term->selection(my ($v, $c)=@_);
        term->selection_grab(urxvt::CurrentTime, $c) and $v };#}}}
# New terminal, name expected. ENV cloned.
def newterm => sub ($@) { new urxvt::term { %ENV }, @_ };
# New terminal with new PTY
def newtermpty => sub ($@) {#{{{
        my $pty = new IO::Pty;
        warn "New PTY allocated: ", fileno $pty;
        # FIXME Separated %ENV
        my $t = new urxvt::term { %ENV,
                URXVT_PTY => fileno $pty, TERM => 'linux'}, @_,
            '-pty-fd' => fileno($pty),
            '-tn' => 'linux';
        # now communicate with new rxvt
        [$t, $pty] };#}}}
# New terminal via exec
def newtermptysys => sub ($@) {#{{{
        # From man urxvt
        my $pty = new IO::Pty;
        fcntl $pty, F_SETFD, 0; # clear close-on-exec
        system 'urxvt -pty-fd '.fileno($pty).'&';
        close $pty;
        # now communicate with new rxvt
        $pty->slave };#}}}
# FIXME From perlcon!!!
def defcolor => sub () { urxvt::SET_COLOR(urxvt::OVERLAY_RSTYLE, 77, 1) };
# Pager#{{{
# newpager :: ArrayRef -> PagerArgsHash -> PagerHandle
def newpager => sub (@) {#{{{
  my $v = shift or die "var expected";
  urxvt::ext::perlcon::urxvt_pager 
    -var => $v,
    -y => 1,
    -x => 82,
    -tail => 1,
    -width => 37,
    -height => 3,
    -border => 1,
    -color => defcolor(),
    @_, -urxvt => term() };#}}}
# pager :: PagerArgsHash -> PagerHandle
def pager => sub (@) { my $v = []; newpager($v, @_) };
def parg => sub ($@) { my $h=shift; $h->{arg}->(@_) }; # >>=
# pvar :: PagerHandle -> ArrayRef
def pvar => sub ($) { ($_[0]->{arg}->(-var))[0] }; # FIXME Ugly
def ppush => sub (@) { push @{pvar($_[0])}, @_[1..$#_] };
# pvisible :: Maybe Bool -> PagerHandle -> Bool
def psetvisible => sub ($\$) { shift->{visible}->(shift) };
def pvisible => sub ($) { psetvisible(undef, shift) };
def pshow => sub ($) { psetvisible(1, shift) };
def phide => sub ($) { psetvisible(0, shift) };
def pdestroy => sub ($) { shift->{destroy}->() };
#}}}
#}}}

## pcon relate#{{{
def height => sub (;$) { con '-con_arg' => -y => -shift };
#}}}

# def dbi => sub () # my PostgreSQL#{{{
do { my $dbi;
  def dbi => sub () { ($dbi && $dbi->ping) ? $dbi :#{{{
    eval {
        # side effect | magick | Monad, may be
        # FIXME Dirty, yes.
        require DBI; 
        $dbi = DBI->connect("dbi:Pg:dbname=den",
          '', '', {AutoCommit => 0, RaiseError => 1})
      } || (carp("DBI error $!"), undef) };#}}}
};
#}}}

## Stash#{{{
# def bag => sub (;$$) {{{
do { my %bag;
    def bag => sub (;$$) {
            my $k = shift
                or return keys %bag;
            @_ ? ((undef, $bag{$k}) = ($bag{$k}, shift))[1] : $bag{$k} }};#}}}
def heap => sub (;$$) {#{{{
        no strict; # FIXME Ugly
        local *heap = *urxvt::ext::perlcon::heap;
        my $k = shift
            or return keys %heap;
        return $heap{$k} unless @_;
        my $r;
        (($r, $heap{$k}) = ($heap{$k}, shift));
        use strict;
        $r };#}}}
def pheap => sub (;$$) {#{{{
  # TODO Updating!
  # dbi->do("create table pheap (k text primary key, v text[] not null"
  #   .", m text[] default null)");
  @_ ? $#_>0
      ? (pheap($_[0]) || @{dbi()->selectrow_array(
        "insert into pheap (k, v) values (?, ?) returning v",
          {}, $_[0], [@_[1..$#_]])})
      : @{dbi()->selectrow_array("select v from pheap where k = ?", {}, shift)||[]}
    : map $_->[0], @{dbi()->selectall_arrayref("select k from pheap")} };#}}}
#}}}
## Misc#{{{
def version => sub { $version };
# def pwgen => sub (;$)#{{{
do { my @pwc = ('a'..'z', 'A'..'Z', 0..9, split //, '.:^#~,');
    def pwgen => sub (;$) { join '', map { @pwc[ rand(37 * $_) % $#pwc ]
        } (1..shift||12) } };#}}}
def md5sum => sub (@) { use Digest::MD5 qw/md5_hex/;#{{{
    md5_hex(scalar @_ ? @_ : $_) };#}}}
def shasum => sub (@) { use Digest::SHA qw/sha1_hex/;#{{{
    sha1_hex(scalar @_ ? @_ : $_) };#}}}
def openser => sub ($@) {#{{{
    sysopen my $f, shift, O_RDWR|O_NONBLOCK|O_NDELAY
        or die "openser sysopen: $!";
    $f
};
#}}}
def pc => sub { no strict "refs"; &${"PCon::t"} };
# shasum readf "/etc/passwd"
def readf => sub { open my $f, shift or return; binmode $f; my $d;#{{{
    while (<$f>) { $d.=$_ }; close $f; $d };
#}}}
def mark => sub () { p(8x80, "\n"); warn "! ", ts() };
def unuse => sub ($) {#{{{
  my $k=shift;
  delete $INC{$_} for grep /$k\.pm$/, keys %INC
};
#}}}
#}}}

## Shotcuts #{{{
def f => sub (@) { map ref()?&$_:$_, @_ }; # Just f
# Paste to terminal: shotcut
def p => sub (@) { paste_term(@_ ? @_ : ($_)) };
def h => sub (;$) { height (shift || 0) };
def d => sub (@) {#{{{ Dump
  require Data::Dumper;
  local $Data::Dumper::Terse = 1;
  local $Data::Dumper::Indent = 0;
  local $Data::Dumper::Quotekeys = 0;
  local $Data::Dumper::Deparse = 1; # FIXME !!!
  Data::Dumper::Dumper(@_) };#}}}
# FIXME Strange, very strange..
def c => sub (@) { clipboard("@_"||$_) };
# (hl $color, $timeout)->($row)#{{{
do { my %mytimers;
    def hl => sub (@) {
        my $color = shift || 2;
        my $next = pop if ref $_[-1] eq 'CODE';
        my $tout = shift || 1; # TODO Or infinity?
        sub {
            my $r = shift;
            my ($key, $saved) = ("hlc $r", color $r, $color);
            if (ref $mytimers{$key}) {
                $mytimers{$key}->after($tout); }
            else {
                $mytimers{$key} = after($tout, sub {
                    delete $mytimers{$key}; color $r, $saved }) }
            $next ? $next->($r, @_) : 1 } } }; #}}}
# def ts => sub () { strftime "[%Y-%m-%d %H:%M:%S] ", localtime };
def tsfmt => sub () {
    sub { sprintf pop, (shift)+1900, (shift)+1, @_ }
        ->( (localtime(time))[reverse 0..5], shift ) };
def ts => sub () { tsfmt("%4.4i-%2.2i-%2.2i %2.2i:%2.2i:%2.2i") };
def date => sub () { tsfmt("%4.4i_%2.2i_%2.2i") };

def t => sub { newterm(shift || 'Unit 22'); (term)->leave };
def ss => sub { @_=split /\s*\n\s*/, sel(); wantarray ? @_ : [@_] };
def e => sub (&@) { eval { $_ = shift->(@_) }; $@ ? "died: $@" : $_ };
def env => sub { my $e=term()->env; @_ ? $e->{+shift}
    : (map "$_=".$e->{$_}, sort keys %{$e}) };
def sh => sub ($) { dec(`@_`) };
def head => sub (@) { $_[0] };
def tail => sub (@) { @_[1..$#_] };
def sl => sub () { head(selarea()) };
def l => sub (@) { map term()->ROW_t($_), scalar @_ ? @_ : sl() };
do { my %ppp; # ppp: new pager, create handler shotcut#{{{
  # FIXME Ugggly...
  def ppp => sub ($@) {
    my $k = shift or return keys %ppp;
    pdestroy($ppp{$k}), delete $ppp{$k} if $ppp{$k};
    $ppp{$k} = pager(@_) or die "pager failed";
    # FIXME Why it this?
    def($k => sub { $ppp{$k} });
    $ppp{$k} } };#}}}
def clock => sub (@) {#{{{
  ppp(_clock =>
    -width => 21,
    -y => -1,
    -x => -21,
    @_);
  my $t = timer(sub { ppush(_clock(), ts()) }, 1);
  def '_clock_t' => sub { $t }; # FIXME So ugggly...
  pshow(_clock()) };#}}}
#}}}

def pty => sub {#{{{
    open my $f, '+>&=', shift || term->env->{'URXVT_PTY'}
        || die "Where I am?";
    $f
};
#}}}
def setnonblock => sub {#{{{
    require Fcntl;
    my ($f, $flags) = (shift || die, '');
    fcntl STDIN, F_GETFL, $flags
        or die "fcntl getfl: $!";
    $flags |= O_NONBLOCK;
    fcntl STDIN, F_SETFL, $flags
        or die "fcntl setfl: $!";
    1
};#}}}
# Проблема: не работают esc-последовательности#{{{
# (в терминале ^[[31mxe)
def ptyt => sub {
    my $p = shift || term->env->{'URXVT_PTY'}
        || die "Where I am?";
    close(STDOUT);
    close(STDERR);
    close(STDIN);
    open(STDOUT, '>&=', $p)
    and open(STDERR, '>&=', $p)
    and open(STDIN, '<&=', $p)
    or die "Can`t: ", dec($!);
    $|=1
};
#}}}
# Проблема: не работает вообще...#{{{
def iowt => sub {
    urxvt::iow->new
        ->fd(fileno STDIN)
        ->events(urxvt::EV_READ)
        ->cb( sub {
                sysread STDIN, my $b, 512;
                push @{bag('d')}, "! $b"
                })
        ->start
    };
#}}}

sub main () {#{{{
} #}}}

1; # Yes, it's just a PERL module.
