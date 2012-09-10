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

## Net#{{{
def macgen=>sub ($) { join ":"#{{{
                    , grep length == 2
                    , split /(.{2})/
                    , md5sum(shift)
                    , 6 };#}}}
def macrnd => sub () { join(":", "00", #{{{
                        ("22", "01", "04") [rand(3)], # SFWM
                        map {sprintf "%2.2X", rand(255)} (1..4)) }; #}}}
def wthset => sub () { " ip link set dev wth"#{{{
                      ." arp off multicast off dynamic off"
                      ." address ".macrnd() };#}}}
def vlanadd => sub () { " ip link add link wth name ".shift#{{{
                       ." type vlan id ".shift };#}}}
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
#}}}

#Veryshotcut#{{{
def tpn => sub { bag(tp=>newtermpty(shift || 'Unit 22')) };
def tp => sub { bag('tp')->[1] };
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
## AnyEvent#{{{
def http_connect => sub ($&;@) {#{{{
  my ($h, $f, $r, $p) = @_;
  $h =~ /([\w\d\.]+):(\d+)$/ and ($h, $p) = ($1, $2);
  require AnyEvent::Socket;
  AnyEvent::Socket::tcp_connect($h, $p || "http",
    sub {
      # See man AnyEvent::Socket
      my ($fh) = @_
        or die "unable to connect: $!";

      my $handle; # avoid direct assignment so on_eof has it in scope.
      require AnyEvent::Handle;
      $handle = new AnyEvent::Handle
      fh     => $fh,
      on_error => sub {
        warn "on_error: $_[2]";
        $_[0]->destroy;
      },
      on_eof => sub {
        $handle->destroy; # destroy handle
        warn "done.";
      };

      $handle->push_write (($r || "GET / HTTP/1.0")
        ."\015\012\015\012");

      $handle->push_read (line => "\015\012\015\012", sub {
          my ($handle, $line) = @_;

          # print response header
          print "HEADER\n$line\n\nBODY\n";

          $handle->on_read (sub {
              # print response body
              ($f or sub { die "undef: @_" })->(
                $_[0]->rbuf, @_);
              $_[0]->rbuf = ""; # FIXME ???
            });
        });
    }
  )};
#}}}
def test_anyevent => sub (\&) {#{{{
  my $f=shift || sub { warn "? ".substr shift, 0, 80 };
  http_connect( "127.0.0.1",
                sub { &$f },
                "GET /debian/dists/stable/Release HTTP/1.0", "3180" )};
#}}}
def http_connect => sub ($&;@) {#{{{
  my ($h, $f, $r, $p) = @_;
  $h =~ /([\w\d\.]+):(\d+)$/ and ($h, $p) = ($1, $2);
  require AnyEvent::Socket;
  AnyEvent::Socket::tcp_connect($h, $p || "http",
    sub {
      # See man AnyEvent::Socket
      my ($fh) = @_
        or die "unable to connect: $!";

      my $handle; # avoid direct assignment so on_eof has it in scope.
      require AnyEvent::Handle;
      $handle = new AnyEvent::Handle
      fh     => $fh,
      on_error => sub {
        warn "on_error: $_[2]";
        $_[0]->destroy;
      },
      on_eof => sub {
        $handle->destroy; # destroy handle
        warn "done.";
      };

      $handle->push_write (($r || "GET / HTTP/1.0")
        ."\015\012\015\012");

      $handle->push_read (line => "\015\012\015\012", sub {
          my ($handle, $line) = @_;

          # print response header
          print "HEADER\n$line\n\nBODY\n";

          $handle->on_read (sub {
              # print response body
              ($f or sub { die "undef: @_" })->(
                $_[0]->rbuf, @_);
              $_[0]->rbuf = ""; # FIXME ???
            });
        });
    }
  )};
#}}}
def ws => sub {#{{{
  # FIXME xexe..
  use lib '/home/den/dev/perl/lib';
  unuse('WS/Server');
  require WS::Server;
  require WS::Driver::AnyEvent;
  require WS::Channel;
  WS::Channel::setup_rpc(WS::Server->new(
      -driver=>'WS::Driver::AnyEvent', @_))
};
#}}}
def wsallevents => sub { map "-on_$_",#{{{
  qw(connect disconnect error open close msg) };
#}}}
def wsflushevents => sub { map $_[0]->Flush($_), wsallevents() };
def wsdbg => sub {#{{{
  # FIXME Extend method -> $self only!
  # TODO This is only one documentation about WS::Server!!
  (shift||ws())->Extend(
    -on_json=>sub { my ($c, $k, %m)=@_;
      warn "[", ts(), "] $k -> {", (join ", ",
        map "$_: ".d($m{$_}), sort keys %m), "}\n" }
    , map { my $k=$_; $k => sub { shift; warn "ws $k: @_\n" } } wsallevents()
  )};
#}}}
def wseval=>sub{shift->say_json(bag('curws'), cmd=>'eval', code=>"@_")};
def m2con =>sub { my ($f, @a) = @_; M2::Api::AnyEvent->connect(#{{{
    -on_open => sub { for (my $m=shift) {
        $m->init('perl', sub { $f->($m, @_, @a) })}},
    -on_close => sub { ref $a[-1] && $a[-1]->(@_) }
  )};
#}}}
#}}}

sub main () {#{{{
} #}}}

1; # Yes, it's just a PERL module.
