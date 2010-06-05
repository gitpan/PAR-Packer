#line 1
package IPC::Run3;

#line 11

$VERSION = '0.042';

#line 32

use 5.006_000;				# i.e. v5.6.0

@EXPORT = qw( run3 );
%EXPORT_TAGS = ( all => \@EXPORT );
@ISA = qw( Exporter );
use Exporter;

use strict;
use constant debugging => $ENV{IPCRUN3DEBUG} || $ENV{IPCRUNDEBUG} || 0;
use constant profiling => $ENV{IPCRUN3PROFILE} || $ENV{IPCRUNPROFILE} || 0;
use constant is_win32  => 0 <= index $^O, "Win32";

BEGIN {
   if ( is_win32 ) {
      eval "use Win32 qw( GetOSName ); 1" or die $@;
   }
}

#use constant is_win2k => is_win32 && GetOSName() =~ /Win2000/i;
#use constant is_winXP => is_win32 && GetOSName() =~ /WinXP/i;

use Carp qw( croak );
use File::Temp qw( tempfile );
use POSIX qw( dup dup2 );

# We cache the handles of our temp files in order to
# keep from having to incur the (largish) overhead of File::Temp
my %fh_cache;
my $fh_cache_pid = $$;

my $profiler;

sub _profiler { $profiler } # test suite access

BEGIN {
    if ( profiling ) {
        eval "use Time::HiRes qw( gettimeofday ); 1" or die $@;
        if ( $ENV{IPCRUN3PROFILE} =~ /\A\d+\z/ ) {
            require IPC::Run3::ProfPP;
            IPC::Run3::ProfPP->import;
            $profiler = IPC::Run3::ProfPP->new(Level => $ENV{IPCRUN3PROFILE});
        } else {
            my ( $dest, undef, $class ) =
               reverse split /(=)/, $ENV{IPCRUN3PROFILE}, 2;
            $class = "IPC::Run3::ProfLogger"
                unless defined $class && length $class;
            if ( not eval "require $class" ) {
                my $e = $@;
                $class = "IPC::Run3::$class";
                eval "require IPC::Run3::$class" or die $e;
            }
            $profiler = $class->new( Destination => $dest );
        }
        $profiler->app_call( [ $0, @ARGV ], scalar gettimeofday() );
    }
}


END {
    $profiler->app_exit( scalar gettimeofday() ) if profiling;
}

sub _binmode {
    my ( $fh, $mode, $what ) = @_;
    # if $mode is not given, then default to ":raw", except on Windows,
    # where we default to ":crlf";
    # otherwise if a proper layer string was given, use that,
    # else use ":raw"
    my $layer = !$mode
	? (is_win32 ? ":crlf" : ":raw")
	: ($mode =~ /^:/ ? $mode : ":raw");
    warn "binmode $what, $layer\n" if debugging >= 2;

    binmode $fh, ":raw" unless $layer eq ":raw";      # remove all layers first
    binmode $fh, $layer or croak "binmode $layer failed: $!";
}

sub _spool_data_to_child {
    my ( $type, $source, $binmode_it ) = @_;

    # If undef (not \undef) passed, they want the child to inherit
    # the parent's STDIN.
    return undef unless defined $source;

    my $fh;
    if ( ! $type ) {
        open $fh, "<", $source or croak "$!: $source";
	_binmode($fh, $binmode_it, "STDIN");
        warn "run3(): feeding file '$source' to child STDIN\n"
            if debugging >= 2;
    } elsif ( $type eq "FH" ) {
        $fh = $source;
        warn "run3(): feeding filehandle '$source' to child STDIN\n"
            if debugging >= 2;
    } else {
        $fh = $fh_cache{in} ||= tempfile;
        truncate $fh, 0;
        seek $fh, 0, 0;
	_binmode($fh, $binmode_it, "STDIN");
        my $seekit;
        if ( $type eq "SCALAR" ) {

            # When the run3()'s caller asks to feed an empty file
            # to the child's stdin, we want to pass a live file
            # descriptor to an empty file (like /dev/null) so that
            # they don't get surprised by invalid fd errors and get
            # normal EOF behaviors.
            return $fh unless defined $$source;  # \undef passed

            warn "run3(): feeding SCALAR to child STDIN",
                debugging >= 3
                   ? ( ": '", $$source, "' (", length $$source, " chars)" )
                   : (),
                "\n"
                if debugging >= 2;

            $seekit = length $$source;
            print $fh $$source or die "$! writing to temp file";

        } elsif ( $type eq "ARRAY" ) {
            warn "run3(): feeding ARRAY to child STDIN",
                debugging >= 3 ? ( ": '", @$source, "'" ) : (),
                "\n"
            if debugging >= 2;

            print $fh @$source or die "$! writing to temp file";
            $seekit = grep length, @$source;
        } elsif ( $type eq "CODE" ) {
            warn "run3(): feeding output of CODE ref '$source' to child STDIN\n"
                if debugging >= 2;
            my $parms = [];  # TODO: get these from $options
            while (1) {
                my $data = $source->( @$parms );
                last unless defined $data;
                print $fh $data or die "$! writing to temp file";
                $seekit = length $data;
            }
        }

        seek $fh, 0, 0 or croak "$! seeking on temp file for child's stdin"
            if $seekit;
    }

    croak "run3() can't redirect $type to child stdin"
        unless defined $fh;

    return $fh;
}

sub _fh_for_child_output {
    my ( $what, $type, $dest, $options ) = @_;

    my $fh;
    if ( $type eq "SCALAR" && $dest == \undef ) {
        warn "run3(): redirecting child $what to oblivion\n"
            if debugging >= 2;

        $fh = $fh_cache{nul} ||= do {
            open $fh, ">", File::Spec->devnull;
	    $fh;
        };
    } elsif ( $type eq "FH" ) {
        $fh = $dest;
        warn "run3(): redirecting $what to filehandle '$dest'\n"
            if debugging >= 3;
    } elsif ( !$type ) {
        warn "run3(): feeding child $what to file '$dest'\n"
            if debugging >= 2;

        open $fh, $options->{"append_$what"} ? ">>" : ">", $dest 
	    or croak "$!: $dest";
    } else {
        warn "run3(): capturing child $what\n"
            if debugging >= 2;

        $fh = $fh_cache{$what} ||= tempfile;
        seek $fh, 0, 0;
        truncate $fh, 0;
    }

    my $binmode_it = $options->{"binmode_$what"};
    _binmode($fh, $binmode_it, uc $what);

    return $fh;
}

sub _read_child_output_fh {
    my ( $what, $type, $dest, $fh, $options ) = @_;

    return if $type eq "SCALAR" && $dest == \undef;

    seek $fh, 0, 0 or croak "$! seeking on temp file for child $what";

    if ( $type eq "SCALAR" ) {
        warn "run3(): reading child $what to SCALAR\n"
            if debugging >= 3;

        # two read()s are used instead of 1 so that the first will be
        # logged even it reads 0 bytes; the second won't.
        my $count = read $fh, $$dest, 10_000, 
	    $options->{"append_$what"} ? length $$dest : 0;
        while (1) {
            croak "$! reading child $what from temp file"
                unless defined $count;

            last unless $count;

            warn "run3(): read $count bytes from child $what",
                debugging >= 3 ? ( ": '", substr( $$dest, -$count ), "'" ) : (),
                "\n"
                if debugging >= 2;

            $count = read $fh, $$dest, 10_000, length $$dest;
        }
    } elsif ( $type eq "ARRAY" ) {
	if ($options->{"append_$what"}) {
	    push @$dest, <$fh>;
	} else {
	    @$dest = <$fh>;
	}
        if ( debugging >= 2 ) {
            my $count = 0;
            $count += length for @$dest;
            warn
                "run3(): read ",
                scalar @$dest,
                " records, $count bytes from child $what",
                debugging >= 3 ? ( ": '", @$dest, "'" ) : (),
                "\n";
        }
    } elsif ( $type eq "CODE" ) {
        warn "run3(): capturing child $what to CODE ref\n"
            if debugging >= 3;

        local $_;
        while ( <$fh> ) {
            warn
                "run3(): read ",
                length,
                " bytes from child $what",
                debugging >= 3 ? ( ": '", $_, "'" ) : (),
                "\n"
                if debugging >= 2;

            $dest->( $_ );
        }
    } else {
        croak "run3() can't redirect child $what to a $type";
    }

}

sub _type {
    my ( $redir ) = @_;
    return "FH" if eval { $redir->isa("IO::Handle") };
    my $type = ref $redir;
    return $type eq "GLOB" ? "FH" : $type;
}

sub _max_fd {
    my $fd = dup(0);
    POSIX::close $fd;
    return $fd;
}

my $run_call_time;
my $sys_call_time;
my $sys_exit_time;

sub run3 {
    $run_call_time = gettimeofday() if profiling;

    my $options = @_ && ref $_[-1] eq "HASH" ? pop : {};

    my ( $cmd, $stdin, $stdout, $stderr ) = @_;

    print STDERR "run3(): running ", 
       join( " ", map "'$_'", ref $cmd ? @$cmd : $cmd ), 
       "\n"
       if debugging;

    if ( ref $cmd ) {
        croak "run3(): empty command"     unless @$cmd;
        croak "run3(): undefined command" unless defined $cmd->[0];
        croak "run3(): command name ('')" unless length  $cmd->[0];
    } else {
        croak "run3(): missing command" unless @_;
        croak "run3(): undefined command" unless defined $cmd;
        croak "run3(): command ('')" unless length  $cmd;
    }

    foreach (qw/binmode_stdin binmode_stdout binmode_stderr/) {
	if (my $mode = $options->{$_}) {
	    croak qq[option $_ must be a number or a proper layer string: "$mode"]
		unless $mode =~ /^(:|\d+$)/;
	}
    }

    my $in_type  = _type $stdin;
    my $out_type = _type $stdout;
    my $err_type = _type $stderr;

    if ($fh_cache_pid != $$) {
	# fork detected, close all cached filehandles and clear the cache
	close $_ foreach values %fh_cache;
	%fh_cache = ();
	$fh_cache_pid = $$;
    }
    
    # This routine procedes in stages so that a failure in an early
    # stage prevents later stages from running, and thus from needing
    # cleanup.

    my $in_fh  = _spool_data_to_child $in_type, $stdin,
        $options->{binmode_stdin} if defined $stdin;

    my $out_fh = _fh_for_child_output "stdout", $out_type, $stdout,
        $options if defined $stdout;

    my $tie_err_to_out =
        defined $stderr && defined $stdout && $stderr eq $stdout;

    my $err_fh = $tie_err_to_out
        ? $out_fh
        : _fh_for_child_output "stderr", $err_type, $stderr,
            $options if defined $stderr;

    # this should make perl close these on exceptions
#    local *STDIN_SAVE;
    local *STDOUT_SAVE;
    local *STDERR_SAVE;

    my $saved_fd0 = dup( 0 ) if defined $in_fh;

#    open STDIN_SAVE,  "<&STDIN"#  or croak "run3(): $! saving STDIN"
#        if defined $in_fh;
    open STDOUT_SAVE, ">&STDOUT" or croak "run3(): $! saving STDOUT"
        if defined $out_fh;
    open STDERR_SAVE, ">&STDERR" or croak "run3(): $! saving STDERR"
        if defined $err_fh;

    my $errno;
    my $ok = eval {
        # The open() call here seems to not force fd 0 in some cases;
        # I ran in to trouble when using this in VCP, not sure why.
        # the dup2() seems to work.
        dup2( fileno $in_fh, 0 )
#        open STDIN,  "<&=" . fileno $in_fh
            or croak "run3(): $! redirecting STDIN"
            if defined $in_fh;

#        close $in_fh or croak "$! closing STDIN temp file"
#            if ref $stdin;

        open STDOUT, ">&" . fileno $out_fh
            or croak "run3(): $! redirecting STDOUT"
            if defined $out_fh;

        open STDERR, ">&" . fileno $err_fh
            or croak "run3(): $! redirecting STDERR"
            if defined $err_fh;

        $sys_call_time = gettimeofday() if profiling;

        my $r = ref $cmd
              ? system { $cmd->[0] }
                       is_win32
                           ? map {
                                 # Probably need to offer a win32 escaping
                                 # option, every command may be different.
                                 ( my $s = $_ ) =~ s/"/"""/g;
                                 $s = qq{"$s"};
                                 $s;
                             } @$cmd
                           : @$cmd
              : system $cmd;

	$errno = $!;		# save $!, because later failures will overwrite it
        $sys_exit_time = gettimeofday() if profiling;
        if ( debugging ) {
            my $err_fh = defined $err_fh ? \*STDERR_SAVE : \*STDERR;
	    if ( defined $r && $r != -1 ) {
		print $err_fh "run3(): \$? is $?\n";
	    } else {
		print $err_fh "run3(): \$? is $?, \$! is $errno\n";
	    }
        }

        die $! if defined $r && $r == -1 && !$options->{return_if_system_error};

        1;
    };
    my $x = $@;

    my @errs;

    if ( defined $saved_fd0 ) {
        dup2( $saved_fd0, 0 );
        POSIX::close( $saved_fd0 );
    }

#    open STDIN,  "<&STDIN_SAVE"#  or push @errs, "run3(): $! restoring STDIN"
#        if defined $in_fh;
    open STDOUT, ">&STDOUT_SAVE" or push @errs, "run3(): $! restoring STDOUT"
        if defined $out_fh;
    open STDERR, ">&STDERR_SAVE" or push @errs, "run3(): $! restoring STDERR"
        if defined $err_fh;

    croak join ", ", @errs if @errs;

    die $x unless $ok;

    _read_child_output_fh "stdout", $out_type, $stdout, $out_fh, $options
        if defined $out_fh && $out_type && $out_type ne "FH";
    _read_child_output_fh "stderr", $err_type, $stderr, $err_fh, $options
        if defined $err_fh && $err_type && $err_type ne "FH" && !$tie_err_to_out;
    $profiler->run_exit(
       $cmd,
       $run_call_time,
       $sys_call_time,
       $sys_exit_time,
       scalar gettimeofday() 
    ) if profiling;

    $! = $errno;		# restore $! from system()

    return 1;
}

1;

__END__

#line 851
