unit module MoarVM::Remote::CLI::Breakpoints;

use App::MoarVM::Debug::Formatter;

my %files;

sub output-breakpoint-notifications(Str $file, Int $line, Supply $notifications, %breakpoints) is export {
    start {
        react whenever $notifications.Supply -> $ev {
            list-file $file, $line, %breakpoints;
            with $ev<frames> -> $frames {
                my @this-backtrace = format-backtrace($frames);

                print-table my @chunks =
                    "Breakpoint on $file:$line hit by thread &bold($ev.<thread>)!"
                        => @this-backtrace;;
            } else {
                say "Breakpoint on $file:$line hit by thread &bold($ev.<thread>)!";
            }
        }
        CATCH {
            say "Breakpoint handler aborted";
            .say;
        }
    };
}

sub get-breakpoint(%breakpoints, Str $file, Int $line) is export {
    %breakpoints.pairs.first({ .value<file> eq $file and .value<line> eq $line })
}

sub list-file(Str $file, Int $line, %breakpoints --> Nil) is export {
    %files{$file} = $file.IO.open.lines.list unless %files{$file}:exists;
    my @lines := %files{$file};
    my $index = max $line - 6, 0;
    my $end-index = min $line + 4, @lines.elems;
    until $index > $end-index {
        say get-breakpoint(%breakpoints, $file, $index + 1) ?? 'BR ' !! '   ',
            $index + 1 eq $line ?? '-->' !! '   ',
            "{$index + 1} \t @lines[$index]";
        $index++;
    }
    
}

# vim: expandtab shiftwidth=4
