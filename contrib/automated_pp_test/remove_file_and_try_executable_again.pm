#!/usr/bin/perl -w
########################################################################
# Copyright 2004 by Malcolm Nooning
# This program does not impose any
# licensing restrictions on files generated by their execution, in
# accordance with the 8th article of the Artistic License:
#
#    "Aggregation of this Package with a commercial distribution is
#    always permitted provided that the use of this Package is embedded;
#    that is, when no overt attempt is made to make this Package's
#    interfaces visible to the end user of the commercial distribution.
#    Such use shall not be construed as a distribution of this Package."
#
# Therefore, you are absolutely free to place any license on the resulting
# executable(s), as long as the packed 3rd-party libraries are also available
# under the Artistic License.
#
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#
# See L<http://www.perl.com/perl/misc/Artistic.html>
#
#
#
########################################################################
# Usage:
# $error = remove_file_and_try_executable_again
#                                  (
#                                    $file_to_remove,
#                                    $test_number,
#                                    $sub_test_number,
#                                    $test_name_string,
#                                    $test_dir,
#                                    $pipe_command_string,
#                                    $executable_name",
#                                    $expected_results,
#                                    $os,
#                                    $verbose,
#                                    \$message,
#                                    $print_cannot_locate_message,
#                                  );
#
########################################################################

########################################################################
our $VERSION = '0.07';

package remove_file_and_try_executable_again;

use Exporter;
@ISA = qw(Exporter);
@EXPORT = ("remove_file_and_try_executable_again");

use POSIX qw(EXIT_SUCCESS EXIT_FAILURE);
use Cwd qw(chdir);

use pipe_a_command;

use strict;

#########################################################################
sub remove_file {
  my ($file, $message_ref, $verbose) = @_;

  if (-e($file)) {
    if (!(unlink($file))) {
      # Try a desparation chmod
      chmod(0775, $file);
      if (!(unlink($file))) {
        $$message_ref = $$message_ref .
           "\[620\]Cannot delete file $file \n";
        return(EXIT_FAILURE);
      }
    }
    if ($verbose) {
      print ("\[625\]Removed file $file\n");
    }
  } else {
    if ($verbose) {
      print ("You wanted me to remove file $file\n");
      print ("but it does not exist.  Skipping \.\.\. \n");
    }
  }
  return (EXIT_SUCCESS);
}

#########################################################################
sub remove_file_and_try_executable_again {
  my (
       $file_to_remove,
       $test_number,
       $sub_test,
       $test_name_string,
       $test_dir,
       $command_string,
       $executable_name,
       $expected_result,
       $os,
       $verbose,
       $message_ref,
       $print_cannot_locate_message,
     ) = @_;

  my $results = "";
  my $error = EXIT_FAILURE;

  $error = remove_file($file_to_remove, $message_ref, $verbose);
  if ($error == EXIT_FAILURE) {
    $$message_ref = "Test ${test_number}_${sub_test} : " . $$message_ref;
     return ($error);
  }

  #.................................................................
  $error = pipe_a_command(
                           $test_number,
                           $sub_test,
                           $test_name_string,
                           $test_dir,
                           $command_string,
                           $executable_name,
                           $expected_result,
                           $os,
                           $verbose,
                           $message_ref,
                           $print_cannot_locate_message,
                        );
  return ($error);

}

#########################################################################
1;
