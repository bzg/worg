#!/usr/bin/perl
# org2remind.pl: perl utility to convert org-mode appointments to remind

# Copyright (C) 2011 Matt Lundin <mdl at imapmail.org>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.

# This program is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#  General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>
#######################################################################

# Remind is a command line calendar application for Unix/Linux that
# can, among other things, spit out formatted plain text calendars and
# agendas.
# http://www.roaringpenguin.com/products/remind
#
# To convert org-mode appointments to remind data, simply call the
# following script on one or more org files:
# perl org2remind.pl ~/org/*.org
#
# The results of this script can saved in a file...
# perl org2remind.pl ~/org/*.org >> org.rem
# ...or piped directly into a remind command...
# perl org2remind.pl ~/org/*.org | remind -
#
# For a nice calendar of the current month, use the following:
# perl org2remind.pl ~/org/*.org | remind -c -
#
# This is similar to org2rem.el (in the contrib directory of the
# org-mode repository), except that, instead of saving files from
# within org-mode, it allows for easier and faster access to
# org/remind data from the command line.
#
# Currently, the script supports appointments (i.e., active
# timestamps) and the org-diary-class sexp.
#
# TODO add option to include SCHEDULED and DEADLINE items.
# TODO parse simple diary sexps
# TODO accomodate spans <ts1>--<ts2>

use strict;

$/ = "\n*";

# TODO check CPAN for localization tools
my @daysofweek = qw/ Mon Tue Wed Thu Fri Sat Sun /;
my @months = qw/ Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec /;

sub subtract_time {
  my @time1 = split(/:/, shift(@_));
  my $minutes1 = $time1[0] * 60 + $time1[1];
  my @time2 = split(/:/, shift(@_));
  my $minutes2 = $time2[0] * 60 + $time2[1];
  return $minutes2 - $minutes1;
}

sub trim_headline {
  my $headline = shift @_;
  # get rid of opening asterisks
  $headline =~ s/^\** //;
  # get rid of priorities - remind chokes on them
  $headline =~ s/\[#[A-Z]\]//;
  # nuke link brackets - remind chokes on them
  $headline =~ s/\]\[/ - /;
  $headline =~ s/\[\[|\]\]//;
  # get rid of tags
  $headline =~ s/:[a-zA-Z0-9:]+://;
  # trim trailing whitespace
  $headline =~ s/\s+$//;
  return $headline;
}

while (<>) {
  my @lines = split(/\n/, $_);
  my $headline = shift @lines;
  my $headtrim = 0;
  for (@lines) {
    if ( /<([0-9]{4}-[0-9]{2}-[0-9]{2}) # YYYY-MM-DD
	  (?:\s+[A-Za-z]{3})		# day of week (English)
	  (?:\s+([0-9]{2}:[0-9]{2}))?	# 1st time of day
	  (?:-([0-9]{2}:[0-9]{2}))?	# second time of day
	  (?:\s+(\+[0-9]+[dmwy]))?>	# repeater
	 /x ) {
      next if ( /(SCHEDULED|DEADLINE):/ );
      my ($timestamp, $hour1, $hour2, $repeater) = ($1, $2, $3, $4);
      if ($headtrim == 0) {
	$headline = trim_headline($headline);
	# only need to trim headline once
    	$headtrim = 1;
      }
      # recurring events
      # TODO Figure out way to support multiples of month and year
      # Right now, only +1m and +1y work. This is due to a limitation
      # in my knowledge of remind syntax.
      my $trigdate;
      if (defined $repeater) {
      	$repeater =~ /([0-9]+)([dmwy])/;
      	my ($interval, $type) = ($1, $2);
      	if ($type eq "y") {
      	  my @ymd = split(/-/, $timestamp);
	  $trigdate = "[trigdate() >= '$timestamp']";
      	  $timestamp = "$months[$ymd[1] - 1] $ymd[2]";
      	} elsif ($type eq "w") {
      	  $timestamp = sprintf("%s *%d", $timestamp, ($interval * 7));
      	} elsif ($type eq "m") {
      	  my @ymd = split(/-/, $timestamp);
	  $trigdate = "[trigdate() >= '$timestamp']";
      	  $timestamp = $ymd[2];
      	} elsif ($type eq "d") {
      	  $timestamp = sprintf("%s *%d", $timestamp, $interval);
	}
      }
      # calculate duration
      $timestamp = join(" ", $timestamp,"AT",$hour1) if defined($hour1);
      if (defined $hour2) {
	my $duration = subtract_time $hour1, $hour2;
	$timestamp = sprintf("%s +%d", $timestamp, $duration);
      }
      $timestamp = sprintf("%s %s %s", $timestamp, "SATISFY", $trigdate)
	if defined($trigdate);
      printf "REM %s MSG %s\n", $timestamp, $headline;
    }
    # org-diary-class
    if (/%%\(org-diary-class\s+
	 ([0-9]{1,2}\s+[0-9]{1,2}\s+[0-9]{4})\s+ # start mdy
	 ([0-9]{1,2}\s+[0-9]{1,2}\s+[0-9]{4})\s+ # end mdy
	 ([0-9])\s*				 # dow
	 ([0-9])?				 # iso exception - not yet supported
	 \)\s*(.*)$
	/x) {
      my ($start, $end, $dow, $iso, $event) = ($1, $2, $3, $4, $5);
      # differentiate between "<%%.*>" and "%%.* event"
      # the former uses the headline; the latter the string after sexp
      $headline = $event unless (/<%%.*>/);
      $dow = $daysofweek[$dow - 1];
      my ($m1, $d1, $y1) = split(/\s+/, $start);
      my ($m2, $d2, $y2) = split(/\s+/, $end);
      if ($headtrim == 0) {
	$headline = trim_headline($headline);
    	$headtrim = 1;
      }
      printf
	"REM %s UNTIL %d-%02d-%02d SATISFY [trigdate() >= '%d-%02d-%02d'] MSG %s\n",
	  $dow, $y2, $m2, $d2, $y1, $m1, $d1, $headline;
    }
    # TODO add support for basic diary sexps
  }
}
