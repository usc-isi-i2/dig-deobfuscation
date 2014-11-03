use strict;
use warnings;
use Getopt::Long;


my $usage = "Usage: hypFile refFile [-alignments alignmentsFile] [-editcounts editcountsFile] [-casePenalty float] [-monocase]  [-discardPunc] [-nosplitpunc] [-usage] [-help]\n";

my $hypFile;
my $refFile;

my $casePenalty = 1.0;
my $monocase = 0;
my $noSplitPunc = 0;
my $alignmentsFile;
my $discardPunc  = 0;
my $usageRequested = 0;
my $helpRequested = 0;
my $contentOnly = 0;
my $editCountsFile;
my $charBased = 0;

my @refStrings;
my @hypStrings;

my $insertions = 0;
my $deletions = 0;
my $flatSubstitutions = 0;
my $caseMismatches = 0;
my $totalRefTokens = 0;
my $totalHypTokens = 0;





my %editCounts;

################################################################################################



GetOptions("casePenalty:f" => \$casePenalty,
	   "nosplitpunc" => \$noSplitPunc,
	   "discardpunc" => \$discardPunc,
	   "alignments:s" => \$alignmentsFile,
	   "usage" => \$usageRequested,
	   "help" => \$helpRequested,
	   "monocase" => \$monocase,
	   "content" => \$contentOnly,
	   "charbased" => \$charBased,
	   "editcounts:s" => \$editCountsFile);


printHelpAndExit() if $helpRequested;

printUsageAndExit() if $usageRequested;

$hypFile = shift;
$refFile = shift;

# Must have at least a ref file
die $usage unless defined($refFile);

# Die for excess arguments
die sprintf("\nExcess args(s): %s\n\n$usage",join(" ",@ARGV)) if @ARGV;

# Protect user from overrwriting his input files
die "Trying to overwrite ref or hyp file: $alignmentsFile" if defined($alignmentsFile) and ($alignmentsFile eq $refFile or $alignmentsFile eq $hypFile);

die "The -editcounts argument $editCountsFile overrwrites one of ref, hyp, or alignments files"
  if (defined($editCountsFile) and ($editCountsFile eq $hypFile or $editCountsFile eq $refFile or (defined($alignmentsFile) and $editCountsFile eq $alignmentsFile)));

# Get the hyp and ref strings
@hypStrings = readLines($hypFile);
@refStrings = readLines($refFile);

# Die if there are differing numbers of them
die sprintf("Mismatched numbers of hyp and ref strings: %d vs. %d",scalar(@hypStrings),scalar(@refStrings)) unless scalar(@hypStrings) == scalar(@refStrings);

# The -content flag is equivalent to -monocase -discardpunc
if ($contentOnly) {
  $monocase = 1;
  $discardPunc = 1;
}

################################################################################################

if (defined($alignmentsFile)) {
  open(ALIGN,">$alignmentsFile") or die "Can't write $alignmentsFile";
  binmode(ALIGN,"utf8");
}

for (my $i=0;$i<scalar(@hypStrings);$i++) {
  updateEditDistance($refStrings[$i],$hypStrings[$i]);
}

close(ALIGN) if defined($alignmentsFile);


printResults();

writeEditCountsFile($editCountsFile) if defined($editCountsFile);


################################################################################################

sub printUsageAndExit {
  die "\n$usage";
}

sub printHelpAndExit {
  print "$usage";
  printf "\nArguments:\n\n";
  my $fmt = " %-15s";
  printf "$fmt Input file of hypothesis sentences to be scored, one sentence per line.\n","hypFile";
  printf "$fmt Input file of reference (ground truth) sentences to be scored against, one sentence per line.\n","refFile";
  printf "$fmt Output file of alignments between refFile and hypFile sentences.\n","-alignments";
  printf "$fmt Output file of counts for edit operations, e.g. 'DELETE word'\n","editcounts";
  printf "$fmt Score penalty for case mismatches: real number from 0 (no penalty) to 1 (full penalty). Default is 1.\n","-casePenalty";
  printf "$fmt Downcase ref and hyp before alignment and scoring. Same scores as '-casePenalty 0', but case mismatches are not flagged in alignments.\n","-monocase";
  printf "$fmt Discard punctuation tokens from ref and hyp before alignment and scoring.\n","-discardpunc";
  printf "$fmt Match on alphanumeric content tokens only. Short for '-monocase -discardpunc'.\n","-content";
  printf "$fmt Do not split punctuation from alphanumeric tokens (not recommended).\n","-nosplitpunc";
  printf "$fmt Do alignment based on characters rather than words. Compute-intensive.\n","-charbased";
  printf "$fmt Print usage and exit.\n","-usage";
  printf "$fmt Print this help message and exit.\n","-help";
  die("\n");
}

sub get {
  my ($matrix,$i,$j) = @_;
  die "Out of range for rows: $i" if $i >= scalar(@$matrix);
  my $row = $matrix->[$i];
  die "Out of range for columns: $j" if $j >= scalar(@$row);
  return $row->[$j];
}

sub set {
  my ($matrix,$i,$j,$value) = @_;
  die "Out of range for rows: $i" if $i >= scalar(@$matrix);
  my $row = $matrix->[$i];
  die "Out of range for columns: $j" if $j >= scalar(@$row);
  $row->[$j] = $value;
}

sub printResults {
  my $substitutions = $flatSubstitutions + $caseMismatches*$casePenalty;
  my $errors        = $substitutions + $insertions + $deletions;
  printf "Total strings:      %d\n",scalar(@refStrings);
  printf "Total ref tokens:   %d\n",$totalRefTokens;
  printf "Total hyp tokens:   %d\n",$totalHypTokens;
  printf "\n";
  printf "Total insertions:   %d\n",$insertions;
  printf "Total deletions:    %d\n",$deletions;
  printf "Total wrong words   %d\n",$flatSubstitutions;
  printf "Total case errors:  %d\n",$caseMismatches;
  printf "\n";
  printf "Total all errors:   %d\n\n",$errors;
  
  printf "Insertion rate:   %4.1f\n",100*($insertions/$totalRefTokens);
  printf "Deletion rate:    %4.1f\n",100*($deletions/$totalRefTokens);
  printf "Subst rate:       %4.1f\n",100*($substitutions/$totalRefTokens);
  printf "Error rate:       %4.1f\n",100*($errors/$totalRefTokens);
}

sub updateEditDistance {
  my ($ref,$hyp) = @_;
  my @ref = tokenize($ref);
  my @hyp = tokenize($hyp);
  my $nref = @ref;
  my $nhyp = @hyp;
  $totalRefTokens += $nref;
  $totalHypTokens += $nhyp;
  my $matrix = makeMatrix($nref+1,$nhyp+1);
  for (my $i=0;$i<$nref+1;$i++) {
    set($matrix,$i,0,$i);
  }
  for (my $j=0;$j<$nhyp+1;$j++) {
    set($matrix,0,$j,$j);
  }
  for (my $i=1;$i<$nref+1;$i++) {
    for (my $j=1;$j<$nhyp+1;$j++) {
      my $r   = $ref[$i-1];
      my $h   = $hyp[$j-1];
      my $del = get($matrix,$i-1,$j)+1;
      my $ins = get($matrix,$i,$j-1)+1;
      my $sub = get($matrix,$i-1,$j-1) + alignmentPenalty(compareTokens($r,$h));
      my $min = getMin($del,$ins,$sub);
      set($matrix,$i,$j,$min);
    }
  }
  # $errors += get($matrix,$nref,$nhyp);
  my @trace = getBacktrace($matrix,\@ref,\@hyp);
  writeAlignmentsToFile(@trace) if defined($alignmentsFile);
  foreach my $elt (@trace) {
    $editCounts{$elt}++ unless $elt =~ /^ALIGN/;
    if ($elt =~ /^DEL/) {
      $deletions++;
    }
    elsif ($elt =~ /^INS/) {
      $insertions++;
    }
    elsif ($elt =~ /^SUBST/) {
      $flatSubstitutions++;
    }
    elsif ($elt =~ /^CASE/) {
      $caseMismatches++;
    }
    elsif ($elt =~ /^ALIGN/) { # ignore
    }
    else {
      die "What's this: $elt";
    }
  }
}

sub writeEditCountsFile {
  my ($file) = @_;
  open(FILE,">$file") or die "Can't write $file";
  my @edits = sort{$editCounts{$b} <=> $editCounts{$a}} keys(%editCounts);
  foreach my $edit (@edits) {
    printf FILE "%-15s %d\n",$edit,$editCounts{$edit};
  }
  close(FILE);
}

sub compareTokens {
  my ($tok1,$tok2) = @_;
  if ($tok1 eq $tok2) {
    return "ALIGN";
  }
  elsif (lc($tok1) eq lc($tok2)) {
    return "CASE";
  }
  else {
    return "SUBST";
  }
}

sub repeatString {
  my ($str,$n) = @_;
  my $result = "";
  for (my $i=0;$i<$n;$i++) {
    $result .= $str;
  }
  return $result;
}

sub indicator {
  my ($case) = @_;
  if ($case eq "DEL") {
    return "v";
  }
  elsif ($case eq "INS") {
    return "^";
  }
  elsif ($case eq "SUBST") {
    return "x";
  }
  elsif ($case eq "CASE") {
    return "c";
  }
  elsif ($case eq "ALIGN") {
    return "|";
  }
  else {
    die "Don't know this case: $case";
  }
}

sub scoringPenalty {
  my ($case) = @_;
  if ($case =~ /DEL|INS|SUBST/) {
    return 1;
  }
  elsif ($case eq "CASE") {
    return $casePenalty;
  }
  elsif ($case eq "ALIGN") {
    return 0;
  }
  else {
    die "Don't know this matching condition: $case";
  }
}

sub alignmentPenalty {
  my ($case) = @_;
  if ($case =~ /DEL|INS|SUBST/) {
    return 1;
  }
  elsif ($case eq "CASE") {
    return 0.5;
  }
  elsif ($case eq "ALIGN") {
    return 0;
  }
  else {
    die "Don't know this matching condition: $case";
  }
}

sub getBacktrace {
  my ($matrix,$ref,$hyp) = @_;
  my $nrows = scalar(@$matrix);
  my $ncols = scalar(@{$matrix->[0]});
  my $i = $nrows-1;
  my $j = $ncols-1;
  my @backtrace;
  while (1) {
    my $value = get($matrix,$i,$j);
    # printf "i=$i j=$j\n";
    # die "bad i,j=$i,$j" if $i == 0 or $j== 0;
    my $r = $ref->[$i-1];
    my $h = $hyp->[$j-1];
    my $del = 100;
    my $ins = 100;
    my $sub = 100;
    $del = get($matrix,$i-1,$j)+1 if $i > 0;
    $ins = get($matrix,$i,$j-1)+1 if $j > 0;
    $sub = get($matrix,$i-1,$j-1) + alignmentPenalty(compareTokens($r,$h)) if $i > 0 and $j > 0;
    my $min = getMin($del,$ins,$sub);
    if ($min == $del) {
      $i--;
      push(@backtrace,"DEL $r");
    }
    elsif ($min == $ins) {
      $j--;
      push(@backtrace,"INS $h");
    }
    elsif ($r eq $h) {
      $i--;
      $j--;
      push(@backtrace,"ALIGN $r");
    }
    else {
      $i--;
      $j--;
      my $ind = lc($h) eq lc($r)?"CASE":"SUBST";
      push(@backtrace,"$ind $h $r");
    }    
    last if $i == 0 and $j==0;
  }
  return reverse(@backtrace);
}

sub writeAlignmentsToFile {
  my @alignments = @_;
  my $refLine = "";
  my $indLine = "";
  my $hypLine = "";
  for (my $i=0;$i<scalar(@alignments);$i++) {
    my $alg = $alignments[$i];
    my ($type,$word1,$word2) = split(' ',$alg);
    if ($i > 0) {
      $refLine .= " ";
      $indLine .= " ";
      $hypLine .= " ";
    }
    if ($type eq "DEL") {
      $refLine .= $word1;
      $indLine .= repeatString("v",length($word1));
      $hypLine .= repeatString(" ",length($word1));
    }
    elsif ($type eq "INS") {
      $refLine .= repeatString(" ",length($word1));
      $indLine .= repeatString("^",length($word1));
      $hypLine .= $word1;
    }
    elsif ($type eq "SUBST" or $type eq "CASE") {
      my $ind  = $type eq "CASE"?"c":"x";
      my $len1 = length($word1);
      my $len2 = length($word2);
      my $len = max($len1,$len2);
      $refLine .= $word2;
      $refLine .= repeatString(" ",$len-$len2);
      $indLine .= repeatString($ind,$len);
      $hypLine .= $word1;
      $hypLine .= repeatString(" ",$len-$len1);
    }
    elsif ($type eq "ALIGN") {
      $refLine .= $word1;
      $indLine .= repeatString("|",length($word1));
      $hypLine .= $word1;
    }
    else {
      die "Don't know this alignment type: $type";
    }
  }
  printf ALIGN "%s\n",$refLine;
  printf ALIGN "%s\n",$indLine;
  printf ALIGN "%s\n",$hypLine;
  printf ALIGN "\n";
}

sub max {
  my @values = @_;
  my $max;
  foreach my $value (@values) {
    $max = $value if (!defined($max) or $value > $max);
  }
  return $max;
}

# sub matchPenalty {
#   my ($h,$r) = @_;
#   return 0 if ($r eq $h);
#   return $casePenalty if lc($r) eq lc($h);
#   return 1;
# }

sub getMin {
  my @values = @_;
  my $min;
  for (my $i=0;$i<scalar(@values);$i++) {
    my $value = $values[$i];
    $min = $value if (!defined($min) or (defined($min) and $value < $min));
  }
  return $min;
}

sub makeMatrix {
  my ($I,$J) = @_;
  my @matrix;
  for (my $i=0;$i<$I;$i++) {
    my @row;
    for (my $j=0;$j<$J;$j++) {
      push(@row,0);
    }
    push(@matrix,\@row);
  }
  return \@matrix;
}


sub readLines {
  my ($file) = @_;
  my @lines;
  open(FILE,$file) or die "Can't read $file";
  binmode(FILE,"utf8");
  while (<FILE>) {
    s/^\s+//;
    s/\s+$//; 
    my $line = $_;
    push(@lines,$line);
  }
  return @lines;
}

sub tokenize {
  my ($str) = @_;
  $str = lc($str) if $monocase;
  my @tokens;
  if ($charBased) {
    @tokens = split(//,$str);
  }
  elsif ($noSplitPunc) {
    @tokens = split(' ',$str);
  }
  else {
    @tokens = tokenizeSplittingPunctuation($str);
  }
  @tokens = removePunctuation(@tokens) if $discardPunc;
  return @tokens;
}

sub tokenizeSplittingPunctuation {
  my ($str) = @_;
  my @result;
  foreach my $tok (split(' ',$str)) {
    my $main = $tok;
    my $pre;
    my $post;
    # Leading case: one or more punctuation characters followed by at least one letter or digit
    if ($main =~ /^([^a-zA-Z\d]+)([a-zA-Z\d]+)(.*)/) {
      $pre = $1;
      $main = $2 . $3
    }
    # Trailing case: at least one letter or digit, followed by one or more punctuation characters
    if ($main =~ /(.*)([a-zA-Z\d]+)([^a-zA-Z\d]+)$/) {
      $main = $1 . $2;
      $post = $3;
    }
    push(@result,$pre) if defined($pre);
    push(@result,$main);
    push(@result,$post) if defined($post);
  }
  return @result;
}

sub removePunctuation {
  my @tokens = @_;
  my @result;
  foreach my $tok (@tokens) {
    push(@result,$tok) unless $tok =~ /^[^a-zA-Z\d\s]+$/;
  }
  return @result;
}

