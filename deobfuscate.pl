use strict;
use warnings;
use Getopt::Long;


$| = 1;

#
# TODO:  Add words/phrases to be capitalized/uppercased
#

my $usage = "\nUsage: inputFile outputFile [-operations operations-comma-separated] [-capitalize files] [-spellfixes files] [-showOps] [-trace] [-linebreak] [-help] [-usage]\n";

my $SEP = "[^a-zA-Z0-9]*";

my $DIGIT = "\\d|zero|one|o|two|three|four|five|six|seven|eight|nine|ten";

my $NON_ZERO = "[1-9]|one|two|three|four|five|six|seven|eight|nine|ten";



my $capitalFormsFiles = "";
my $spellfixFiles     = "";



my $traceOps = 0;

my $helpRequested;
my $usageRequested;
my $showOps;
my $addLineBreaks;
my $keyMode;

# Took out letter-digit as doing that gained 0.3
my $allOperations = "downcase,html,non-ascii,initial-punct,pre-tokenize,tokenize,non-alphanumeric,ellipsis,repeated-sentence-end,repeated-punct,repeated-chars,spelled-numbers,fix-emails,fix-urls,spelled-words,lookup,capitalize,final-replacements,cosmetic,std-whitespace";

my @allOperations = split(/\,/,$allOperations);

my @contractions = qw(i'm i'll i'd i've you're you'll);

# Default is all operations

my $operations = "all";

GetOptions("operations:s" => \$operations,
	   "showOps" => \$showOps,
	   "capitalize:s" => \$capitalFormsFiles,
	   "spellfixes:s" => \$spellfixFiles,
	   "trace" => \$traceOps,
	   "linebreak" => \$addLineBreaks,
	   "help" => \$helpRequested,
	   "keymode" => \$keyMode,
	   "usage" => \$usageRequested,
	   );

# Quit early if -help or -usage are present in the arugments list.

printHelpAndExit() if $helpRequested;

die $usage if $usageRequested;

my %capitalizedWords;
my %capitalizedRegexes;

my $inputFile  = shift;
my $outputFile = shift;

# die $usage unless $outputFile;

die sprintf("\nExcess arg(s): %s\n$usage\n",join(" ",@ARGV)) if @ARGV;

my @operations = parseOperations($operations);

warn "No operations; will be a no-op" unless @operations;


my %spellFixWords;
my %spellFixRegexes;



# Spell fix list is inserted here:
# ++SPELLFIXES++

# Capitalization list is inserted here:
# ++CAPITALIZE++



# Read capitalization forms files
foreach my $file (split(/\,/,$capitalFormsFiles)) {
  readCapitalizationsFile($file);
}

# Read the spell fix files
foreach my $file (split(/\,/,$spellfixFiles)) {
  readSpellFixFile($file);
}



printOperationsUsed() if $showOps;


####################################################################################



my $inputHandle;
my $outputHandle;


if (defined($inputFile)) {  # If an input file was supplied, open it, otherwise use STDIN.
  open($inputHandle, "<", $inputFile) or die "Can't read $inputFile";
}
else {
  $inputHandle = *STDIN;
}

# If an output file was supplied, open it, otherwise use STDOUT.
if (defined($outputFile)) {
  open($outputHandle,">",$outputFile) or die "can't write $outputFile";
}
else {
  $outputHandle = *STDOUT;
}


my $lineNum = 1;
while (<$inputHandle>) {
  s/^\s+//;
  s/\s+$//;
  next unless $_;  # Ignore blank lines
  my $line = $_;
  if ($keyMode) {
    # Get the key (URI) and content
    my ($uri,$content)  = split('\t',$line,2);
    die "Bad line: $_" unless defined($content);
    $line = unencodeString($content);
    # Output the key and tab separator as the first thing on the line.
    printf $outputHandle "%s\t",$uri;  
  }
  # Output the deobfuscated content.
  printf $outputHandle "%s\n",transform($line);
  # Output an additional line break for readability if desired.  Note this will break some follow-on scripts.
  printf $outputHandle "\n" if $addLineBreaks;
  printf STDERR "%d\n", $lineNum if ($lineNum%1000 == 0);
  $lineNum++;
}
close($inputHandle);
close($outputHandle);

########################################

sub unencodeString {
  my ($str) = @_;
  $str =~ s/\\n/ /g;
  $str =~ s/\\t/ /g;
  $str =~ s/\\r/ /g;
  $str =~ s/\\"/"/g;
  return $str;
}

# Takes a token; returns its class as one of LETTER, DIGIT, SPACE, or PUNCT.
sub tokenClass {
  my ($tok) = @_;
  # We include apostrophe because of contractions
  if ($tok =~ /^[a-zA-Z']+$/) {
    return "LETTER";
  }
  elsif ($tok =~ /\d/) {
    return "DIGIT";
  }
  elsif ($tok =~ /\s/) {
    return "SPACE";
  }
  else {
    return "PUNCT";
  }
}

sub tokenize {
  my ($str) = @_;
  $str =~ s/\`/'/g;
  my @chars = split(//,$str);
  my $curToken = "";
  my @tokens;
  for (my $i=0;$i<scalar(@chars);$i++) {
    my $char = $chars[$i];
    my $class = tokenClass($char);
    if ($i > 0 and $class ne tokenClass($chars[$i-1])) {
      push(@tokens,$curToken);
      $curToken = "";
    }
    $curToken .= $char;
  }
  push(@tokens,$curToken);
  my $assembled = assembleTokens(@tokens);
  $assembled = fixAmounts($assembled);
  return $assembled;
}

sub finalReplacements {
  my ($str) = @_;
  # l3ch3 => leche
  $str =~ s/[1l]\s*3\s*ch\s*3/leche/g;  # Let digit '1' and letter 'l' be the same (they look the same)
  # Standalone 'u' => 'you
  $str =~ s/\bu\b/you/g;              
  $str =~ s/\bn\b/and/g;
  $str =~ s/\bw\b/with/g;
  # Fix the ten => 10 problem introduced by number fixing
  $str =~ s/([a-zA-Z])10/$1ten/g;
  $str =~ s/10([a-zA-Z])/ten$1/g;
  $str =~ s/\b24\s+7\b/24\/7/g;  # 24/7
  $str =~ s/dot/\./g;          # dot => .
  $str =~ s/\bits\b/it's/g;     # its => it's .  Gives a slight gain.
  $str =~ s/\bdon\s*t\b/don't/g;
  $str =~ s/\bi\s+m\b/I'm/g;

  # Fix hypenated words like 'post - X' and 'non - <X>'
  $str =~ s/post\s*\-\s*([a-zA-Z])/post-$1/g;  # "post-op"
  $str =~ s/non\s*\-\s*([a-zA-Z])/non-$1/g;    # "non-rushed"
  $str =~ s/top\s*\-\s*([a-zA-Z])/top-$1/g;    # "top-rated"
  $str =~ s/open\s*\-\s*minded/open-minded/g;  # "open-minded"
  $str =~ s/g - string/g-string/g;             # G-string

  $str =~ s/\s*\/\s*/\//g;

  # Fix measurements
  $str =~ s/(\d\d[a-zA-Z]?) - (\d\d) - (\d\d)/$1-$2-$3/g;

  # Fix dollar amounts
  $str = fixDollarAmounts($str);

  # Fix heights
  # $str =~ s/\b(\d) ' (\d\d?)\b/$1'$2/g;
  # Remove trailing space
  $str =~ s/\s+$//g; 

  # Get rid of trailing comma
  $str =~ s/\,$//g;

  # Add trailing period if sentence is ending in letters - gives a net gain in punctuation WER.
  $str =~ s/([a-zA-Z])$/$1./;
  return $str;
}

sub readSpellFixFile {
  my ($file) = @_;
  # printf STDERR "Reading spellfix file $file\n";
  open(FILE,$file) or die "Can't read $file";
  while (<FILE>) {
    s/^\s+//;
    s/\s+$//;
    next unless $_;
    next if /^\#/;
    spellfix($_);
  }
  close(FILE);
}

sub spellfix {
  my ($spellfix) = @_;
  my ($key,$fix) = split(/\=\>/,$spellfix);
  die "Bad line: $_" unless defined($fix);
  $key = fixWhiteSpace($key);
  $fix = fixWhiteSpace($fix);
  if ($key =~ /\S\s/) {
    $key = join("\\s+",split(' ',$key));
    $key = "\\b$key\\b";
    $spellFixRegexes{$key} = $fix;
  } 
  else {
    $spellFixWords{$key} = $fix;
  }
}

sub fixWhiteSpace {
  my ($str) = @_;
  return join(" ",split(' ',$str));
}

sub preTokenize {
  my ($str) = @_;
  $str =~ s/([a-zA-Z])\$([a-zA-Z])/$1s$2/g;
  return $str;
}

sub fixAmounts {
  my ($str) = @_;
  $str =~ s/\$ (\d\d+)/\$$1/g;
  $str =~ s/100 \%/100\%/g;
  $str =~ s/1 oo \%/100\%/g;  # 100% spelt deviantly as 1 oo
  $str =~ s/(\d+) \$/\$ $1/g;   # Permuted dollar amounts:  '100 $' vs. '$ 100'
  $str =~ s/(\d\d)\s+(b|c|d|dd|e|ee)/$1 . uc($2)/ge;  # Cup size
  $str =~ s/(\d)\s*\'\s*(\d)/$1'$2/g; # Heights: 5'4
  $str =~ s/(\d\d?)\s*\:?\s([ap])\.?m/$1:00 $2m/g;
  return $str;
}

sub fixDollarAmounts {
  my ($str) = @_;
  $str =~ s/\s\s+/ /g;
  # $str =~ s/(?<!(\$ )(\d\d\d?)\s+((quick|hour|per|half))/\$ $1 $2/gi;


  $str =~ s/(?<![\$]) (\d\d\d?)\s+((quick|hour|per|\/|half|incall(s)?|outcall(s)?))/ \$ $1 $2/gi;

  # No space required between the number and the whatever
  # $str =~ s/(?<![\$]) (\d\d\d?)\s*((quick|hour|per|\/|half|incall(s)?|outcall(s)?))/ \$ $1 $2/gi;

  # $str =~ s/(?<![\$])\s(\d\d\d?)\s+quick/ \$ $1 quick/gi;
  # $str =~ s/(?<!\$ )\b(\d\d\d?)\b(?!(pounds|lbs))/\$ $1/g;
  return $str;
}


sub assembleTokens {
  my @tokens = @_;
  my @tmp;
  for (my $i=0;$i<scalar(@tokens);$i++) {
    my $tok = $tokens[$i];
    if ($i > 0 and (tokenClass($tokens[$i-1]) ne "SPACE") and (tokenClass($tok) ne "SPACE")) {
      push(@tmp," ");
    }
    else {
    }
    push(@tmp,$tok);
  }
  my @final;
  foreach my $tok (@tmp) {
    if (scalar(@final) >= 2 and (tokenClass($final[-2]) eq "PUNCT") and (tokenClass($final[-1]) eq "SPACE") and (tokenClass($tok) eq "PUNCT")) {
      pop(@final);
    }
    push(@final,$tok);
  }
  return join("",@final);
}


sub fixSpelledWords {
  my ($str) = @_;
  $str =~ s/\b([a-zA-Z]) ([a-zA-Z])(( [a-z])+)\b/$1 . $2 .  remove($3," ")/ge;
  # $str =~ s/\b([a-zA-Z])_([a-zA-Z])((_[a-zA-z])*)\b/$1 . $2 .  remove($3,"_")/ge;
  $str = fixSpelledWordsWithSeparator($str," _ ");
  $str = fixSpelledWordsWithSeparator($str," - ");
  $str = fixSpelledWordsWithSeparator($str,"  ");
  return $str;
}

sub fixSpelledWordsWithSeparator {
  my ($str,$sepr) = @_;
  $str =~ s/\b([a-zA-Z])$sepr([a-zA-Z])(($sepr[a-z])*)\b/$1 . $2 .  remove($3,$sepr)/ge;
  return $str;
}


sub remove {
  my ($str,$item) = @_;
  $str =~ s/$item//g;
  return $str;
}

sub combine {
  my ($str,$sepr) = @_;
  $str =~ s/\s\s+/ /g;
  my @tokens = split(' ',$str);
  return join($sepr,@tokens);
}


sub printOperationsUsed {
  printf STDERR "\nWill perform these %d operations:\n\n",scalar(@operations);
  foreach my $op (@operations) {
    printf STDERR "$op\n";
  } 
}

sub fixEmails {
  my ($str) = @_;
  $str =~ s/([a-zA-Z]+)\s*\@\s*([a-zA-Z]+)\s*\.\s*com/$1 . "@" . $2 . ".com"/egi;
  return $str;
}

sub parseOperations {
  my ($str) = @_;
  my %allOperations;
  foreach my $op (@allOperations) {
    $allOperations{$op}=1;
  }
  my %negated;
  my @opsInOrder;
  my @tokens = split(/\,/,$str);
  foreach my $tok (@tokens) {
    my $negated = 0;
    if ($tok =~ /^\!(.+)$/) {
      $negated = 1;
      $tok = $1;
    }
    my @ops = ($tok eq "all")?@allOperations:($tok);
    foreach my $op (@ops) {
      die "\nNot an operation: $op\n" unless $allOperations{$op};
      $negated{$op} = 1 if $negated;
      push(@opsInOrder,$op);
    }
  }
  my @operations = ();
  foreach my $op (@opsInOrder) {
    push(@operations,$op) unless $negated{$op};
  }
  return @operations;
}

sub printHelpAndExit {
  printf STDERR "$usage\n";
  printf STDERR "Args:\n\n";
  printf STDERR "inputFile    Input file of sentences to be transformed, one sentence per line\n";
  printf STDERR "outputFile   Output file of transformed sentences, one sentence per line\n";
  printf STDERR "-operations  Comma-separated list of operation names to use. The token 'all' means all operations. Prefixing an operation name with a '!' means don't use it\n";
  printf STDERR "-capitalize  Comma-separated list of files which contain 'capitalization forms' like 'Mary', 'Palm Springs', and 'NYC',etc.\n";
  printf STDERR "-spellfixes  Comma-separated list of files which contain spell fixes like 'gentalmen => gentlemen'.\n";
  printf STDERR "-trace       Prints out the result of each operation as it is applied to each sentence.  Can be voluminous.\n";
  printf STDERR "-showOps     Print the list of operations that will be used\n";
  printf STDERR "-help        Print this message and exit\n";
  printf STDERR "\nAvailable operations:\n\n";
  foreach my $op (@allOperations) {
    printf STDERR "$op\n";
  }
  printf STDERR "all         = All of the above operations, applied in the order they appear above.\n";
  die("\n");
}

sub downcase {
  my ($line) = @_;
  return lc($line);
#   my @tokens = split(' ',$line);
#   for (my $i=0;$i<scalar(@tokens);$i++) {
#     my $tok = $tokens[$i];
#     if ($tok =~ /^[A-Z][a-z]*$/) { # leave alone
#       # die "leaving alone";
#     }
#     else {
#       $tokens[$i] = lc($tok);
#     }
#   }
#   return join(" ",@tokens);
}

sub transform {
  my ($line) = @_;
  printf STDERR "init: %s\n\n",$line if $traceOps;
  foreach my $op (@operations) {
    if ($op eq "downcase") {
      $line = downcase($line);
    }
    elsif ($op eq "cosmetic") {
      $line = makeCosmeticFixes($line);
    }
    elsif ($op eq "html") {
      $line = stripHtml($line);
    }
    elsif ($op eq "non-ascii") {
      $line = stripNonASCII($line);
    }
    elsif ($op eq "std-whitespace") {
      $line = standardizeWhitespace($line);
    }
    elsif ($op eq "non-alphanumeric") {
      $line = stripNonAlphaNumeric($line);
    }
    elsif ($op eq "ellipsis") {
      $line = ellipsis($line);
    }
    elsif ($op eq "repeated-sentence-end") {
      $line = reduceRepeatedPunct($line);
    }
    elsif ($op eq "final-replacements") {
      $line = finalReplacements($line);
    }
    elsif ($op eq "pre-tokenize") {
      $line = preTokenize($line);
    }
    elsif ($op eq "repeated-punct") {
      $line = stripRepeatedPunct($line);
    }
    elsif ($op eq "initial-punct") {
      $line = initialPunct($line);
    }
    elsif ($op eq "tokenize") {
      $line = tokenize($line);
    }
    elsif ($op eq "repeated-chars") {
      $line = removeRepeatedChars($line);
    }
    elsif ($op eq "lookup") {
      $line = applySpellFixes($line);
    }
    elsif ($op eq "spelled-numbers") {
      $line = fixSpelledPhoneNumbers($line);
    }
    elsif ($op eq "fix-emails") {
      $line = fixEmails($line);
    }
        elsif ($op eq "fix-urls") {
      $line = fixUrls($line);
    }
    elsif ($op eq "letter-digit") {
      $line = splitLettersAndDigits($line);
    }
    elsif ($op eq "capitalize") {
      $line = applyCapitalizations($line);
    }
    elsif ($op eq "spelled-words") {
      $line = fixSpelledWords($line);
    }
    else {
      die "Unknown operation: $op";
    }
    printf STDERR "%-22s => %s\n\n",$op,$line if $traceOps;
  }
  return $line;
}

# TODO: Only handles a specific number of path entries right now...
sub fixUrls {
  my ($str) = @_;
  $str =~ s/http\s*\:\s*([a-zA-Z]+)\s*\.\s*([a-zA-Z]+)\s*\.\s*(com|net)/"http:\/\/" . combine("$1 $2",".") . ".$3"/ge;
  # $str =~ s/http\s*\:\s*([a-zA-Z]+(\s*\.\s*[a-zA-Z]+))\s*\.\s*com/"http:\/\/" . combine("$1 $2","") . ".com"/ge;
  return $str;
}

sub makeCosmeticFixes {
  my ($line) = @_;
  # Token followed by certain types of puntuation, or close paren ')'.
  $line =~ s/([\da-zA-Z])\s+([,.?!;:\)])/$1$2/g;
  # Open paren followed by token
  $line =~ s/\(\s+([\da-zA-Z])/\($1/g;
  return $line;
}

sub applySpellFixes {
  my ($line) = @_;
  $line =~ s/xo[xo]*/xoxo/g;
  for my $regex (keys(%spellFixRegexes)) {
    my $fix = $spellFixRegexes{$regex};
    $line =~ s/$regex/$fix/ig;
  }
  # printf STDERR "line: %s\n",$line;
  my @tokens = split(' ',$line);
  for (my $i=0;$i<scalar(@tokens);$i++) {
    my $tok = $tokens[$i];
    my $lookup = $spellFixWords{$tok};
    $tokens[$i] = $lookup if defined($lookup);
  }
  return join(" ",@tokens);
}


sub splitLettersAndDigits {
  my ($str) = @_;
  $str =~ s/(\d)([a-zA-z])/$1 $2/g;
  $str =~ s/([a-zA-z])(\d)/$1 $2/g;
  return $str;
}

sub applyCapitalizations {
  my ($str) = @_;

  # Capitalize pronouns i'm, i'll, and i
  $str =~ s/(\W)i'm(\W)/$1I'm$2/g;
  $str =~ s/(\W)i'll(\W)/$1'll$2/g;
  $str =~ s/(\W)i(\W)/$1I$2/g;

  # Capitalize words which follow end-of-sentence punctuation
  my @tokens = split(' ',$str);
  for (my $i=0;$i<scalar(@tokens);$i++) {
    my $tok = $tokens[$i];
    my $cap = $capitalizedWords{$tok};
    my $prev = $tokens[$i-1];
    if ($cap) {
      $tokens[$i] = $cap;
    }
    elsif ($prev =~ /[\.\?\!]$/ and !($tok =~ /^[A-Z]/)) {
      $tokens[$i] = ucfirst($tok);
    }
  }
  $str = join(" ",@tokens);
  # Apply capitalization regexes
  foreach my $regex (keys(%capitalizedRegexes)) {
    my $value = $capitalizedRegexes{$regex};
    $str =~ s/$regex/$value/ig;
  }
  # Capitalize the first word unless it is already capitalized (it might be all caps).
  $str = ucfirst($str) unless $str =~ /^[A-Z]/;
  return $str;
}

sub readCapitalizationsFile {
  my ($file) = @_;
  open(INPUT,$file) or die "can't read $file";
  while (<INPUT>) {
    s/^\s+//;
    s/\s+$//;
    next unless $_;  # Skip blank lines.
    next if /^\#/;   # Skip comment lines
    my $name = $_;
    capitalize($name);
  }
  close(INPUT);
}

sub capitalize {
  my ($name) = @_;
  if ($name =~ /^\S+\s+\S+/) {
    my $regex = "\\b$name\\b";
    $capitalizedRegexes{$regex}=$name;
  } else {
    my $key = lc($name);
    $capitalizedWords{$key} = $name;    
  }
}

sub digitizeTeens {
  my ($str) = @_;
  $str =~ s/ten/10/g;
  # $str =~ s/(\d)ten/$110/g;
  # $str =~ s/ten\d/10$1/g;

# $str =~ s/eleven/11/g;
#   $str =~ s/twelve/12/g;
#   $str =~ s/thirteen/13/g;
#   $str =~ s/fourteen/14/g;
#   $str =~ s/fifteen/15/g;
#   $str =~ s/sixteen/16/g;
#   $str =~ s/seventeen/17/g;
#   $str =~ s/eighteen/18/g;
#   $str =~ s/nineteen/18/g;
  return $str;
}

sub fixSpelledPhoneNumbers {
  my ($str) = @_;
  $str = digitizeTeens($str);
  # if ($str =~ m/($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)/) {
  if ($str =~ m/($SEP)($NON_ZERO)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)/) {

    my $s0 = escapeSepr($1);

    my $n1 = $2;
    my $s1 = escapeSepr($3);

    my $n2 = $4;
    my $s2 = escapeSepr($5);

    my $n3 = $6;
    my $s3 = escapeSepr($7);

    my $n4 = $8;
    my $s4 = escapeSepr($9);

    my $n5 = $10;
    my $s5 = escapeSepr($11);

    my $n6 = $12;
    my $s6 = escapeSepr($13);

    my $n7 = $14;
    my $s7 = escapeSepr($15);

    my $n8 = $16;
    my $s8 = escapeSepr($17);

    my $n9 = $18;
    my $s9 = escapeSepr($19);

    my $n10 = $20;

    my $nn1 = decodeNumber($n1);
    my $nn2 = decodeNumber($n2);
    my $nn3 = decodeNumber($n3);
    my $nn4 = decodeNumber($n4);
    my $nn5 = decodeNumber($n5);
    my $nn6 = decodeNumber($n6);
    my $nn7 = decodeNumber($n7);
    my $nn8 = decodeNumber($n8);
    my $nn9 = decodeNumber($n9);
    my $nn10 = decodeNumber($n10);

    $str =~ s/$s0$n1$s1$n2$s2$n3$s3$n4$s4$n5$s5$n6$s6$n7$s7$n8$s8$n9$s9$n10/ $nn1$nn2$nn3 $nn4$nn5$nn6-$nn7$nn8$nn9$nn10 /g;
  }
  return $str;
}

sub escapeSepr {
  my ($str) = @_;
  my @tokens = split(//,$str);
  my $result = "";
  foreach my $tok (@tokens) {
    $result .= "\\";
    $result .= $tok;
  }
  return $result;
}


# NOTE: No longer used for phone numbers; could it be useful for other contexts?

# sub fixSpelledNumbers {
#   my ($line) = @_;
#   while (1) {
#     my $matchFound = 0;
#     while ($line =~ /(\d)(zero|one|two|three|four|five|six|seven|eight|nine)/) {
#       my $orig  = $line;
#       my $digit  = $1;
#       my $spelled = $2;
#       my $decoded = decodeNumber($spelled);
#       $line =~ s/$digit$spelled/$digit$decoded/g;
#       # printf STDERR "pre: %s => %s\n",$orig,$line;
#       $matchFound = 1;
#     }
#     while ($line =~ /(zero|one|two|three|four|five|six|seven|eight|nine)(\d)/) {
#       my $orig  = $line;
#       my $spelled = $1;
#       my $digit  = $2;
#       my $decoded = decodeNumber($spelled);
#       # printf STDERR "digit: $digit spelled: $spelled decoded: $decoded\n";
#       $line =~ s/$spelled$digit/$decoded$digit/g;
#       # printf STDERR ("post: %s => %s\n",$orig,$line);
#       $matchFound = 1;
#     }
#     last unless $matchFound;
#   }
#   return $line;
# }


sub decodeNumber {
  my ($str) = @_;
  return 0 if $str eq "zero" or $str eq "o" or $str eq "0";
  return 1 if $str eq "one" or $str eq "1";
  return 2 if $str eq "two" or $str eq "2";
  return 3 if $str eq "three" or $str eq "3";
  return 4 if $str eq "four" or $str eq "4";
  return 5 if $str eq "five" or $str eq "5";
  return 6 if $str eq "six" or $str eq "6";
  return 7 if $str eq "seven" or $str eq "7";
  return 8 if $str eq "eight" or $str eq "8";
  return 9 if $str eq "nine" or $str eq "9";
  return 10 if $str eq "ten";
  die "Can't decode: '$str'";

}


# Removes initial punctuation from the setennce
sub initialPunct {
  my ($line) = @_;
  $line =~ s/^[\W_]+//g;
  return $line;
}

sub removeRepeatedChars {
  my ($line) = @_;
  $line =~ s/azz/ass/g;
  $line =~ s/ooo+/oo/g;
  $line =~ s/kk+/k/g;
  $line =~ s/zz+/z/g;
  $line =~ s/sss+/ss/g;
  $line =~ s/yy+/y/g;
  $line =~ s/z\b/s/g;
  return $line;
}

sub stripRepeatedPunct {
  my ($line) = @_;
  $line =~ s/(\p{IsPunct})\p{IsPunct}+/ /g;
  $line =~ s/[\'\`\:\.][\'\`\:\.]+/ /g;
  return $line;
}

sub reduceRepeatedPunct {
  my ($line) = @_;
  $line =~ s/\![\!\?]+/! /;
  $line =~ s/\?[\!\?]+/? /;
  return $line;
}

sub ellipsis {
  my ($line) = @_;
  # $line =~ s/\s*\.\.*/\. /g;
  $line =~ s/\.\.*/\. /g;
  return $line;
}

# Replaces all non-ASCII characters, and ASCII representations of non-ASCII characters, with the empty string.
sub stripNonASCII {
  my ($line) = @_;
  $line =~ s/[^[:ascii:]]//g;
  $line =~ s/\\u\d\d\d\d//g;
  $line =~ s/\\u[\da-f]{4}//g;
  $line =~ s/\<u\+[\da-zA-Z]*\>//g;
  return $line;
}

sub standardizeWhitespace {
  my ($line) = @_;
  my @tokens = split(' ',$line);
  return join(" ",@tokens);
}

# Removes HTML tags
sub stripHtml {
  my ($line) = @_;
  # Kill everything inside a <script> </script>
  $line =~ s/<script>[^<>]*<\/script>/ /g;
  $line =~ s/<[^>]+>/ /g;
  # $line =~ s/\<\s*[a-zA-Z]+\s*\/?>/ /g;
  # $line =~ s/\<\s*\/[a-zA-Z]+\s*>/ /g;
  $line =~ s/\&quot\;/ /g;
  $line =~ s/\&amp\;/ /g;
  $line =~ s/\&nbsp\;/ /g;
  $line =~ s/\&[a-zA-Z]{1,6}\;/ /g;
  $line =~ s/\&\#\d*\;//g;
  return $line;
}

sub stripNonAlphaNumeric {
  my ($line) = @_;
  # Leave parens () as sometimes they appear in reference
  # Leave underscore?
  $line =~ s/[\\\/\*\+\^\~\[\]\{\}\<\>\=\"]+/ /g;
  # $line =~ s/[\\\*\+\^\~\[\]\{\}\<\>\=\"]+/ /g;
  return $line;
}
