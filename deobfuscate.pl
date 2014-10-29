use strict;
use warnings;
use Getopt::Long;


$| = 1;

#
# TODO:  Add words/phrases to be capitalized/uppercased
#

my $usage = "\nUsage: [inputFile] [outputFile] [-operations operations-comma-separated] [-capitalize files] [-spellfixes files] -datafiles dir] [-showOps] [-trace] [-linebreaks] [-help] [-usage]\n";

my $SEP = "[^a-zA-Z0-9]{0,4}";

my $DIGIT = "\\d|zero|one|o|l|two|three|four|five|six|seven|eight|nine|ten";

my $NON_ZERO = "[1-9]|one|two|three|four|five|six|seven|eight|nine|ten";

my $useTags = 0;

# my $fromFile  = "786.5l9.5399";
# my $myVersion = "786.519.5399";
# my $myVersion2 = "786.519.5399";

# printf "%d vs. %d\n",length($fromFile),length($myVersion);

# for (my $i=0;$i<length($fromFile);$i++) {
#   my $file = substr($fromFile,$i,1);
#   my $mine = substr($myVersion,$i,1);
#   printf "%s = %s: %d\n",$file,$mine,$file eq $mine;
# }

# printf "PHONE: %s\n",fixSpelledPhoneNumbers("786 519 5399");


# printf "PHONE: %s\n",fixSpelledPhoneNumbers("786 519 5399");

# printf "PHONE: %s\n",fixSpelledPhoneNumbers("6'o2' 688 oo94");
# die("done");

# die("done");
my $capitalFormsFiles = "";
my $spellfixFiles     = "";
my $validWordsFile;


my %validWords;

my $traceOps = 0;
my $trace = 0; # for tokenization
my $maxLines;
my $helpRequested;
my $usageRequested;
my $showOps;
my $dataFilesDir;
my $addLineBreaks;
my $keyMode;

# Took out letter-digit as doing that gained 0.3
my $allOperations = "downcase,html,non-ascii,initial-punct,pre-tokenize,tokenize,non-alphanumeric,ellipsis,repeated-sentence-end,repeated-punct,repeated-chars,phone-numbers,fix-emails,fix-urls,spelled-words,lookup,capitalize,final-replacements,cosmetic,std-whitespace";

my @allOperations = split(/\,/,$allOperations);

my @contractions = qw(i'm i'll i'd i've you're you'll);

# Default is all operations

my $operations = "all";

GetOptions("operations:s" => \$operations,
	   "showOps" => \$showOps,
	   "capitalize:s" => \$capitalFormsFiles,
	   "spellfixes:s" => \$spellfixFiles,
	   "datafiles:s" => \$dataFilesDir,
	   "trace" => \$traceOps,
	   "linebreaks" => \$addLineBreaks,
	   "help" => \$helpRequested,
	   "keymode" => \$keyMode,
	   "usage" => \$usageRequested,
	   "maxlines:i" => \$maxLines,
	   "tags" => \$useTags,
	   "validwords:s" => \$validWordsFile,
	   );

# Quit early if -help or -usage are present in the arugments list.

printHelpAndExit() if $helpRequested;

die $usage if $usageRequested;

$spellfixFiles = "$dataFilesDir/spellfixes.txt" if !$spellfixFiles and $dataFilesDir;
$capitalFormsFiles = "$dataFilesDir/capitalize.txt" if !$capitalFormsFiles and $dataFilesDir;
$validWordsFile = "$dataFilesDir/valid-vocab.word-counts" if !$validWordsFile and $dataFilesDir;

my %capitalizedWords;
my %capitalizedRegexes;

my $inputFile  = shift;
my $outputFile = shift;

# die $usage unless $outputFile;

die sprintf("\nExcess arg(s): %s\n$usage\n",join(" ",@ARGV)) if @ARGV;

my @operations = parseOperations($operations);

warn "No operations; will be a no-op" unless @operations;


$trace = $traceOps;

# printf "TRACE: $trace\n";

my %spellFixWords;
my %spellFixRegexes;
my %spellFixKeys;


# Spell fix list is inserted under the line below. Do NOT remove!
# ++SPELLFIXES++

# Capitalization list is inserted the line below. Do NOT remove!
# ++CAPITALIZE++

# Vocabulary list
# ++VOCAB++


# Read capitalization forms files
foreach my $file (split(/\,/,$capitalFormsFiles)) {
  readCapitalizationsFile($file);
}

# Read the spell fix files
foreach my $file (split(/\,/,$spellfixFiles)) {
  readSpellFixFile($file);
}

readValidWordsFile($validWordsFile) if $validWordsFile;

printOperationsUsed() if $showOps;



####################################################################################


# printf "FIXED: %s\n",join(" ",tokenizeAndFixSpelling('hhhhellllooo.my#1friend decreeeeet eb0nyy $tacy ca$h non rushed'));
# die("done");

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

printf STDERR "\nPROCESSING INPUT...\n\n";


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
  printf $outputHandle "%s\n",transformCompleteMessage($line);
  # Output an additional line break for readability if requested.  Note this will break some follow-on scripts.
  printf $outputHandle "\n" if $addLineBreaks;
  # Pacifier/progress indicator
  printf STDERR "%d\n", $lineNum if ($lineNum%1000 == 0);
  last if defined($maxLines) and $lineNum == $maxLines;
  $lineNum++;
  
}
close($inputHandle);
close($outputHandle);

###########################################################################


sub validWord {
  my ($word,$count) = @_;
  $validWords{$word} = $count;
}

sub transformCompleteMessage {
  my ($str) = @_;
  $str =~ s/`/'/g;
  $str = transform($str);
  # Capitalize the first word unless it is already capitalized (it might be all caps).
  $str = ucfirst($str) unless $str =~ /^[A-Z]/;
  # Remove any leading or trailing whitespace that may have gotten added.
  $str =~ s/^\s+//; 
  $str =~ s/\s+$//;
  # Add final period if sentence is ending in letters - gives a net gain in punctuation WER.
  $str =~ s/([a-zA-Z])$/$1./;
  return $str;
}

sub transform {
  my ($str) = @_;
  $str = stripHtml($str);
  $str = stripNonASCII($str);
  $str = transformSemantic($str); 
  return $str;
}

sub transformSemantic {
  my ($str) = @_;
  printf "CALLED: '%s'\n",$str if $trace;
  if (!defined($str) or $str eq "") {
    return "";
  }
  # Height with feet and inches
  if ($str =~ /^(.*)(?<!\d)([56])\s*('|"|foot|ft|feet),?\s*(\d\d?)\b\s*("|'|inches|inch|in)?(.*)$/i) {
    my $feet = $2;
    my $inches = $4;
    if (!($inches =~ /^0/) and $inches <= 11) {
      my $pre = $1;
      my $post = $6;
      my $height = "$feet'$inches";
      printf "MATCHED HEIGHT(1) '%s'\n pre: '%s'\n post: '%s'\n",$height,$pre,$post if $trace;
      return append(transform($pre),tag("ht",$height),transform($post));
    }
  }
  if ($str =~ /^(.*)(?<!\d)($NON_ZERO)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)(?!\d)(.*)$/i) {
    my $pre  = $1;
    # my $s0 = escapeSepr($1);

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

    my $post = $21;

    my $area = join("",$nn1,$nn2,$nn3);
    my $exch = join("",$nn4,$nn5,$nn6);
    my $dig4 = join("",$nn7,$nn8,$nn9,$nn10);
    my $phone = "$area $exch-$dig4";
    print "MATCHED PHONE: $phone\n pre: $pre\n post: $post\n" if $trace;
    # $str =~ s/$s0$n1$s1$n2$s2$n3$s3$n4$s4$n5$s5$n6$s6$n7$s7$n8$s8$n9$s9$n10/ $nn1$nn2$nn3 $nn4$nn5$nn6-$nn7$nn8$nn9$nn10 /g;
    return append(transform($pre),tag("phone",$phone),transform($post));
  }
  # '100%' as in 100% real
  if ($str =~ /^(.*)(1[o0][o0]+)\s*%(.*)$/i) {
    my $pre = $1;
    my $perc = $2;
    my $post = $3;
    $perc =~ s/o/0/ig;
    $perc .= "%";
    return append(transform($pre),$perc,transform($post));
  }
  # URL starting with http(s)
  if ($str =~ /^(.*)(https?\S+)\b(.*)$/) {
    my $pre = $1;
    my $url = $2;
    my $post = $3;
    printf "MATCHED URL(1): '$2' pre: '$pre' post: '$post'\n" if $trace;
    return append(transform($pre),tag("url",$url),transform($post));
  }
  # URL starting with www
  if ($str =~ /^(.*)\b(www\.\S+)\b(.*)$/) {
    my $pre = $1;
    my $url = $2;
    my $post = $3;
    printf "MATCHED URL(2): '$url' pre: '$pre' post: '$post'\n" if $trace;

    return append(transform($pre),tag("url",$url),transform($post));
  }
  # Email ending in .com, .net, etc.
  if ($str =~ /^(.*)\b(\S+)@(\S+)\.(com|net|us|mx)\b(.*)$/) {
    my $pre = $1;
    my $email = $2 . "@" . $3 . "." . $4;
    my $post = $5;
    print "MATCHED EMAIL(1): '$email'\n pre: '$pre'\n post: '$post'\n" if $trace;
    return append(transform($pre),tag("email",$email),transform($post));
  }
  # Email for known provider (gmail, hotmail, etc), but with dropped '.com'
  if ($str =~ /^(.*)\b(\S+)@(yahoo|gmail|hotmail)\b(.*)$/) {
    my $pre = $1;
    my $email = $2 . $3 . ".com";
    my $post = $4;
    print "MATCHED EMAIL(2): '$email'\n pre: '$pre'\n post: '$post'\n" if $trace;
    return append(transform($pre),tag("email",$email),transform($post));
  }
  # URL without www or http
  if ($str =~ /^(.*)\b(\S+)\.(com|net)(.*)$/) {
    my $pre = $1;
    my $url = "$2.$3";
    my $post = $4;
    print "MATCHED URL(3): '$url'\n pre: '$pre'\n post: '$post'\n" if $trace;
    return append(transform($pre),tag("url",$url),transform($post));
  }
  # Dollar amount with pre-pended '$'
  if ($str =~ /^(.*)\$\s*(\d\d\d?)\b(.*)$/) {
    my $pre = $1;
    my $amt  = "\$$2";
    my $post = $3;
    printf "MATCHED DOLLAR(1): '$amt' pre: '$pre' post: '$post'\n" if $trace;
    return append(transform($pre),tag("\$",$amt),transform($post));
  }
  # Dollar amount with post-pended 'dollar(s)'
  # if ($str =~ /^(.*)(?<!\d)(\d\d\d?)(?>!\d)\s*(dollars|\$)(.*)$/) {
  if ($str =~ /^(.*)(?<!\d)(\d\d\d?)\s*(dollars|dollar)(.*)$/) {
    my $pre = $1;
    my $amt = "\$$2";
    my $post = $4;
    printf "MATCHED DOLLAR(2): '$amt' pre: '$pre' post: '$post'\n" if $trace;
    return append(transform($pre),tag('$',$amt),transform($post));
  }
  # Dollar amount with directly post-pended '$'
  if ($str =~ /^(.*)\b(\d\d\d?)\$(?!\S)(.*)$/) {
    my $pre = $1;
    my $amt = "\$$2";
    my $post = $3;
    printf "MATCHED DOLLAR(3): '$amt'\n pre: '$pre'\n post: '$post'\n" if $trace;
    return append(transform($pre),tag('$',$amt),transform($post));
  }

  # Height with just feet
  # if ($str =~ /^(.*)(?<!\d)([456])\s*('|"|foot|ft|feet)(?>!$DIGIT)(.*)$/) {
  if ($str =~ /^(.*)(?<!\d)([456])\s*('|"|foot|ft|feet)(.*)$/) {
    printf "MATCHED HEIGHT(2) '$2'\n pre: '$1'\n post: '$3'\n" if $trace;
    my $pre = $1;
    my $post = $4;
    my $height = "$2'";

    return append(transform($pre),tag("ht",$height),transform($post));
  }
  # Weight
  if ($str =~ /^(.*)(\d\d\d)\s*(pounds|pound|lbs|lb|pnds|pnd|wt)(.*)$/i) {
    my $qty = $2;
    my $pre = $1;
    my $post = $4;
    printf "MATCHED WEIGHT %s\n pre: '%s'\n post: '%s'\n",$qty,$pre,$post if $trace;

    my @result = (transform($pre),tag("wt","$qty lbs"),transform($post));

    return append(@result);
  }
  # Cup size
  if ($str =~ /^(.*)(3\d)\s*([BCDEF]+)('?s)?(.*)$/i) {
    my  $pre = $1;
    my $post = $5;
    my $n = $2;
    my $cup = uc($3);
    my $poss = $4;
    $poss = "" unless defined($poss);
    $poss = "'s" if lc($poss) eq "s";
    print "MATCHED CUP SIZE: '$n$cup$poss'\n pre: '$pre'\n post: '$post'\n" if $trace;
    return append(transform($pre),tag("cup","$n$cup$poss"),transform($post));
  }

  return transformGeneralText($str);
}

sub append {
  my (@items) = @_;
  my $result = join(" ",@items);
  print "APPEND: $result\n" if $trace;
  return $result;
}

sub tag {
  my ($tag,$str) = @_;
  if ($useTags) {
    return "<$tag>$str</$tag>";
  }
  else {
    return $str;
  }
}

# sub trace {
#   my @list = @_;
#   printf "RETURNING: %s\n",join(" ",@list) if $trace;
#   return @list;
# }


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
  if ($tok =~ /^[a-zA-Z'@]+$/) {
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





sub splitOnCharClass {
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
  return @tokens;
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

  return $str;
}

sub readSpellFixFile {
  my ($file) = @_;
  printf STDERR "Reading spellfix file $file\n";
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

sub readValidWordsFile {
  my ($file) = @_;
  printf STDERR "Reading valid words file $file\n";
  open(FILE,$file) or die "Can't read $file";
  while (<FILE>) {
    my ($word,$count) = split;
    $validWords{$word}=$count;
  }
  close(FILE);
}

sub spellfix {
  my ($spellfix) = @_;
  my ($key,$fix) = split(/\=\>/,$spellfix);
  die "Bad line: $_" unless defined($fix);
  $key = fixWhiteSpace($key);
  $fix = fixWhiteSpace($fix);
  foreach my $tok (split(' ',$fix)) {
    $validWords{$tok}++;
  }
  printf "$key being mapped to itself\n" if $key eq $fix;  
  my $current = $spellFixKeys{$key};
  printf "Already have a fix for $key: $current vs. $fix\n" if $current;
  $spellFixKeys{$key} = $fix;
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
  $str =~ s/\r/ /g;
  $str =~ s/([a-zA-Z])\$([a-zA-Z])/$1s$2/g;
  return $str;
}

sub fixAmounts {
  my ($str) = @_;
  # Put dollar signs next to number
  $str =~ s/\$ (\d\d+)/\$$1/g;
  # Put '%' next to number
  $str =~ s/100 \%/100\%/g;
  $str =~ s/1 oo \%/100\%/g;  # 100% spelt deviantly as 1 oo
  $str =~ s/(\d+) \$/\$ $1/g;   # Permuted dollar amounts:  '100 $' vs. '$ 100'
  $str =~ s/(\d\d)\s+(b|c|d|dd|ddd|e|ee)\b/$1 . uc($2)/ge;  # Cup size
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
  $str = fixSpelledWordsWithSeparator($str,"   ");
  $str = fixSpelledWordsWithSeparator($str," \* ");
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
  printf STDERR "inputFile    Input file of sentences to be transformed, one sentence per line.  Inputs from stdin if this file is not provided. \n";
  printf STDERR "outputFile   Output file of transformed sentences, one sentence per line. Outputs to stdin if this file is not provided.\n";
  printf STDERR "-operations  Comma-separated list of operation names to use. The token 'all' means all operations. Prefixing an operation name with a '!' means don't use it\n";
  printf STDERR "-capitalize  Comma-separated list of files which contain 'capitalization forms' like 'Mary', 'Palm Springs', and 'NYC',etc.\n";
  printf STDERR "-spellfixes  Comma-separated list of files which contain spell fixes like 'gentalmen => gentlemen'.\n";
  printf STDERR "-datafiles   Directory to find data files in. Equivalent to '-spellfixes dir/spellfixes.txt -capitalize dir/capitalize.txt\n";
  printf STDERR "-trace       Prints out the result of each operation as it is applied to each sentence.  Not recommended unless you only have a few sentences!\n";
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

sub transformGeneralText {
  my ($line) = @_;
  printf STDERR "GENERAL TEXT: %s\n\n",$line if $traceOps;
  $line = tokenizeAndFixSpelling($line);
  foreach my $op (@operations) {
    if ($op eq "downcase") {
      $line = downcase($line);
    }
    elsif ($op eq "cosmetic") {
      $line = makeCosmeticFixes($line);
    }
    elsif ($op eq "html") {
      # $line = stripHtml($line);
    }
    elsif ($op eq "non-ascii") {
      # $line = stripNonASCII($line);
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
      # $line = preTokenize($line);
    }
    elsif ($op eq "repeated-punct") {
      $line = stripRepeatedPunct($line);
    }
    elsif ($op eq "initial-punct") {
      $line = initialPunct($line);
    }
    elsif ($op eq "tokenize") {
      # $line = join(" ",splitOnCharClass($line));
    }
    elsif ($op eq "repeated-chars") {
      $line = removeRepeatedChars($line);
    }
    elsif ($op eq "lookup") {
      $line = applySpellFixes($line);
    }
    elsif ($op eq "phone-numbers") {
      # $line = fixSpelledPhoneNumbers($line);
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

sub tokenizeAndFixSpelling {
  my ($str) = @_;
  $str = applySpellFixRegexes($str);
  my @result;
  my $TRACE = 0;
  my @working = split(' ',$str);
  while (@working) {
    my $tok = shift(@working);
    printf "tok: $tok\n" if $TRACE;
    if (isValidToken($tok)) {
      printf "Is valid\n" if $TRACE;
      push(@result,$tok);
    }
    else {
      my @exp = splitOnRegularPunctuation($tok);
      printf "split regular: %s\n",join(" ",@exp) if $TRACE;
      if ($exp[0] eq $tok) {  # Splitting didn't change it
	printf "Splitting on regular punct didn't change\n" if $TRACE;
	my $fixed = fixDuplicateChars($tok);
	if ($fixed ne $tok) {
	  printf "Removing duplicate chars changed it: $fixed\n" if $TRACE;
	  unshift(@working,$fixed);
	}
	else {
	  printf "Removing duplicate chars didn't change\n" if $TRACE;
	  my $tentative = applyTentativeSpellFixes($tok);
	  if (isValidToken($tentative)) {
	    printf "Removing tentative changed it and made it valid: $fixed\n" if $TRACE;
	    push(@result,$tentative);
	  }
	  else {
	    @exp = lookupSpellFix($tok);
	    if ($exp[0] eq $tok) {
	      printf "Spell lookup didn't change it\n" if $TRACE;
	      @exp = splitOnCharClass($tok);
	      if ($exp[0] eq $tok) {
		printf "Splitting on char class didn't change it\n" if $TRACE;
		push(@result,$tok);
	      }
	      else {
		printf "Splitting on char class changed it\n" if $TRACE;
		unshift(@working,@exp);
	      }
	    }
	    else {
	      printf "Spell lookup changed it: %s\n",join(" ",@exp) if $TRACE;
	      unshift(@working,@exp);
	    }
	  }
	}
      }
      else {
	printf "Split on regular punct: %s\n",join(" ",@exp) if $TRACE;
	unshift(@working,@exp);
      }
    }
  }
  my $result = join(" ",@result);
  $result = applySpellFixRegexes($result);
  return $result;
}


sub splitOnRegularPunctuation {
  my ($str) = @_;
  my $allowed = 'a-zA-Z\'\d\$\@';
  if ($str =~ /^([$allowed]*)([^$allowed]+)([$allowed]*)$/) {
    my @tokens;
    my $pre = $1;
    my $punct = $2;
    my $post = $3;
    push(@tokens,$pre) if $pre;
    push(@tokens,$punct);
    if ($post) {
      push(@tokens,splitOnRegularPunctuation($post));
    }
    return @tokens;
  }
  else {
    return ($str);
  }
}

sub applyTentativeSpellFixes {
  my ($str) = @_;
  $str =~ s/\@/a/g;
  $str =~ s/\$/s/g;
  $str =~ s/0/o/g;
  return $str;
}

sub isValidToken {
  my ($str) = @_;
  $str = lc($str);
  return (isValidWord($str) or 
	  $str =~ /^[\d\$\%]+$/ or 
	  $str =~ /^[^a-z\'\d]+$/);
}

sub fixDuplicateChars {
  my ($str) = @_;
  $str =~ s/azz/ass/g;

  # Two down to one
  $str =~ s/([bcdghnpquvwxyz])\1+/$1/g;
  # Three copies down to two
  $str =~ s/([a-z])\1\1+/$1$1/g;
  
  $str =~ s/oo\b/o/;

  return $str;
}

sub fixWord {
  my ($str) = @_;
  if ($str =~ /^mmm+$/) {
    return qw(mmm);
  }
  elsif ($str =~ /^ahh+$/) {
    return qw(ahh);
  }
  $str =~ s/azz/ass/g;
  # Two down to one
  $str =~ s/([bcdghnpquvwxyz])\1+/$1/g;
  # Three copies down to two
  $str =~ s/([a-z])\1\1+/$1$1/g;
  
  $str =~ s/oo\b/o/;

  return $str;
}

sub isValidWord {
  my ($word) = @_;
  $word = lc($word);
  return $validWords{$word};
}

# TODO: Only handles a specific number of path entries right now...
sub fixUrls {
  my ($str) = @_;
  $str =~ s/http\s*\:\s*([a-zA-Z]+)\s*\.\s*([a-zA-Z]+)\s*\.\s*(com|net)/"http:\/\/" . combine("$1 $2",".") . ".$3"/ge;
  # $str =~ s/http\s*\:\s*([a-zA-Z]+(\s*\.\s*[a-zA-Z]+))\s*\.\s*com/"http:\/\/" . combine("$1 $2","") . ".com"/ge;
  return $str;
}


sub NEWfixUrls {
  my ($str) = @_;
  $str =~ s/http[\s:\/]*(.*)((com|net|us|mx))/"http:\/\/" . purgeWhiteSpace($1) . $2/ge;
  return $str;
}

sub purgeWhiteSpace {
  my ($str) = @_;
  $str =~ s/\s+//g;
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

sub applySpellFixRegexes {
  my ($line) = @_;
  $line =~ s/xo[xo]*/xoxo/g;
  for my $regex (keys(%spellFixRegexes)) {
    my $fix = $spellFixRegexes{$regex};
    $line =~ s/$regex/$fix/ig;
  }
  return $line;
}

sub lookupSpellFix {
  my ($word) = @_;
  my $fix = $spellFixWords{lc($word)};
  if (defined($fix)) {
    return split(' ',$fix);
  }
  else {
    return ($word);
  }
}

sub applySpellFixes {
  my ($line) = @_;
  $line = applySpellFixRegexes($line);
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
  $str =~ s/(\W)i'll(\W)/$1I'll$2/g;
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
  return $str;
}

sub readCapitalizationsFile {
  my ($file) = @_;
  printf STDERR "Reading capitalizations file $file\n";
  open(INPUT,$file) or die "can't read $file";
  while (<INPUT>) {
    s/^\s+//;
    s/\s+$//;
    next unless $_;  # Skip blank lines.
    next if /^\#/;   # Skip comment lines
    my $name = $_;
    printf "Not capitalized: $name\n" unless $name =~ /^[A-Z]/;
    capitalize($name);
  }
  close(INPUT);
}

sub capitalize {
  my ($name) = @_;
  foreach my $tok (split(' ',$name)) {
    $validWords{lc($tok)}++;
  }
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
  if ($str =~ /(.*)($NON_ZERO)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)($SEP)($DIGIT)/) {

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
  $str = lc($str);
  return 0 if $str eq "zero" or $str eq "o" or $str eq "0" or $str eq "O";
  return 1 if $str eq "one" or $str eq "1" or $str eq "l";
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
  $line =~ s/([a-z])\1\1+/$1$1/g;
#   $line =~ s/ooo+/oo/g;
#   $Line =~ s/aa+/aa/g;
#   $line =~ s/kk+/k/g;
#   $line =~ s/zz+/z/g;
#   $line =~ s/sss+/ss/g;
#   $line =~ s/yy+/y/g;
#   $line =~ s/z\b/s/g;
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
  $line =~ s/\.\.+/\. /g;
  return $line;
}

# Replaces all non-ASCII characters, and ASCII representations of non-ASCII characters, with the empty string.
sub stripNonASCII {
  my ($line) = @_;
  $line =~ s/[^[:ascii:]]//g;
  $line =~ s/\\u\d\d\d\d//g;
  $line =~ s/\\u[\da-f]{4}//g;
  $line =~ s/\<u\+[\da-zA-Z]*\>//g;
  $line =~ s/\r/ /g;
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

  # $line =~ s/\&quot\;/ /g;
  $line =~ s/\&quot\;/"/g;
  $line =~ s/\&#039\;/'/g;
  $line =~ s/\&amp\;/ /g;
  $line =~ s/\&nbsp\;/ /g;
  $line =~ s/\&[a-zA-Z]{1,6}\;/ /g;
  $line =~ s/\&\#\d*\;//g;
  $line =~ s/\&\#\d+//g;
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
