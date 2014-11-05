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

my $inputFile;
my $outputFile;
my $useTags = 0;
my $capitalFormsFiles = "";
my $capsDecisionFile;
my $spellfixFiles     = "";
my $validWordsFile;
my $traceOps = 0;
my $trace = 0; # for tokenization
my $maxLines;
my $helpRequested;
my $usageRequested;
my $showOps;
my $dataFilesDir;
my $addLineBreaks;
my $keyMode;
my $operations = "all";
my $minWordCount = 1;



# Took out letter-digit as doing that gained 0.3
my $allOperations = "downcase,html,non-ascii,initial-punct,pre-tokenize,tokenize,non-alphanumeric,ellipsis,repeated-sentence-end,repeated-punct,repeated-chars,phone-numbers,fix-emails,fix-urls,spelled-words,lookup,capitalize,final-replacements,cosmetic,std-whitespace";

my @allOperations = split(/\,/,$allOperations);

my @contractions = qw(i'm i'll i'd i've you're you'll);

# Default is all operations



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
	   "minwordcount:i" => \$minWordCount,
	   );

# Quit early if -help or -usage are present in the arugments list.

printHelpAndExit() if $helpRequested;

die $usage if $usageRequested;

$spellfixFiles = "$dataFilesDir/spellfixes.txt" if !$spellfixFiles and $dataFilesDir;
$capitalFormsFiles = "$dataFilesDir/capitalize.txt" if !$capitalFormsFiles and $dataFilesDir;
$capsDecisionFile  = "$dataFilesDir/caps.decisions" if $dataFilesDir;
$validWordsFile = "$dataFilesDir/valid-vocab.word-counts" if !$validWordsFile and $dataFilesDir;

# Get the input and output files

$inputFile  = shift;
$outputFile = shift;

# die $usage unless $outputFile;

die sprintf("\nExcess arg(s): %s\n$usage\n",join(" ",@ARGV)) if @ARGV;

my @operations = parseOperations($operations);

warn "No operations; will be a no-op" unless @operations;


$trace = $traceOps;

my $singletonProb;
my %capitalizedWords;
my %capitalizedRegexes;
my %validWords;
my %wordProbs;
my %spellFixWords;
my %spellFixRegexes;
my %spellFixKeys;
my $spellTree = {};



# Vocabulary list
# ++VOCAB++

# Capitalization list is inserted the line below. Do NOT remove!
# ++CAPITALIZE++

# Spell fix list is inserted under the line below. Do NOT remove!
# ++SPELLFIXES++



readValidWordsFile($validWordsFile) if $validWordsFile;


# Read the explicit capitalization forms files
foreach my $file (split(/\,/,$capitalFormsFiles)) {
  readCapitalizationsFile($file);
}

# Data-driven capitalization is hurting us right now

# Read the automatically-derived caps forms next.  These will not override the explicit ones.
# readCapsDecisionFile($capsDecisionFile);

# Read the spell fix files
foreach my $file (split(/\,/,$spellfixFiles)) {
  readSpellFixFile($file);
}



buildSpellTree();


printOperationsUsed() if $showOps;


# printf "FIXED: %s\n",fixSpelledWords("i am a");

# findWords(split(//,"disclaimer"));
# die('done');

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

# printf STDERR "PERC: %s\n",transform("1oOoo percent real");

# my @tmp = split(' ',"h o t n e w  g o r g e o u s  b l o n d e  p l a y m a t e");
# printf STDERR "TMP: %s\n",join(" ",findWords(@tmp));

# printf STDERR "FIXED: %s\n",fixSpelledWords("hello my name is s e x y");

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
    $line = decodeJSONString($content);
    # Output the key and tab separator as the first thing on the line.
    printf $outputHandle "%s\t",$uri;  
  }
  my $transformed = transformCompleteMessage($line);
  $transformed = encodeJSONString($transformed) if $keyMode;
  # Output the deobfuscated content.
  printf $outputHandle "%s\n",$transformed;
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


sub replaceOsWithZeros {
  my ($str) = @_;
  $str =~ s/[oO]/0/g;
  return $str;
}

sub transformCompleteMessage {
  my ($str) = @_;
  $str =~ s/`/'/g;
  $str =~ s/\b(\d+)([oO]+)\b/$1 . replaceOsWithZeros($2)/eg;
  $str = transform($str);
  # Capitalize the first word unless it is already capitalized (it might be all caps).
  $str = ucfirst($str) unless $str =~ /^[A-Z]/;
  $str =~ s/\([^a-zA-Z\d]*\)//g;
  # Get rid of useless parens
  $str =~ s/\([^a-zA-Z\d]*\)//g;
  $str =~ s/^(.*)\(([^\)]*)$/$1$2/g;
  $str =~ s/^([^\(]*)\)(.*)$/$1$2/g;

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
  printf STDERR "CALLED: '%s'\n",$str if $trace;
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
      printf STDERR "MATCHED HEIGHT(1) '%s'\n pre: '%s'\n post: '%s'\n",$height,$pre,$post if $trace;
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
  # '100%' as in "100% real"
  if ($str =~ /(1[o0][o0]+)\s*(%|percent|perc)/i) {
    my ($pre,$perc,$post) = ($`,$1,$');
    $perc =~ s/o/0/ig;
    $perc .= "%";
    return append(transform($pre),$perc,transform($post));
  }
  # URL starting with http(s)
  if ($str =~ /^(.*)(https?\S+)\b(.*)$/i) {
    my $pre = $1;
    my $url = $2;
    my $post = $3;
    printf STDERR "MATCHED URL(1): '$2' pre: '$pre' post: '$post'\n" if $trace;
    return append(transform($pre),tag("url",$url),transform($post));
  }
  # URL starting with www
  if ($str =~ /www(\.|dot)\S+/i) {
    my $pre  = $`;
    my $url  = $&;
    my $post = $';
    $url =~ s/dot/\./g;
    printf STDERR "MATCHED URL(2): '$url' pre: '$pre' post: '$post'\n" if $trace;

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
    printf STDERR "MATCHED DOLLAR(1): '$amt' pre: '$pre' post: '$post'\n" if $trace;
    return append(transform($pre),tag("\$",$amt),transform($post));
  }
  # Dollar amount with post-pended 'dollar(s)'
  if ($str =~ /^(.*)(?<!\d)(\d\d\d?)\s*(dollars|dollar)(.*)$/) {
    my $pre = $1;
    my $amt = "\$$2";
    my $post = $4;
    printf STDERR "MATCHED DOLLAR(2): '$amt' pre: '$pre' post: '$post'\n" if $trace;
    return append(transform($pre),tag('$',$amt),transform($post));
  }
  # Dollar amount with directly post-pended '$'
  if ($str =~ /^(.*)\b(\d\d\d?)\$(?!\S)(.*)$/) {
    my $pre = $1;
    my $amt = "\$$2";
    my $post = $3;
    printf STDERR "MATCHED DOLLAR(3): '$amt'\n pre: '$pre'\n post: '$post'\n" if $trace;
    return append(transform($pre),tag('$',$amt),transform($post));
  }

  # Height with just feet
  # if ($str =~ /^(.*)(?<!\d)([456])\s*('|"|foot|ft|feet)(?>!$DIGIT)(.*)$/) {
  if ($str =~ /^(.*)(?<!\d)([456])\s*('|"|foot|ft|feet)(.*)$/) {
    printf STDERR "MATCHED HEIGHT(2) '$2'\n pre: '$1'\n post: '$3'\n" if $trace;
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
    printf STDERR "MATCHED WEIGHT %s\n pre: '%s'\n post: '%s'\n",$qty,$pre,$post if $trace;

    my @result = (transform($pre),tag("wt","$qty lbs"),transform($post));

    return append(@result);
  }
  # A+
  if ($str =~ /\b[aA]\+/) {
    my ($pre,$post) = ($`,$');
    return append(transform($pre),tag("A+","A+"),transform($post));
  }
  # Cup size with an indicator, directly adjacent or not
  if ($str =~ /([34]\d)\s*(([BCDEF])\3*)\s*(cup size|cupsize|cups|cup|s|\'s)/i) {
    my $pre = $`;
    my $post = $';
    my $chest= $1;
    my $letters = uc($2);
    my $cupPoss = $4;
    # printf "letters: '%s'\n",$letters;
    $cupPoss = "" unless defined($cupPoss);
    $cupPoss = "'s" if lc($cupPoss) eq "s";
    $cupPoss = " $cupPoss" if ($cupPoss =~ /cup/);
    my $result = "$chest$letters$cupPoss";
    print "MATCHED CUP SIZE (1): '$result'\n pre: '$pre'\n post: '$post'\n" if $trace;
    return append(transform($pre),tag("cup",$result),transform($post));
  }
  if ($str =~ /([34]\d)\s*(([BCDEF])\3*)(?![a-zA-Z])/i) {
    my $pre = $`;
    my $post = $';
    my $chest= $1;
    my $letters = uc($2);
    # printf "letters: '%s'\n",$letters;
    my $result = "$chest$letters";
    print "MATCHED CUP SIZE (2): '$result'\n pre: '$pre'\n post: '$post'\n" if $trace;
    return append(transform($pre),tag("cup",$result),transform($post));
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
#   printf STDERR "RETURNING: %s\n",join(" ",@list) if $trace;
#   return @list;
# }


sub encodeJSONString {
  my ($str) = @_;
  $str =~ s/\n/\\n/g;
  $str =~ s/\t/\\t/g;
  $str =~ s/\r/\\r/g;
  $str =~ s/\"/\\"/g;
  $str = '"' . $str . '"';
  return $str;
}

sub decodeJSONString {
  my ($str) = @_;
  $str =~ s/^\"//;
  $str =~ s/\"$//;
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
    # printf "%s = %d\n",$word,$count if $word eq "disclaimer";
    next if $count < $minWordCount;
    $validWords{$word}=$count;
    # printf "valid? %d\n",isValidWord($word) if $word eq "disclaimer";
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
  printf STDERR "$key being mapped to itself\n" if $key eq $fix;  
  my $current = $spellFixKeys{$key};
  printf STDERR "Already have a fix for $key: $current vs. $fix\n" if $current;
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

sub allValidWords {
  my @words =@_;
  foreach my $word (@words) {
    return 0 if (!isValidWord($word)) 
  }
  return 1;
}


sub fixSpelledWords {
  my ($str) = @_;
  # printf "ARG: %s\n",$str;
  # printf "is valid: %d\n",isValidWord("disclaimer");
  my $letter = "[a-zA-Z']";
  my $sep    = '[^a-zA-Z\'\d]+';
  my $digraph = $letter . '{1,2}';

  if ($str =~ /(?<![a-zA-Z\d])$letter($sep$digraph){2,}(?![a-zA-Z\d])/i) {

    my $pre =  $`;
    my $match = $&;
    my $post = $';

    # printf "pre: '%s'\nmatch: '%s'\npost: '%s'\n",$pre,$match,$post;
    $match =~ s/$sep/ /g;
    my @whiteTokens = split(' ',$match);
    if (allValidWords(@whiteTokens)) {
      # printf "BLOCKED(1): $match\n";
      return fixSpelledWords($pre) . $match . fixSpelledWords($post);
    }
    else {
      # printf "MATCHED: $match\n";

      my @tokens;
      foreach my $tok (@whiteTokens) {
	push(@tokens,split(//,$tok));
      }
      # printf "tokens: %s\n",join(" ",@tokens);
      my @words  = findWords(@tokens);
      if (@words) {
	$match = join(" ",@words);
      }
      return fixSpelledWords($pre) . $match . fixSpelledWords($post);
    }

  }
  return $str;
}


# sub fixSpelledWordsWithSeparator {
#   my ($str,$sepr) = @_;
#   $str =~ s/\b([a-zA-Z])$sepr([a-zA-Z])(($sepr[a-z])*)\b/$1 . $2 .  remove($3,$sepr)/ge;
#   return $str;
# }


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
      # $line = removeRepeatedChars($line);
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
  my $TRACE = $trace;
  my @working = split(' ',$str);
  while (@working) {
    my $tok = shift(@working);
    # If it's spell-mapped, change it regardless of whether it's "valid"
    if ($spellFixWords{lc($tok)}) {
      push(@result,split(' ',$spellFixWords{lc($tok)}));
      next;
    }
    # Check to see if it's a valid token
    printf STDERR "tok: $tok\n" if $TRACE;
    if (isValidToken($tok)) {
      printf STDERR "Is valid\n" if $TRACE;
      push(@result,$tok);
      next;
    }
    $tok = lc($tok);
    # Split on regular punctuation
    my @exp = splitOnRegularPunctuation($tok);
    if ($exp[0] ne $tok) {
      printf STDERR "Splitting on regular punct changed it\n" if $TRACE;
      unshift(@working,@exp);
      next;
    }
    # Fix duplicate chars
    printf STDERR "Splitting on regular punct didn't change\n" if $TRACE;
    my $fixed = fixDuplicateChars($tok);      
    if ($fixed ne $tok) {
      printf STDERR "Removing duplicate chars changed it: $fixed\n" if $TRACE;
      unshift(@working,$fixed);
      next;
    }
    printf STDERR "Removing duplicate chars didn't change\n" if $TRACE;
    # Tentative spell fixes
    my $tentative = applyTentativeSpellFixes($tok);
    if (isValidToken($tentative)) {
      printf STDERR "Removing tentative changed it and made it valid: $fixed\n" if $TRACE;
      push(@result,$tentative);
      next;
    }
    # Lookup spell fix
    @exp = lookupSpellFix($tok);
    if ($exp[0] ne $tok) {
      printf STDERR "Look up spell fix changed it\n" if $TRACE;
      push(@result,@exp);	# assume it's valid
      next;
    }
    printf STDERR "Spell lookup didn't change it\n" if $TRACE;
    @exp = splitOnCharClass($tok);
    if ($exp[0] ne $tok) {      
      printf STDERR "Split on char class changed it\n" if $TRACE;
      unshift(@working,@exp);
      next;
    }

    @exp = findWords(split(//,lc($tok)));
    if (@exp) {
      printf STDERR "\nSPELL TREE WORKED: %s => %s\n\n",$tok,join(" ",@exp) if $TRACE;
      # printf STDERR "Spell tree changed it!" if $TRACE;
      my $correctionProb = getUnigramLogProb(@exp);
      my $origProb       = getUnigramLogProb($tok);
      # printf "Prob of orig: %f\n",getUnigramProb($tok);

      my @logProbs;
      foreach my $w (@exp) {
	push(@logProbs,getUnigramLogProb($w));
      }
      printf "Probability: %s = %f vs. %s = %s\n",$tok,$origProb,join(" ",@exp),join(" ",@logProbs) if $TRACE;
      if ($correctionProb > $origProb) {
	print "And it's higher!\n" if $TRACE;
	push(@result,@exp);	# assume it's valid
	next;
      }
      else {
	printf "But it's not higher\n" if $TRACE;
      }
    }
    # All out of tricks. Give up.
    printf STDERR "Nothing worked. Giving up on this token and will let it through!\n" if $TRACE;
    push(@result,$tok);
  }
  my $result = join(" ",@result);
  $result = applySpellFixRegexes($result);
  return $result;
}

sub getUnigramProb {
  my ($word) = @_;
  my $prob = $wordProbs{$word};
  $prob = $singletonProb unless $prob;
  return $prob;
}

sub getUnigramLogProb {
  my @words = @_;
  my $logProb = 0;
  foreach my $word (@words) {
    $logProb += log(getUnigramProb($word));
  }
  return $logProb;
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
  $str =~ s/z/s/ig;
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
    if (lc($str) eq "www") {
      return "www";
    }
    if ($str =~ /^mm{1,}$/i) {
	return "mmm";
    }
    elsif ($str =~ /^ah{1,}$/i) {
	return "ahh";
    }
    else {

	# Three or more copies down to two for all letters
	$str =~ s/([a-z])\1\1+/$1$1/ig;

	# Two down to one for these letters
	$str =~ s/([bchkpquvwxyz])\1+/$1/ig;

	# Two copies of 'o' down to 1 if at word end
	$str =~ s/oo\b/o/ig;

	return $str;
    }
}

# sub fixWord {
#   my ($str) = @_;
#   if ($str =~ /^mmm+$/) {
#     return qw(mmm);
#   }
#   elsif ($str =~ /^ahh+$/) {
#     return qw(ahh);
#   }
#   $str =~ s/azz/ass/g;

#   # Three copies down to two for all letters
#   $str =~ s/([a-z])\1\1+/$1$1/g;

#   # Two down to one
#   $str =~ s/([bcdghnpquvwxyz])\1+/$1/g;
  
#   $str =~ s/oo\b/o/;

#   return $str;
# }


# sub validWord {
#   my ($word) = @_;
#   $validWords{$word} = 1 unless $validWords{$word};
# }

sub setWordCount {
  my ($word,$count) = @_;
  $validWords{$word} = $count;
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


# sub NEWfixUrls {
#   my ($str) = @_;
#   $str =~ s/http[\s:\/]*(.*)((com|net|us|mx))/"http:\/\/" . purgeWhiteSpace($1) . $2/ge;
#   return $str;
# }

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
    # $cap = "NONE" unless $cap;
    # printf "tok: $tok $cap\n";
    if ($cap) {
      $tokens[$i] = $cap;
    }
    elsif ($prev =~ /[\.\?\!]$/ and !($tok =~ /^[A-Z]/)) {
      $tokens[$i] = ucfirst($tok);
    }
  }

  $str = join(" ",@tokens);
  # printf "str: $str\n";
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
    printf STDERR "Not capitalized: $name\n" unless $name =~ /^[A-Z]/;
    capitalize($name);
  }
  close(INPUT);
}

sub readCapsDecisionFile {
    my ($file) = @_;
    printf STDERR "Reading caps decision file $file\n";
    open(INPUT,$file) or die "Can't read $file";
    while (<INPUT>) {
	my ($word,$decision) = split;
	next if $capitalizedWords{$word};  # Don't reset if we have already have an entry128
	# next unless isValidWord($word);
	next if length($word) == 1;  # Don't do single letters
	next if ($word eq "no" or $word eq "outcall");
	my $form = $word;
	if ($decision eq "up") {
	    $form = uc($word);
	    capitalize($form,0);
	}
	elsif ($decision eq "cap") {
	    $form = ucfirst($word);
	    capitalize($form,0);
	}
# 	elsif ($decision eq "low") {
# 	    $form = lc($word);
# 	}
# 	else {
# 	    die "Cap decision for $word is not up,cap, or low: $decision";
# 	}
# 	capitalize($form,0);
    }
    close(INPUT);
}

sub capitalize {
  my ($name,$makeValidWords) = @_;
  $makeValidWords = 1 unless defined($makeValidWords);
  if ($makeValidWords) {
    foreach my $tok (split(' ',$name)) {
      $validWords{lc($tok)}++;
    }
  }
  if ($name =~ /^\S+\s+\S+/) {
    my $regex = "\\b$name\\b";
    $capitalizedRegexes{$regex}=$name;
  } 
  else {
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

sub findWords {
  my @tokens = @_;
  my $counter = 0;
  my @states = (makeState(@_));
  my @successStates;
  while (@states) {
    my $state = pop(@states);
    foreach my $state (expandState($state)) {
      if (isCompleteState($state)) {
	push(@successStates,$state) if $state->{SUCCESS};
      }
      else {
	push(@states,$state);
      }
    }
    $counter++;
  }
  # printf STDERR "# Success states: %d\n",scalar(@successStates);
#   foreach my $state (@successStates) {
#     my $words = $state->{WORDS};
#     printf STDERR "WORDS: %s\n",join(" ",@$words);;
#   }
  my $minLength;
  my $bestState;
  my $maxLogProb;
  foreach my $state (@successStates) {
    my @words = getStateWords($state);

    my $logProb = 0;
    foreach my $word (@words) {
	$logProb += log($wordProbs{$word});
    }
    # printf "words: %s logprob: %f\n",join(" ",@words),$logProb; 
    if (!defined($bestState) or $logProb > $maxLogProb) {
	$bestState = $state;
	$maxLogProb = $logProb;
    }
    # if (!defined($bestState) or scalar(@words) < $minLength) {
    #   $bestState = $state;
    #   $minLength = scalar(@words);
    # }
  }
  if ($bestState) {
    return getStateWords($bestState);
  }
  else {
    return ();
  }
}



###############################################

# Adds the argument word to the spell tree
sub addWordToTree {
  my ($word) = @_;
  my @letters = split(//,$word);
  my $cur = $spellTree;
  foreach my $char (@letters) {
    my $node = $cur->{$char};
    if (!defined($node)) {
      $node = {};
      $cur->{$char} = $node;
    }
    $cur = $node;
  }
  $cur->{WORD} = $word;
}

sub lookupWordInTree {
  my ($word) = @_;
  my @letters = split(//,$word);
  my $cur = $spellTree;
  foreach my $char (@letters) {
    my $node = $cur->{$char};
    if (!defined($node)) {
      return undef();
    }
    $cur = $node;
  }
  return $cur->{WORD};
}



sub makeState {
  my @letters = @_;
  my $state       = {};
  $state->{POS}   = 0;
  $state->{NODE}  = $spellTree;
  $state->{INPUT} = \@letters;
  $state->{WORDS} = [];
  # $state->{NUM}   = $NUM_STATES++;
  return $state;
}

sub copyState {
  my ($state) = @_;
  my $copy = {};
  $copy->{PARENT} = $state;
  $copy->{INPUT}  = $state->{INPUT};
  $copy->{POS}    = $state->{POS};
  $copy->{NODE}   = $state->{NODE};
  # $copy->{NUM}    = $NUM_STATES++;
  my $words = $state->{WORDS};
  die "no words" unless defined($state->{WORDS});
  my @words = @$words;
  $copy->{WORDS} = \@words;
  return $copy;
}

sub getStateWords {
  my ($state) = @_;
  my $words = $state->{WORDS};
  return @$words;
}


sub expandState {
  my ($state) = @_;
  die "Don't try to expand a completed state" if isCompleteState($state);
  my $pos  = $state->{POS};
  my $char = $state->{INPUT}->[$pos];
  # printf STDERR "Expanding state %d pos: %d letter: %s\n",$state->{NUM},$pos,$char;
  my $nextNode = $state->{NODE}->{$char};
  if (!defined($nextNode)) {
    $state->{FAILED} = 1;
    return ();
  }
  else {
    # printf STDERR "Found a node for %s\n",$char;
    my $nextState = copyState($state);
    $nextState->{POS}++;
    $nextState->{NODE} = $nextNode;

    if (defined($nextNode->{WORD})) {
      my $word     = $nextNode->{WORD};
      # printf STDERR "found a word: %s\n",$word;
      push(@{$nextState->{WORDS}},$word);
      if (isCompleteState($nextState)) {
	my $words = $nextState->{WORDS};
	# printf STDERR "and we're complete: %s\n",join(" ",@$words);
	$nextState->{SUCCESS} = 1;
	return ($nextState);
      }
      else {
	# Reset the new state's node back to the tree root
	$nextState->{NODE} = $spellTree;
	# Make an alternative continuation state that will look for a different word than this one
	my $continue = copyState($state);
	$continue->{POS}++;
	$continue->{NODE} = $nextNode;
	return($continue,$nextState);
      }
    }
    else {
      return ($nextState);
    }
  }  
}


sub isCompleteState {
  my ($state) = @_;
  my $length = scalar(@{$state->{INPUT}});
  return ($state->{POS} >= $length);
}

sub buildSpellTree {
  printf STDERR "Building spell tree...\n";
  my $totalWordCount = 0;
  my $singletons = 0;
  foreach my $count (values(%validWords)) {
    $totalWordCount += $count;
    $singletons++ if $count == 1;
  }
  # $singletonProb = $singletons/$totalWordCount;
  $singletonProb = 1/$totalWordCount;
  printf "Total word count: %d\n",$totalWordCount;
  printf "Singleton prob: %s\n",$singletonProb;
  foreach my $word (keys(%validWords)) {
    if (length($word) > 1 or $word eq "a") {
      addWordToTree($word);
    }
    # addWordToTree($word);
    $wordProbs{$word} = $validWords{$word}/$totalWordCount;
  }
  printf STDERR "Done\n";
}
