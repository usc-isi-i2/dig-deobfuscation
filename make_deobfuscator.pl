use strict;
use warnings;
use Getopt::Long;

my $usage = "Usage: inputScriptFile outputScriptFile [-spellfixes spellfixFile] [-capitalize capitalizationsFile] [-datafiles dir]\n";

my $inputScript;
my $outputScript;
my $spellfixes;
my $capitalize;
my $datafilesDir;
my $vocabFile;

GetOptions("spellfixes:s" => \$spellfixes,
	   "capitalize:s" => \$capitalize,
	   "datafiles:s" => \$datafilesDir);

$inputScript = shift;
$outputScript = shift;


die $usage unless $outputScript;


if (!$spellfixes or !$capitalize) {
  die "Must have '-datafiles dir' argument unless both -spellfixes and -capitalize arguments are provided\n$usage" unless $datafilesDir;
  $spellfixes = "$datafilesDir/spellfixes.txt";
  $capitalize = "$datafilesDir/capitalize.txt";
}

$vocabFile = "$datafilesDir/valid-vocab.word-counts";

die $usage unless $outputScript and $spellfixes and $capitalize;



my @spellFixFiles = split(/\,/,$spellfixes);
my @capitalizeFiles = split(/\,/,$capitalize);

my @allInputFiles = ($inputScript,@spellFixFiles,@capitalizeFiles);

die "Trying to overwrite input script: $inputScript\n" if ($outputScript ~~ @allInputFiles);

my $spellFixesDone = 0;
my $capitalizeDone = 0;
my $wordsDone      = 0;

#######################################


open(INPUT,$inputScript) or die "Can't read $inputScript";
open(OUTPUT,">$outputScript") or die "Can't write $outputScript";

while (<INPUT>) {
  printf OUTPUT "%s",$_;
  # Must be EXACT match!
  if (/^\# \+\+SPELLFIXES\+\+/) {
    printf OUTPUT "\n";
    foreach my $file (@spellFixFiles) {
      foreach my $line (readFile($file)) {
	$line =~ s/'/\\'/g;
	printf OUTPUT "spellfix('%s');\n",$line;
      }
    }
    $spellFixesDone = 1;
  }
  elsif (/^# \+\+CAPITALIZE\+\+/) {
    printf OUTPUT "\n";
    foreach my $file (@capitalizeFiles) {
      foreach my $line (readFile($file)) {
	printf OUTPUT "capitalize(\"%s\");\n",$line;
      }
    }
    $capitalizeDone = 1;
  }
  elsif (/^# \+\+VOCAB\+\+/) {
    printf OUTPUT "\n";
    foreach my $line (readFile($vocabFile)) {
      my ($word,$count) = split(' ',$line);
      printf OUTPUT "setWordCount(\"%s\",%d);\n",$word,$count;
    }
    $wordsDone = 1;
  }
    
}
close(INPUT);
close(OUTPUT);

die "Did not find '# ++SPELLFIXES++' in input script $inputScript" unless $spellFixesDone;
die "Did not find '# ++CAPITALIZE++' in input script $inputScript" unless $capitalizeDone;
die "Did not find '# ++VOCAB++' in input script $inputScript" unless $wordsDone;
################################################

sub readFile {
  my ($file) = @_;
  printf "Reading $file\n";
  my @lines;
  open(FILE,$file) or die "Can't read $file";
  while (<FILE>) {
    s/^\s+//;
    s/\s+$//;
    next unless $_;
    next if /^\#/;
    push(@lines,$_);
  }
  close(FILE);
  return @lines;
}



