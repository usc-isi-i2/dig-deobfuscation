dig-deobfuscation
===========

repository for DIG deobfuscation

primary contributor: David Stallard <stallard@isi.edu>

To invoke the deobfuscator on specific input and output files, use:

 perl deobfuscate.pl inputFile outputFile -datafiles datafiles -keymode

Arguments:
  
   inputFile   Input file of sentences, one sentence per line
   outputFile  Output file of deobfuscated sentences, one sentence per line
  -datafiles   The directory containing the spellfixes.txt and capitalize.txt files.
	       We use 'datafiles' as the name of the directory here.
  -keymode     Lines begin with a key (i.e. a URI), which is
   	       separated by a tab from the sentence itself.

To invoke the deobfuscator in streaming mode, i.e. reading form stdin
and writing to stdout, simply omit the inputFile and outputFile
arguments.

To invoke the deobfuscator as a standalone script without external
data files, you must first make a standalone verison. To do this,
invoke:

 perl make_deobfuscator.pl deobfuscate.pl standalone_deobfuscator.pl -datafiles datafiles

I've used 'standalone_deobfuscator' as the output file name here;
you may use another name, provided it is not "deobfuscate.pl".

To invoke this script, use the form:

 perl standalone_deobfuscator.pl -keymode

Don't forget the -keymode argument!












