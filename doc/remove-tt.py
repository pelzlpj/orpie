#!/usr/bin/env python
# Strip out all the \texttt{} occurrences (not perfect, but good enough)
import re, sys

infilename  = sys.argv[1]
outfilename = sys.argv[2]

infile = open(infilename, "r").read()

tt_regex         = re.compile("\\\\texttt\{([^}]+)}")

def tt_replace(m):
   return m.group(1) 

replaced_string = re.sub(tt_regex, tt_replace, infile)

outfile = open(outfilename, "w")
outfile.write(replaced_string)
outfile.close()

# arch-tag: DO_NOT_CHANGE_97d4155d-0a4e-42ae-ae0a-0d064c1a7b71 
