Program oeisearch
=================

Author: Hugo Pfoertner http://www.pfoertner.org/

This file is stored at
https://github.com/HugoPfoertner/OEIS-Search-GCD-reduced/blob/main/oeisearch_readme.txt

Update 2022-11-23: Project moved to GitHub
Date of initial version: 2020-10-13

Purpose
=======

The program is used to search for the occurrence of lists of integers in the
stripped.gz file provided by the OEIS Foundation.

This basic function corresponds to the normal search function in OEIS, but stops
the search immediately after the first hit.

In addition, two major enhancements have been implemented.

Search with variants of data
----------------------------

The first extension is a multiple search with modified data.
Up to 9 further derived lists are generated from a list of numbers,
which are also compared with the "stripped" database.

First, 4 shifted lists are generated from each input list
by adding the shifts +1, -1, +2, and -2 to each data element.

For each of the now 5 lists it is checked whether all elements of the list
can be divided by a common factor. If this factor is greater than 1,
a derived list is generated for the list in which this applies,
in which all entries are divided by the common factor found.

All shifted and divided lists are used after preprocessing together
with the original list like this as input for the search.

The comparisons with all variants are made immediately one after the other
for each A number searched, in ascending order of the A numbers.
If one of the variants leads to a hit with an earlier A number than the
original search entry, then the search will be terminated with this hit
and the later match with the original data will not be found.

It is possible to force a continuation of the search after a first hit by
appending a single A-number prefixed by "!" after the search items.
Multiple exclusions of A-numbers are not supported.

GCD-reduced version of OEIS
---------------------------

The second extension is a continuation of the search using 3 GCD-reduced
versions of the OEIS data, one for the original data, and two others
for the data first incremented by +1 and by -1 and then GCD-reduced.


Input format and limitations
============================

Valid separators of the numbers are blanks and/or commas.
A comma after the last data item may lead to a non-acceptance
of the program call by the command interpreter.

The input is limited to a maximum of 20 items.

A search for negative numbers is not supported. The occurence of negative numbers
in the input is interpreted as termination of the input before the negative item.

Input items must not exceed 2^63-1. The presence of numbers bigger than this limit
in the input string may lead to a failure abort of the program.

For additional information see also the introductory comment of the source
code of oeisearch.f.


Installation
============

Program
-------

The program and its data files need to be in the same directory.
Symbolic links to the files in Linux systems will also work.

For 64-bit Windows environments, a precompiled executable is available.
It can either be copied directly from

https://github.com/HugoPfoertner/OEIS-Search-GCD-reduced/blob/main/oeisearch.exe

or, if firewalls or other protection measures inhibit a direct download, the
executable is part of a protected zip archive, where the executable
is renamed to oeisearch._exe, to be renamed after unpacking.
The zip archive is available at

https://github.com/HugoPfoertner/OEIS-Search-GCD-reduced/blob/main/oeisearch_wex.zip

The p a s s w o r d needed for unpacking of the archive is O..S,
with an obvious replacement of "..".

If you have access to a Fortran-90 compiler,
oeiseach can also be compiled from its source, which is available either
as part of the archive cited above, or, if the Windows executable is
not needed, from

https://github.com/HugoPfoertner/OEIS-Search-GCD-reduced/blob/main/oeisearch.zip

In non-Windows environments the compilation from the source is mandatory.


Data files
----------

For a full functionality of the oeisearch program, 4 data files are needed
in the directory of the program, either directly or via symbolic links.

The file "stripped" is mandatory. Its current version is available from

https://oeis.org/stripped.gz

It has to be unpacked by a suitable tool, e.g. 7-Zip and many others in Windows,
or gunzip on Linux.
The size of the unpacked file "stripped" is currently (Nov 2022) about 69 MBytes.

To perform the "GCD-reduced" search, the following
3 additional files are needed:

gcd0.txt, gcdminus1.txt, gcdplus1.txt

The files (size ~ 9 MBytes each) may either be downloaded from
https://github.com/HugoPfoertner/OEIS-Search-GCD-reduced/blob/main/gcd0.txt
https://github.com/HugoPfoertner/OEIS-Search-GCD-reduced/blob/main/gcdminus1.txt
https://github.com/HugoPfoertner/OEIS-Search-GCD-reduced/blob/main/gcdplus1.txt

or they can be created by an auxiliary program gcdoeis that uses "stripped"
as input. The stored files are currently only updated on an occasional basis,
whereas creating the files by running gcdoeis together
with a freshly downloaded file stripped.gz guarantees the inclusion of the
latest OEIS-updates.

The source gcdoeis.f of the auxiliary program is part of the archive
oeisearch.zip, but no ececutable is provided.

The source gcdoeis.f needs to be compiled by a Fortran-90 compiler.
The programs were successfully built using gfortran shipped with gcc,
both in a Linux environment and in MinGW64 in Windows.

To create the 3 files, the executable has to be called 3 times:

gcdoeis.exe 0    (creates gcd0.txt and stripped_stats.txt)
gcdoeis.exe -1   (creates gcdminus1.txt)
gcdoeis.exe 1    (creates gcdplus1.txt)

A few additional logfiles named fort.nn, nn=22, 23, 32, 33, 42, 43, and
an information file stripped_stats.txt will be produced by the calls,
but may be discarded.

The file stripped_stats.txt, which shows the number of data items per A-number
and counts of minus signs, is also available at

https://github.com/HugoPfoertner/OEIS-Search-GCD-reduced/blob/main/stripped_stats.txt

The size of the gcd*.txt files is about 8-9 MBytes each, the other information files
are of similar size or less.


Examples
========

1) Nothing to do:

$ oeisearch
STOP At least one input required


2) Missing data file(s):

$ oeisearch 1729
Items: 1
List of inputs for the search
 # add gcd
 1  0   1  items: 1729
 3  1   1  items: 1730
 5 -1   1  items: 1728
 7  2   1  items: 1731
 9 -2   1  items: 1727

STOP file stripped not found


3) File stripped present, obvious result:

$ oeisearch 3 5 7 11 13 17 19 23
Items: 8
List of inputs for the search
 # add gcd
 1  0   1  items: 3 5 7 11 13 17 19 23
 3  1   1  items: 4 6 8 12 14 18 20 24
 4  1   2  items: 2 3 4 6 7 9 10 12
 5 -1   1  items: 2 4 6 10 12 16 18 22
 6 -1   2  items: 1 2 3 5 6 8 9 11
 7  2   1  items: 5 7 9 13 15 19 21 25
 9 -2   1  items: 1 3 5 9 11 15 17 21

 Searching in file stripped ...
Input #1: match found in A000040 at position 2
URL: https://oeis.org/A000040


4) A000040 not wanted:

$ oeisearch 3 5 7 11 13 17 19 23 ! 40
Hitting A000040 will not terminate the search
Items: 8
List of inputs for the search
 # add gcd
 1  0   1  items: 3 5 7 11 13 17 19 23
 3  1   1  items: 4 6 8 12 14 18 20 24
 4  1   2  items: 2 3 4 6 7 9 10 12
 5 -1   1  items: 2 4 6 10 12 16 18 22
 6 -1   2  items: 1 2 3 5 6 8 9 11
 7  2   1  items: 5 7 9 13 15 19 21 25
 9 -2   1  items: 1 3 5 9 11 15 17 21

 Searching in file stripped ...
Input #1: match found in A000040 at position 2
Input #6: match found in A000534 at position 2
URL: https://oeis.org/A000534


5) If no hits in stripped are found, the gcd*.txt files are needed;
running through the complete stripped file typically needs a few seconds.

$ oeisearch 123 321 1234 4321 12345 54321
Items: 6
List of inputs for the search
 # add gcd
 1  0   1  items: 123 321 1234 4321 12345 54321
 3  1   1  items: 124 322 1235 4322 12346 54322
 5 -1   1  items: 122 320 1233 4320 12344 54320
 7  2   1  items: 125 323 1236 4323 12347 54323
 9 -2   1  items: 121 319 1232 4319 12343 54319

 Searching in file stripped ...
 No success in untransformed OEIS
STOP file gcd0.txt not found


6) Data items must not exceed 2^63-1 (~19 digits)

$ oeisearch 139968, 536870912, 5000000000000, 92442129447518208, 2988151979474457198592
STOP Error in input


7) A productive hit in the edit process of A338110; match between
suggested terms, when divided by 64, with terms of another sequence that
has a common factor of 192.

oeisearch 128, 139968, 536870912, 5000000000000, 92442129447518208
Items: 5
List of inputs for the search
 # add gcd
 1  0   1  items: 128 139968 536870912 5000000000000 92442129447518208
 2  0   64  items: 2 2187 8388608 78125000000 1444408272617472
 3  1   1  items: 129 139969 536870913 5000000000001 92442129447518209
 5 -1   1  items: 127 139967 536870911 4999999999999 92442129447518207
 7  2   1  items: 130 139970 536870914 5000000000002 92442129447518210
 8  2   2  items: 65 69985 268435457 2500000000001 46221064723759105
 9 -2   1  items: 126 139966 536870910 4999999999998 92442129447518206
10 -2   2  items: 63 69983 268435455 2499999999999 46221064723759103

 Searching in file stripped ...
 No success in untransformed OEIS
Input #2: match found in A193131 / 192 at position 1
URL: https://oeis.org/A193131
End of search

8) Don Knuth didn't know about the existence of A033472 when submitting A337274;
By specifying the exclusion parameter "! A337274",
the situation at that time is simulated. The hit at A337274 is reported,
but not considered as success. The search is not terminated, thus
proceeding into the gcd* files.

oeisearch 376, 2010, 11788, 77816, 556016, 4366814 ! A337274
Hitting A337274 will not terminate the search
Items: 6
List of inputs for the search
 # add gcd
 1  0   1  items: 376 2010 11788 77816 556016 4366814
 2  0   2  items: 188 1005 5894 38908 278008 2183407
 3  1   1  items: 377 2011 11789 77817 556017 4366815
 5 -1   1  items: 375 2009 11787 77815 556015 4366813
 7  2   1  items: 378 2012 11790 77818 556018 4366816
 8  2   2  items: 189 1006 5895 38909 278009 2183408
 9 -2   1  items: 374 2008 11786 77814 556014 4366812
10 -2   2  items: 187 1004 5893 38907 278007 2183406

 Searching in file stripped ...
Input #1: match found in A337274 at position 8
 No success in untransformed OEIS
Input #2: match found in A033472 / 4 at position 5
URL: https://oeis.org/A033472
End of search


License
=======

Copyright 2020, 2022 Hugo Pfoertner
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

----------------------------------------------------------------------------

End of file oeisearch_readme.txt