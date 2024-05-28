C Alternative search function for GCD-reduced OEIS.
C This is an alternative search function that is used to supplement,
C but not replace, the query implemented in the database.
C
C Author: Hugo Pfoertner http://www.pfoertner.org/
C ----------------------------------------------------------------------
C Copyright 2020 Hugo Pfoertner
C
C Licensed under the Apache License, Version 2.0 (the "License");
C you may not use this file except in compliance with the License.
C You may obtain a copy of the License at
C
C    http://www.apache.org/licenses/LICENSE-2.0
C
C    Unless required by applicable law or agreed to in writing, software
C    distributed under the License is distributed on an "AS IS" BASIS,
C    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
C    See the License for the specific language governing permissions and
C    limitations under the License.
C
C The license is not applicable to the attached source code of
C function gcdn, which is distributed via netlib.org
C http://netlib.org/toms/386.gz
C without any restrictions on its use.
C ----------------------------------------------------------------------
C
C Combines search in file "stripped" and in 3 derived
C files containing OEIS data divided by the GCD of the
C larger terms, also for data "shifted" by +1 and -1.
C
C The search string of numbers provided in the command line
C is checked for a common factor. If this
C GCD is > 1, the search is repeated with the divided input.
C The same check is applied to the input shifted by offsets +-1 and +-2.
C
C Thus up to 10 different inputs are created for the comparison.
C
C Search for negative numbers is currently not implemented.
C Sequences with keyword sign are excluded from the
C GCD-reduced files.
C
C The first hit terminates the search.
C
C 4 input files are needed and must be in the same directory
C as the search program.
C
C shifted (gunzipped https://oeis.org/stripped.gz)
C gcd0.txt, download from http://www.randomwalk.de/sequences/gcd0.txt
C gcdplus1.txt from http://www.randomwalk.de/sequences/gcdplus1.txt
C gcdminus1.txt from http://www.randomwalk.de/sequences/gcdminus1.txt
C At the moment, the last 3 files are only sporadically updated.
C
C The untransformed input is first searched in file "stripped",
C which will usually produce one hit that is identical to one of the
C results of the normal OEIS search function.
C In the case that the search input in its untransformed form
C is present in OEIS, but is divisible by a common factor,
C an "earlier" than expected hit may occur,
C if one of the tranformed variants produces a
C match in a sequence with lower A-number than the sequence where
C the "direct" hit would occur.
C
C The input list may be extended at its end by an A-number.
C A hit for this A-number will not terminate the search.
C The format for this optional input is !Annn or !nnn.
C leading zeros need not be provided in this input,
C e.g., !40, !000040, and !A0040 are alle equivalent.
C
C
C Version history:
C 2024-05-19 Write information of successful call to file and
C            re-use it for exclusion without specified A-number
C 2020-10-02 Optional input of excluded sequence A-number
C 2020-10-01 Improved comments
C 2020-09-28 Revised output
C 2020-09-26 Initial version
C
C Undeclared variables assume Fortran's implicit type default,
C i.e., variables with first letter (i..n) are 4 byte integers.
C
C
C
C lcline: length of command line
C m: maximum items in search list
C 
      parameter (lcline=160,m=21)
C noff: maximum number of variants of modified search
      parameter (noff=10)
C Search list input: s, copy of search list with offsets
C and gcd-reduction: w.
C activation switches: v
C 8 byte integers:
      integer*8 s(m), t(500), offset(noff), z(500), gcd(noff), gcda
     &          , w(m,noff), timprv, timnow
C 16 byte floats to read data from file "stripped"
      real*16 r(500), rvalue
C Lines in file "stripped" are assumed not to exceed 500 characters
      character cline*(lcline), aline*500, c*1, prefil*23
C Type of comparison functions
      logical matchr, matchi, v(noff), pexist, axclud
C Source code of comparison functions at end of this file
      external matchr, matchi
C List of offsets used as addends to input items
      data offset / 0,0, 1,1, -1,-1, 2,2, -2,-2 /
C Name of file with information from the preceding call
      prefil = 'prev_call_oeisearch.txt'
C Preset activation switches to "only items as supplied"
      v = .false.
      v(1) = .true.
C Get full command line
      call get_command (cline, lcmd )
C For debugging
C report length of command line and write echo
C      write (*,*) 'L=', lcmd
C      write (*,*) trim(cline)
C
C extract numbers from input line
C Assumes that name of program and path contain no blanks
C so that search items start after first blank
      nblank = index(cline,' ')
      nstart = nblank
C remove a potentially disturbing trailing comma
C at end of input line.
C This does not work in the windows Power Shell.
C Therefore the search input must not be terminated
C by a trailing comma.
      ncomma = index(cline,',',.true.)
      if ( ncomma .ne. 0 ) cline(ncomma:ncomma) = ' '
C
C write termination character to rear end of line to
C enable reading by Fortran's list directed input
      cline (lcline:lcline) = '/'
C extract optional excluded A-number
      i = index(cline, '!')
      naex = 0
      axclud = .false.
      if ( i .ne. 0 ) then
C check whether ! is the last character in input
        if (len_trim(cline(1:lcline-1)) .eq. i ) then
C try to read A-number found in previous call
          timnow = time8()
          inquire (file=prefil, exist=pexist)
          if (pexist) then
            open (unit=9, file=prefil, form='formatted', status='old')
            read (9,*) timprv, nax
            close (unit=9,status='delete')
            if (timnow-timprv .lt. 120) then
              naex = nax
              axclud = .true.
              ios = 0
            endif
          endif
        else
          j = max(1,index(cline(i:), 'A' ))
          read ( cline(i+j:),*,iostat=ios) naex
        endif
C replace "!" by another termination character to stop reading
C of search item list at this point
        cline(i:i) = '/'
        if ( ios .ne. 0 ) then
          write (*,*)
     & 'unreadable A-number, continue without exclusion'
        else
C silently limit A-number to plausible range
          if ( naex .lt. 1 .or. naex .gt. 999999 ) then
            naex = 0
          else
            write (*,1003) naex
1003        format ( 'Hitting A', I6.6,
     &               ' will not terminate the search')
          endif
        endif
      endif
C preset terms:
C very simplistic: no negative inputs allowed.
C if negative inputs are provided, the first
C negative number will act as a terminator of the
C item list and will be ignored.
      s = -1
      read ( cline(nstart:),*,end=2,err=1 ) s
      goto 2
1     continue
      stop 'Error in input'
2     continue
      n = -1
      if ( s(1) .lt. 0 ) stop 'At least one input required'
      do 10 i = 1, m
      if ( s(i) .lt. 0 ) then
        n = i - 1
        goto 20
      endif
10    continue
      stop 'Too many numbers'
20    continue
      write (*,'(A,i0)') 'Items: ', n
C      write (*,*) s(1:n)
C Check for GCD reduction only if more than one search item is provided
      if ( n .gt. 1 ) then
C Loop over offsets
        v = .true.
        gcd = 1
        do 5 nof = 1, 9, 2
        t(1:n) = s(1:n) + offset(nof)
        w(1:n,nof) = t(1:n)
        call gcdn ( n, t, z, gcd(nof+1) )
C        write (*,*) 'gcd:', gcd
C Divided input variant is only activated if GCD (search items) > 1
        if ( gcd(nof+1) .gt. 1 ) then
          w(1:n,nof+1) = (s(1:n) + offset(nof)) / gcd(nof+1)
        else
C GCD = 1
          v(nof+1) = .false.
        endif
5       continue
      else
C If only a single number is provided, the +-1 and +-2
C variants are activated
        gcd = 1
        w(1,1) = s(1)
        v(3) = .true.
        w(1,3) = s(1) + 1
        v(5) = .true.
        w(1,5) = s(1) - 1
        v(7) = .true.
        w(1,7) = s(1) + 2
        v(9) = .true.
        w(1,9) = s(1) - 2
      endif
C Write summary of searched variants
      write (*,1001)
1001  format ('List of inputs for the search',/,' # add gcd')
      do 6 nof = 1, noff
      if ( v(nof) )
     &  write (*,1006) nof, offset(nof), gcd(nof), w(1:n,nof)
1006  format ( i2, i3, 3X, i0, '  items:', 21(1x,i0,:) )
6     continue
C
C End of input processing
C
      write (*,*) ' '
C      if ( s(1) .gt. 0 ) stop
C
C Start of search in file "stripped"
      open (unit=10,file='stripped',form='formatted',
     &      status='old',iostat=ios)
      if ( ios .ne. 0 ) stop 'file stripped not found'
1000  format ( A )
C skip header lines in file stripped
      read (10,1000) c
      read (10,1000) c
      read (10,1000) c
      read (10,1000) c
C get largest representable number to preset terms
C > 10^4000 for 16 byte floats, so no risk of interfering
C with any DATA item
      rvalue = 1.0D0
      rvalue = huge(rvalue)
C This dummy write inibits segfault in Windows executable
C not understood
      write (cline,*) 'Huge:', rvalue
      write (*,*) 'Searching in file stripped ...'
100   continue
      read (10,1000,end=200) aline
      read (aline,'(1X,i6.6)') numa
c      if ( numa .gt. 2) stop
      if (axclud .and. numa .le. naex) goto 100
      lastc = index ( aline, ',', .true. )
      aline(lastc:lastc) = '/'
C preset terms before list directed read
      r = rvalue
      read (aline(10:lastc),*) r
C Loop over modified inputs
      do 101 nof = 1, noff
      if ( v(nof) ) then
C call comparison function
      if ( matchr(r, n, w(1,nof), rvalue, nrel) ) then
C        write (*,1006) nof, offset(nof), gcd(nof), w(1:n,nof)
        write (*,1100) nof, numa, nrel
1100    format ( 'Input #', i0,': match found in A', i6.6,
     &   ' at position ', i0 )
C Termination after first match
        if (numa .ne. naex) goto 900
      endif
      endif
101   continue
      goto 100
200   continue
C
      write (*,*) 'No success in untransformed OEIS'
      close (unit=10)
C
C Continuation of search in GCD-reduced DATA
C
      open ( unit=10, file='gcd0.txt', form='formatted',
     &       status='old', iostat=ios )
      if ( ios .ne. 0 ) stop 'file gcd0.txt not found'
C skip header line
      read (10,1000) c
210   continue
C Format of each line:
C numa: A-number without (A),
C gcda: GCD by which the DATA have been divided
C j: number of items in stored list, potentially excluding
C    small terms at sequence start and terms at end of
C    sequence exceeding 2^63-1 after division by gcda
C t: list of divided terms
      read (10,1000,end=290) aline
      read (aline,*,end=211,err=211) numa, gcda, j, (t(k),k=1,j)
211   continue
      if (axclud .and. numa .le. naex) goto 210
C      write (*,*) numa, gcda, jj, (t(k),k=1,j)
      do 201 nof = 1, noff
      if ( v(nof) ) then
      if ( matchi (j,t,n,w(1,nof),nrel) ) then
C        write (*,1006) nof, offset(nof), gcd(nof), w(1:n,nof)
        write (*,1200) nof, numa, gcda, nrel
1200    format ( 'Input #', i0,': match found in A', i6.6,
     &           ' / ',i0, ' at position ', i0 )
        if (numa .ne. naex) goto 900
      endif
      endif
201   continue
      goto 210
290   continue
      write (*,*) 'No success in GCD-reduced OEIS'
      close (unit=10)
C
C continue search using GCD-reduced numbers obtained by application
C of offset +1 to all OEIS DATA
C
      open ( unit=10, file='gcdplus1.txt', form='formatted',
     &       status='old', iostat=ios )
      if ( ios .ne. 0 ) stop 'file gcdplus1.txt not found'
C skip header line
      read (10,1000) c
310   continue
      read (10,1000,end=390) aline
      read (aline,*,end=311,err=311) numa, gcda, j, (t(k),k=1,j)
311   continue
      if (axclud .and. numa .le. naex) goto 310
c      write (99,'(i6.6, 2(1x,i0))') numa,gcda,j
C      write (*,*) numa, gcda, jj, (t(k),k=1,j)
      do 301 nof = 1, noff
      if ( v(nof) ) then
      if ( matchi (j,t,n,w(1,nof),nrel) ) then
C        write (*,1006) nof, offset(nof), gcd(nof), w(1:n,nof)
        write (*,1300) nof, numa, gcda, nrel
1300    format ( 'Input #', i0,': match found in (A', i6.6,
     &           ' + 1) / ',i0, ' at position ', i0 )
        if (numa .ne. naex) goto 900
      endif
      endif
301   continue
      goto 310
390   continue
      write (*,*) 'No success in GCD-reduced OEIS + 1'
      close (unit=10)
C
C Search in GCD-reduced "OEIS-1" DATA
C
      open ( unit=10, file='gcdminus1.txt', form='formatted',
     &       status='old', iostat=ios )
      if ( ios .ne. 0 ) stop 'file gcdminus1.txt not found'
C skip header line
      read (10,1000) c
410   continue
      read (10,1000,end=490) aline
      read (aline,*,end=411,err=411) numa, gcda, j, (t(k),k=1,j)
411   continue
      if (axclud .and. numa .le. naex) goto 410
c      write (99,'(i6.6, 2(1x,i0))') numa,gcda,j
      do 401 nof = 1, noff
      if ( v(nof) ) then
      if ( matchi (j,t,n,w(1,nof),nrel) ) then
C        write (*,1006) nof, offset(nof), gcd(nof), w(1:n,nof)
        write (*,1400) nof, numa, gcda, nrel
1400    format ( 'Input #', i0, ': match found in (A', i6.6,
     &           ' - 1) / ',i0, ' at position ', i0 )
        if (numa .ne. naex) goto 900
      endif
      endif
401   continue
      goto 410
490   continue
      write (*,*) 'No success in GCD-reduced OEIS - 1'
      close (unit=10)
      numa = 0
900   continue
      if ( numa .gt. 0 .and. numa .ne. naex ) then
        write (*,1900) numa
1900    format ( 'URL: https://oeis.org/A', i6.6, /, 'End of search' )
C Write information on success to file
        open (unit=9, file=prefil, form='formatted', status='unknown')
        write (9,*) time8(), numa
      endif
C End of main program
      end
C
C ----------------------------
C
      logical function matchr ( r, n, s, rvalue, nrel )
C Find contiguous match of search vector s in original DATA
C vector R.
      real*16 r(*), rvalue, dr
      integer*8 s(n)
C Difference between 8 byte integer and 16 byte float will
C be exact for the possible range of search values.
      parameter (dr=5.0D-1)
      nrel = 0
C Empirical limit: not more than 120 data items in single line of
C file "stripped"
      do 110 i = 1, 120
      if ( r(i) .eq. rvalue ) then
C signal no success if loop reaches empty part of vector
        matchr = .false.
        return
      endif
C Check if first searched item is present in list
      if ( abs(r(i) - s(1)) .lt. dr ) then
C Check if remaining searched items are at the
C subsequent positions of the list
        do 120 j = 2, n
C first non-match exits from inner loop
        if (abs(r(i+j-1) - s(j)) .gt. dr ) goto 110
120     continue
        nrel = i
        matchr = .true.
        return
      endif
110   continue
C End of function matchr
      end
C
C ----------------------------
C
      logical function matchi ( m, t, n, s, nrel )
C Find contiguous match of search vector s in
C integer vector t.
      integer*8 t(m), s(n)
      nrel = 0
      matchi = .false.
      do 110 i = 1, m
C Check if first searched item is present in list
      if ( t(i) .eq. s(1) ) then
C Check if remaining searched items are at the
C subsequent positions of the list
        if ( i+n .gt. m+1 ) return
        do 120 j = 2, n
C first non-match exits from inner loop
        if ( t(i+j-1) .ne. s(j) ) goto 110
120     continue
        nrel = i
        matchi = .true.
        return
      endif
110   continue
C End of function matchi
      end
C ---------------------------------------------------------------------
C modified version of  http://netlib.org/toms/386.gz
C     ALGORITHM 386 COLLECTED ALGORITHMS FROM ACM.
C     ALGORITHM APPEARED IN COMM. ACM, VOL. 13, NO. 07,
C     P. 447.
      SUBROUTINE GCDN  (N,A,Z,IGCD)
C   N     NUMBER OF INTEGERS
C   A(I)  INPUT ARRAY OF N INTEGERS, A(I) IS USED AS WORKING STORAGE,
C         INPUT IS DESTROYED
C   Z(I)  OUTPUT ARRAY OF N MULTIPLIERS
C   IGCD  OUTPUT, GREATEST COMMON DIVISOR OF THE A(I) INTEGERS
C
C modified types: 8 byte integers instead of default 4 byte integers
C static dimension of A and Z may be overridden,
C left untouched for consistency with published version, but 50 -> 120.
C H. Pfoertner, Sept 2020, tested to work with longer type.
C
      integer*8 A(120),Z(120),igcd
      INTEGER*8 C1,C2,Y1,Y2,Q
C FIND FIRST NON-ZERO INTEGER
      DO 1 M = 1,N
      IF(A(M) .NE.0) GO TO 3
    1 Z(M)=0
C ALL ZERO INPUT RESULTS IN ZERO GCD AND ALL ZERO MULTIPLIERS
      IGCD=0
      RETURN
C IF LAST NUMBER IS THE ONLY NON-ZERO NUMBER, EXIT IMMEDIATELY
    3 IF(M.NE.N) GO TO 4
      IGCD=A(M)
      Z(M)=1
      RETURN
    4 MP1= M+1
      MP2= M+2
C CHECK THE SIGN OF A(M)
      ISIG=0
      IF(A(M).GE.0) GO TO 5
      ISIG=1
      A(M)=-A(M)
C CALCULATE GCD VIA N-1 APPLICATIONS OF THE GCD ALGORITHM FOR TWO
C INTEGERS. SAVE THE MULTIPLIERS.
    5 C1= A(M)
      DO 30 I=MP1,N
      IF(A(I).NE.0) GO TO 7
      A(I) = 1
      Z(I)= 0
      GO TO 25
    7 Y1=1
      Y2=0
      C2=ABS(A(I))
   10 Q= C2/C1
      C2= C2-Q*C1
C TESTING BEFORE COMPUTING Y2 AND BEFORE COMPUTING Y1 BELOW SAVES N-1
C ADDITIONS AND N-1 MULTIPLICATIONS.
      IF(C2.EQ.0) GO TO 20
      Y2= Y2-Q*Y1
      Q =C1/C2
      C1 = C1- Q*C2
      IF(C1.EQ.0) GO TO 15
      Y1 = Y1 - Q*Y2
      GO TO 10
   15 C1 = C2
      Y1 = Y2
   20 Z(I)= (C1 - Y1*A(M))/A(I)
      A(I)=Y1
      A(M)=C1
C TERMINATE GCD CALCULATIONS IF GCD EQUALS ONE.
   25 IF(C1.EQ.1) GO TO 60
   30 CONTINUE
   40 IGCD=A(M)
C CALCULATE MULTIPLIERS
      DO 50 J= MP2,I
      K = I-J+2
      KK=K+1
      Z(K)=Z(K)*A(KK)
   50 A(K)=A(K)*A(KK)
      Z(M)=A(MP1)
      IF(ISIG.EQ.0) GO TO 100
      Z(M)=-Z(M)
  100 RETURN
C GCD FOUND, SET REMAINDER OF THE MULTIPLIERS EQUAL TO ZERO.
   60 IP1= I+1
      DO 65 J= IP1,N
   65 Z(J) =0
      GO TO 40
      END
