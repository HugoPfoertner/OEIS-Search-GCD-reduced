C This is an auxiliary program to support the
C alternative search function for GCD-reduced OEIS.
C The alternative search function is used to supplement,
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
C This program reads the file stripped, to be obtained from OEIS at
C (gunzipped https://oeis.org/stripped.gz)
C and looks for sequences with a common
C factor in terms with 3 to 18 decimal digits
C Dependent on parameter "offset" to be  provided in call, write output
C to files gcd0.txt (offset=0), gcdplus1.txt (offset=1),
C and gcdminus1.txt (offset=-1)
C Copies of these files are uploaded sporadically
C to http://www.randomwalk.de/sequences/
C
C If called with offset=0, an information file stripped_stats.txt
C with counts of commas and minus signs for each A-number is written.
C
C The program has been tested uing the GNU fortran compiler gfortran
C gcc version 7.2.0 under MinGW-W64
C
C
C Version history:
C 2020-10-06 Extendend comment, license information
C 2020-10-02 Include data with a single minus
C 2020-09-28 Output to named files
C 2020-09-26 Initial version
      integer c(500), m(500)
      integer*8 a(500), b(500), z(500), gcd, ir, plus, maxb
      real*16 r(500)
      character line*500, head*50
      call get_command_argument ( 1, head)
      read (head,*) plus
      if ( abs(plus) .gt. 1 ) stop 'Illegal offset'
C Channel numbers for output files
      if ( plus .eq. 0 ) then
        i10 = 20
        open ( unit=24, file='gcd0.txt', form='formatted',
     &         status='unknown', iostat=ios )
        if ( ios .ne. 0 ) stop 'Error opening file gcd0.txt'
        open ( unit=21, file='stripped_stats.txt', form='formatted',
     &         status='unknown', iostat=ios )
        if ( ios .ne. 0 ) stop 'Error opening file stripped_stats.txt'
      elseif ( plus .eq. 1 ) then
        i10 = 30
        open ( unit=34, file='gcdplus1.txt', form='formatted',
     &         status='unknown', iostat=ios )
        if ( ios .ne. 0 ) stop 'Error opening file gcdplus1.txt'
      else
        i10 = 40
        open ( unit=44, file='gcdminus1.txt', form='formatted',
     &         status='unknown', iostat=ios )
        if ( ios .ne. 0 ) stop 'Error opening file gcdminus1.txt'
      endif
      open (unit=10,file='stripped',form='formatted',
     &      status='old',iostat=ios)
      if ( ios .ne. 0 ) stop 'file stripped not found'
      read (10,1000,end=20) line
      read (10,1000,end=20) line
      lhead = len_trim(line)
      head = line(1:lhead)
      if ( plus .eq. 0 ) write (21,1000) trim(head)
      write (i10+4,1000) trim(head)
      write (*,*) 'Version info of file stripped'
      write (*,*) trim(head)
C start loop over lines of file "stripped"
10    continue
      read (10,1000,end=20) line
1000  format ( A )
      if ( line(1:1) .ne. 'A' ) goto 10
      read (line(2:7),1001) na
C      if ( na .gt. 100 ) stop
1001  format ( i6 )
      L = len_trim(line)
C Determine number of items in DATA by counting commas
      k = 0
      do 30 i = 1, L
      if ( line(i:i) .eq. ',' ) then
        k = k + 1
        c(k) = i
      endif
30    continue
C Determine if DATA has negative numbers
      j = 0
      do 40 i = 1, L
      if ( line(i:i) .eq. '-' ) then
        j = j + 1
        m(j) = i
      endif
40    continue
C Diagnostic output for all DATA lines only written for
C call with PLUS=0
      if (plus .eq. 0 ) write (21,1002) na, k, j
1002  format (i6.6, i4, i4)
C
C check if DATA has negative numbers
      if ( j .gt. 0 ) then
        if ( j.eq. 1 ) then
C A single occurrence will be included by converting
C the affected term to a positive value.
C There are a few cases where the single negative numbers
C is in the middle of the sequence, but many important
C sequences with an initial negative term would otherwise
C be discarded.
          line(m(1):m(1)) = '+'
        else
          goto 10
        endif
      endif
C
      n = 0
      do 110 i = k, 2, -1
      nd = c(i)-c(i-1)
C no special treatment of potential occurence of the
C single negative term inside the sequence
C
C nd >=4 is a deliberate choice to include only terms >= 100
C in identifying candidates for GCD > 1.
C
      if ( nd .le. 19 .and. nd .ge. 4 ) then
        n = n + 1
        read ( line(c(i-1)+1:c(i)-1),* ) a(n)
        a(n) = a(n) + plus
      endif
110   continue
      write (i10+2,1022) na, n, a(n), a(1)
1022  format ( i6.6, i4, 1x, i0, 1x, i0 )
C GCD
      if ( n .gt. 1 ) then
        call gcdn ( n, a, z, gcd )
        if ( gcd .gt. 1 ) then
          write (i10+3, 1023) na, n, gcd
1023      format ( i6.6, i4, 1x, i0 )
C reread items in DATA line as 16 byte float
          read (line(10:),*) r(1:k-1)
          r(1:k-1) = (r(1:k-1) + plus) / gcd
C          write (*,*) r(1:k-1)
          mm = 0
          maxb = 0
          do 200 i =  k-1, 1, -1
C Exclude results > 2^63 after division.
C It is accepted that this might lead to the omission of terms
C that are local maxima > 2^63 surrounded by smaller terms
          if ( r(i) .gt. 9.223372D18 ) goto 200
C terminate list at lower end if division result is < 1.
          if ( r(i) .lt. 0.9999 ) goto 210
          ir = nint ( r(i), kind=8 )
          if ( abs(r(i)-ir) .lt. 1.0D-20 ) then
            mm = mm + 1
            b(mm) = ir
            maxb = max ( maxb, ir )
          else
            goto 210
          endif
200       continue
210       continue
C At least 4 consecutive terms are required for inclusion in output.
C "All one" results are also excluded.
          if ( mm .gt. 3 .and. maxb .gt. 1 )
     &         write (i10+4,1034) na, gcd, mm, (b(i),i=mm,1,-1)
1034      format ( i6.6,1X,i0,1x,i0,150(1x,i0,:))
        endif
      endif
C
C Skip back to processing of next line in file "stripped"
      goto 10
C Target label for termination
20    continue
C End of main program
      end
C ---------------------------------------------------------------------
C modified version of http://netlib.org/toms/386.gz
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
