C Tests for search extended to constants
C Number of conversions
      parameter (ndmax=21, mf=13, lcline=160, laline=500)
      real*16 q, one, two, three, half, othird, pi, e
      character c*50, eform*10, eout*35, T(mf)*7
      data t /'1*c', '2*c   ', 'c/2   ', '3*c   ', 'c/3   ',
     & 'c^2   ', 'c^(1/2)',
     & '1/c   ', 'Pi*c  ', 'e*c   ', 'log(c)', 'exp(c)','Cl(n)-c'/
      integer w(ndmax,mf)
C 16 byte floats to read data from file "stripped"
      real*16 r(500), rvalue
C Lines in file "stripped" are assumed not to exceed 500 characters
      character cline*(lcline), aline*(laline)
      logical matchr
      external matchr
C
      zero = real(0,16)
      one = real(1,16)
      two = real(2,16)
      three = real(3,16)
      half = one/two
      othird = one/three
      pi = (two+two)*atan(one)
      e = exp(one)
      naex = 0
10    continue
C Get full command line
      call get_command (cline, lcmd )
C
C count of significant nonzero digits
C Position of decimal point (DP).
C Use backward search to exclude occurrences of "." in program name
      ndp = index(cline,'.',.true.)
C first blank right of DP
      nb1 = index(cline(ndp:),' ') + ndp - 1
C
C write termination character to rear end of line to
C enable reading by Fortran's list directed input
      cline (lcline:lcline) = '/'
C extract optional excluded A-number
C check for character starting A number exclusion
      nex = index(cline,'!')
      if (nex .gt. 0) nb1 = min(nb1, nex)
      naex = 0
      if ( nex .ne. 0 ) then
        j = max(1,index(cline(nex:), 'A' ))
        read ( cline(nex+j:),*,iostat=ios) naex
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
C
C Position of last blank before DP
      nb0 = index(cline(1:ndp-1),' ',.true.)
      if (nb0 .eq. 0) stop 'Missing separator before number'
C first nonzero digit left of decimal point
      nd0 = -1
      do 13 k = nb0+1, ndp-1
      if ( cline(k:k) .ne. '0' ) then
        nd0 = k
        goto 14
      endif
13    continue
14    continue
      if (nd0 .ne. -1) then
        nsig = nb1 - nd0 - 1
      else
C no significant digits left of DP
C first nonzero digit after DP
        nz1 = nb1
        do 15 k = ndp+1, nb1-1
          if (cline(k:k) .ne. '0') then
            nz1 = k
            goto 16
          endif
15      continue
16      continue
        nsig = nb1 - nz1
        if (nsig .eq. 0) stop 'Only digits zero found'
      endif
C
C read floating point number
      read (cline(nb0:nb1-1),*) q
C
C truncate input to 20 significant digits
      nsig = min (nsig, ndmax)
C Variable format from which only the mantissa part is used
C The E-format always produces a mantisse of the form 0.xxxxx...
      eform='(e30.xxe4)'
      write(eform(6:7),'(i2.2)') nsig
      write (eout,fmt=eform) q
      call wrivec(eout,nv,w(1,1))
      write (eout,fmt=eform) two*q
      call wrivec(eout,nv,w(1,2))
      write (eout,fmt=eform) half*q
      call wrivec(eout,nv,w(1,3))
      write (eout,fmt=eform) three*q
      call wrivec(eout,nv,w(1,4))
      write (eout,fmt=eform) othird*q
      call wrivec(eout,nv,w(1,5))
      write (eout,fmt=eform) q*q
      call wrivec(eout,nv,w(1,6))
      write (eout,fmt=eform) sqrt(q)
      call wrivec(eout,nv,w(1,7))
      write (eout,fmt=eform) one/q
      call wrivec(eout,nv,w(1,8))
      write (eout,fmt=eform) pi*q
      call wrivec(eout,nv,w(1,9))
      write (eout,fmt=eform) e*q
      call wrivec(eout,nv,w(1,10))
      write (eout,fmt=eform) exp(q)
      call wrivec(eout,nv,w(1,11))
      write (eout,fmt=eform) log(q)
      call wrivec(eout,nv,w(1,12))
      write (eout,fmt=eform) ceiling(q)-q
      call wrivec(eout,nv,w(1,13))
C
C Start of search in file "stripped"
      open (unit=10,file='stripped',form='formatted',
     &      status='old',iostat=ios)
      if ( ios .ne. 0 ) stop 'file stripped not found'
C skip header lines in file stripped
1000  format (A)
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
      lastc = index ( aline, ',', .true. )
      aline(lastc:lastc) = '/'
C preset terms before list directed read
      r = rvalue
      read (aline(10:lastc),*) r
C
C Loop over transformed inputs
      do 101 nof = 1, mf
C call comparison function
      if ( matchr(r, nv, w(1,nof), rvalue, nrel) ) then
C        write (*,1006) nof, offset(nof), gcd(nof), w(1:n,nof)
        write (eout(1:nv),1101) w(1:nv,nof)
1101    format(21 (i1,:))
        write (*,1100) T(nof), eout(1:nv), numa, nrel
1100    format ( 'Input ', A,': match ', A,' found in A', i6.6,
     &   ' at position ', i0 )
C Termination after first match
        if (numa .ne. naex) goto 900
      endif
101   continue
      goto 100
200   continue
C
      call cpu_time (etime)
      write (*,1099) etime
1099  format ('No success; search time', F6.2, ' s')
      goto 999
900   continue
      if ( numa .gt. 0 .and. numa .ne. naex ) then
        call cpu_time (etime)
        write (*,1900) numa, etime
1900    format ( 'URL: https://oeis.org/A', i6.6,
     &           /, 'End of search; time', F6.2, ' s' )
      endif
999   continue
      close (unit=10)
      end
C
      subroutine wrivec(eout, nw, d)
      integer nw
      character*(*) eout
      integer d(*)
      idp = index(eout,'.')
      iex = index(eout,'E')-2
C      write (*,*) idp, iex, eout
      k = 0
      do 10 i = idp+1, iex
      k = k + 1
      read(eout(i:i),'(i1)') d(k)
10    continue
      nw = k
C      write (*,1000) d(1:k)
C1000  format (i0, 19(',',i0,:))
      end
C
      logical function matchr ( r, n, s, rvalue, nrel )
C Find contiguous match of search vector s in original DATA
C vector R.
C Modified version: Exit when sequence terms < 0 or > 9 are found
      real*16 r(*), rvalue, dr, diglim, zero
      integer*4 s(n)
C Difference between 8 byte integer and 16 byte float will
C be exact for the possible range of search values.
      parameter (dr=5.0D-1, diglim=9.5D0, zero=0.0D0)
      nrel = 0
C Empirical limit: not more than 120 data items in single line of
C file "stripped"
      do 110 i = 1, 120
      if ( r(i) .eq. rvalue
     &  .or. r(i) .gt. diglim .or. r(i) .lt. zero ) then
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
