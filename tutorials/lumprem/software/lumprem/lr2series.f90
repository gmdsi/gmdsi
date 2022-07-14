       program LR2SERIES

! -- Program LR2SERIES reads a set of LUMPREM2 output files, using their contents to write
!    time series input files for MODFLOW6.

       implicit none

       integer, parameter   :: MAXSERIES = 200
       integer              :: iline,ifail,nn,i,ierr,jline,icol
       integer              :: iseries,iseriesold,numseries,nseries,jseries,kseries
       integer              :: maxtime,itime,ntime,iiday
       integer              :: i_lumpfile
       integer              :: lw(30),rw(30)
       integer              :: lcol(MAXSERIES),idiv(MAXSERIES)
       integer              :: outseries(MAXSERIES),copy(MAXSERIES)

       double precision     :: dtemp
       double precision     :: tdelay
       double precision     :: scale(MAXSERIES),offset(MAXSERIES)
       double precision     :: firstval(MAXSERIES)

       character (len=10)   :: aline,aseries,acopy
       character (len=15)   :: outmethod(MAXSERIES)
       character (len=20)   :: atype
       character (len=20)   :: stype(MAXSERIES)
       character (len=20)   :: sname(MAXSERIES),rname(MAXSERIES)
       character (len=30)   :: aname,astring
       character (len=50)   :: keyword,prevkey
       character (len=256)  :: infile,lumpfile,tsfile
       character (len=1000) :: cline
       character (len=1500) :: amessage

       integer, allocatable :: time(:)
       double precision, allocatable :: value(:,:)

! -- Initialization

       nseries=0
       iseries=0
       i_lumpfile=0
       keyword=' '

! -- The control file is opened.

100    write(6,110,advance='no')
110    format(' Enter name of LR2SERIES control file: ')
       read(5,*) infile
       open(unit=10,file=infile,status='old',action='read',err=100)
       write(6,*)
       write(6,115) trim(infile)
115    format(' - reading file ',a,'...')
       write(6,*)

! -- Keywords are now read and action is taken.

       iline=0
       do
         iline=iline+1
         read(10,'(a)',end=1000) cline
         cline=adjustl(cline)
         if(cline.eq.' ') cycle
         if(cline(1:1).eq.'#') cycle
         call lowcase(cline)
         call writint(aline,iline)
         call linesplit(ifail,3,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         prevkey=keyword
         keyword=cline(lw(1):rw(1))
         select case(keyword)

         case('read_lumprem_output_file')

! -- The line in the control file is parsed.

           iseriesold=iseries
           call upcase(keyword)
           cline=cline(lw(2):)
           call remove_leading_filename(ifail,lumpfile,cline,amessage,aline,infile)
           if(ifail.ne.0) go to 9890
           call linesplit(ifail,1,lw,rw,cline)
           if(ifail.ne.0) go to 9000
           call intread(ifail,cline(lw(1):rw(1)),numseries)
           if(ifail.ne.0)then
             write(amessage,120) trim(keyword),trim(aline),trim(infile)
120          format('Cannot read number of time series associated with ',a,' keyword at ',   &
             'line ',a,' of file ',a,'.')
             go to 9890
           end if
           if(numseries.le.0)then
             write(amessage,130) trim(aline),trim(infile)
130          format('Number of time series must be positive at line ',a,' of file ',a,'.')
             go to 9890
           end if
           if(numseries.gt.MAXSERIES)then
             call writint(aseries,MAXSERIES)
             write(amessage,140) trim(aseries),trim(aline),trim(infile)
140          format('Number of time series cannot exceed ',a,' at line ',a,' of file ',a,'.')
             go to 9890
           end if
           if(iseries+numseries.gt.MAXSERIES)then
             call writint(aseries,MAXSERIES)
             write(amessage,142) trim(aseries),trim(aline),trim(infile)
142          format('The total number of time series stored in LR2SERIES memory cannot exceed ',a,   &
             '. Violation occurs at line ',a,' of file ',a,'.')
             go to 9890
           end if
           do jseries=1,numseries
             iseries=iseries+1
151          iline=iline+1
             read(10,'(a)',end=9220) cline
             if(cline.eq.' ') go to 151
             cline=adjustl(cline)
             if(cline(1:1).eq.'#') go to 151
             call writint(aline,iline)
             call linesplit(ifail,3,lw,rw,cline)
             if(ifail.ne.0)go to 9000
             call remove_leading_filename(ifail,aname,cline,amessage,aline,infile)
             if(ifail.ne.0)then
               write(amessage,170) trim(aline),trim(infile)
170            format('Cannot read user-supplied time series name at line ',a,' of file ',a,'.')
               go to 9890
             end if
             nn=len_trim(aname)
             if(nn.gt.20)then
               write(amessage,190) trim(aname),trim(aline),trim(infile)
190            format('User-provided time series name "',a,'" exceeds 20 characters at line ',a,' of file ',a,'.')
               go to 9890
             end if
             do i=1,nn
               if(aname(i:i).eq.' ')then
                 write(amessage,200) trim(aname),trim(aline),trim(infile)
200              format('Time series name "',a,'" must not have a space at line ',a,' of file ',a,'.')
                 go to 9890
               end if
             end do
             if(iseries.gt.1)then
               do kseries=1,iseries-1
                 if(aname.eq.sname(kseries)) then
                   write(amessage,210) trim(aname),trim(aline),trim(infile)
210                format('User-supplied series name "',a,'" is not unique at line ',a,' of file ',a,'.')
                   go to 9890
                 end if
               end do
             end if
             sname(iseries)=aname

             call remove_leading_filename(ifail,stype(iseries),cline,amessage,aline,infile)
             if(ifail.ne.0)then
               write(amessage,150) trim(aline),trim(infile)
150            format('Cannot read LUMPREM2 time series type at line ',a,' of file ',a,'.')
               go to 9890
             end if

             call remove_leading_filename(ifail,astring,cline,amessage,aline,infile)
             if(ifail.ne.0)then
               write(amessage,1510) trim(aline),trim(infile)
1510           format('Cannot read DIV_DELTA_T string at line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(astring.eq.'div_delta_t')then
               idiv(iseries)=1
             else if(astring.eq.'no_div_delta_t')then
               idiv(iseries)=0
             else
               write(amessage,1520) trim(aline),trim(infile)
1520           format('DIV_DELTA_T string must be "div_delta_t" or "no_div_delta_t" at line ',  &
               a,' of file ',a,'.')
               go to 9890
             end if

           end do

! -- The LUMPREM output file is read.

           write(6,211) trim(lumpfile)
211        format(' - reading LUMPREM2 output file ',a,'...')
           open(unit=11,file=lumpfile,status='old',action='read',iostat=ierr)
           if(ierr.ne.0)then
             write(amessage,212) trim(lumpfile)
212          format('Cannot open LUMPREM output file ',a,'.')
             go to 9890
           end if
           jline=1
           read(11,'(a)',err=9020,end=9040) cline
           call lowcase(cline)
           call linesplit(ifail,20,lw,rw,cline)
           if(ifail.ne.0) go to 9070
           do jseries=iseriesold+1,iseries
             atype=stype(jseries)
             do i=1,20
               if(trim(atype).eq.cline(lw(i):rw(i)))then
                 lcol(jseries)=i
                 go to 214
               end if
             end do
             write(amessage,213) trim(atype),trim(lumpfile)
213          format('A series type of "',a,'" does not exist in file ',a,'.')
             go to 9890
214          continue
           end do

! -- If this is the first time we read a LUMPREM output file we read it and then re-wind it
!    to obtain the length of the time series.

           if(i_lumpfile.eq.0)then
             maxtime=0
             do
               maxtime=maxtime+1
               read(11,*,end=215)
             end do
215          maxtime=maxtime-1
             if(maxtime.eq.1)then
               write(amessage,216) trim(lumpfile)
216            format('The time series in file ',a,' has insufficient days.')
               go to 9890
             end if
             rewind(unit=11)
             read(11,*)
             allocate(time(0:maxtime),stat=ierr)
             if(ierr.ne.0) go to 9200
             allocate(value(0:maxtime,MAXSERIES),stat=ierr)
             if(ierr.ne.0) go to 9200
           end if

! -- We now extract data from the LUMPREM2 output file.

           itime=-1
           jline=1
           do
             itime=itime+1
             jline=jline+1
             read(11,'(a)',err=9020,end=300) cline
             if(cline.eq.' ') go to 300
             if(i_lumpfile.gt.0)then
               if(itime.gt.ntime) go to 9100
             end if
             call linesplit(ifail,20,lw,rw,cline)
             if(ifail.ne.0) then
               call writint(aline,jline)
               write(amessage,230) trim(aline),trim(lumpfile)
230            format('Insufficient entries on line ',a,' of file ',a,'.')
               go to 9890
             end if
             call intread(ifail,cline(lw(1):rw(1)),iiday)
             if(ifail.ne.0)then
               call writint(aline,jline)
               write(amessage,229) trim(aline),trim(lumpfile)
229            format('Cannot read number of days from first column of line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(i_lumpfile.eq.0)then
               time(itime)=iiday
               if(itime.eq.0)then
                 if(time(itime).ne.0)then
                   write(amessage,231) trim(lumpfile)
231                format('The first "days" entry in file ',a,' is expected to be zero.')
                   go to 9890
                 end if
               else
                 if(time(itime)-time(itime-1).le.0)then
                   call writint(aline,jline)
                   write(amessage,232) trim(aline),trim(lumpfile)
232                format('Days are not in ascending order at line ',a,' of file ',a,'.')
                   go to 9890
                 end if
               end if
             else
               if(iiday.ne.time(itime)) go to 9100
             end if
             do jseries=iseriesold+1,iseries
               icol=lcol(jseries)
               call drealread(ifail,cline(lw(icol):rw(icol)),dtemp)
               if(ifail.ne.0) go to 9020
               if(itime.eq.0)then
                 if(idiv(jseries).eq.1)then
                   value(itime,jseries)=0.0
                 else
                   value(itime,jseries)=dtemp
                 end if
               else
                 if(idiv(jseries).eq.1)then
                   value(itime,jseries)=dtemp/(time(itime)-time(itime-1))
                   if(itime.eq.1)value(0,jseries)=value(1,jseries)
                 else
                   value(itime,jseries)=dtemp
                 end if
               end if
             end do
           end do
300        continue
           if(i_lumpfile.gt.0) then
             if(itime-1.ne.ntime) go to 9100
           else
             ntime=itime-1
             if(ntime.le.1)then
               write(amessage,216) trim(lumpfile)
               go to 9890
             end if
           end if
           close(unit=11)
           write(6,310) trim(lumpfile)
310        format(' - file ',a,' read ok.')
           i_lumpfile=i_lumpfile+1
           nseries=iseries

         case('write_mf6_time_series_file')

! -- The line in the control file is parsed and series names read.

           call upcase(keyword)
           cline=cline(lw(2):)
           call remove_leading_filename(ifail,tsfile,cline,amessage,aline,infile)
           if(ifail.ne.0) go to 9890
           call linesplit(ifail,1,lw,rw,cline)
           if(ifail.ne.0) go to 9000
           call intread(ifail,cline(lw(1):rw(1)),numseries)
           if(ifail.ne.0)then
             write(amessage,120) trim(keyword),trim(aline),trim(infile)
             go to 9890
           end if
           if(numseries.le.0)then
             write(amessage,130) trim(aline),trim(infile)
             go to 9890
           end if
           if(numseries.gt.MAXSERIES)then
             call writint(aseries,MAXSERIES)
             write(amessage,311) trim(aseries),trim(aline),trim(infile)
311          format('Number of time series written to MF6 time series file cannot exceed ',a,    &
             ' at line ',a,' of file ',a,'.')
             go to 9890
           end if
! -- We see if a time delay is necessary.
           call linesplit(ifail,2,lw,rw,cline)
           if(ifail.ne.0)then
             tdelay=0.0d0
           else
             call drealread(ifail,cline(lw(2):rw(2)),tdelay)
             if(ifail.ne.0)then
               write(amessage,308) trim(aline),trim(infile)
308            format('Cannot read value for time series delay at line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(tdelay.lt.0.0d0)then
               write(amessage,307) trim(aline),trim(infile)
307            format('Value for time series delay must be zero or greater at line ',a,' of file ',a,'.')
               go to 9890
             end if
           end if
           do jseries=1,numseries
309          iline=iline+1
             read(10,'(a)',end=9220) cline
             if(cline.eq.' ') go to 309
             cline=adjustl(cline)
             if(cline(1:1).eq.'#') go to 309
             call writint(aline,iline)
             call linesplit(ifail,4,lw,rw,cline)
             if(ifail.ne.0) go to 9000
             call remove_leading_filename(ifail,aname,cline,amessage,aline,infile)
             if(ifail.ne.0)then
               write(amessage,320) trim(aline),trim(infile)
320            format('Cannot read series name at line ',a,' of file ',a,'.')
               go to 9890
             end if
             do iseries=1,nseries
               if(aname.eq.sname(iseries))then
                 outseries(jseries)=iseries
                 go to 329
               end if
             end do
             write(amessage,325) trim(aname),trim(aline),trim(infile)
325          format('Unknown series name "',a,'" at line ',a,' of file ',a,'.')
             go to 9890
329          continue

             call remove_leading_filename(ifail,astring,cline,amessage,aline,infile)
             if(ifail.ne.0)then
               write(amessage,1550) trim(aline),trim(infile)
1550           format('Cannot read SCALE at line ',a,' of file ',a,'.')
               go to 9890
             end if
             call drealread(ifail,astring,scale(jseries))
             if(ifail.ne.0) then
               write(amessage,1550) trim(aline),trim(infile)
               go to 9890
             end if

             call remove_leading_filename(ifail,astring,cline,amessage,aline,infile)
             if(ifail.ne.0)then
               write(amessage,1551) trim(aline),trim(infile)
1551           format('Cannot read OFFSET at line ',a,' of file ',a,'.')
               go to 9890
             end if
             call drealread(ifail,astring,offset(jseries))
             if(ifail.ne.0) then
               write(amessage,1551) trim(aline),trim(infile)
               go to 9890
             end if

             call remove_leading_filename(ifail,astring,cline,amessage,aline,infile)
             if(ifail.ne.0)then
               write(amessage,1530) trim(aline),trim(infile)
1530           format('Cannot read MF6_METHOD string at line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(astring.eq.'stepwise')then
               outmethod(jseries)='STEPWISE'
             else if(astring.eq.'linear')then
               outmethod(jseries)='LINEAR'
             else if(astring.eq.'linearend')then
               outmethod(jseries)='LINEAREND'
             else
               write(amessage,1540) trim(aline),trim(infile)
1540           format('MF6_METHOD string must be "stepwise", "linear" or "linearend" at line ',  &
               a,' of file ',a,'.')
               go to 9890
             end if

             if(tdelay.gt.0.0d0)then
               call remove_leading_filename(ifail,astring,cline,amessage,aline,infile)
               if(ifail.ne.0)then
                 write(amessage,1560) trim(aline),trim(infile)
1560             format('Cannot read fill-in value string at line ',a,' of file ',a,'.')
                 go to 9890
               end if
               call lowcase(astring)
               if(astring(1:5).eq.'next')then
                 firstval(jseries)=value(0,outseries(jseries))
               else
                 call drealread(ifail,astring,firstval(jseries))
                 if(ifail.ne.0) then
                   write(amessage,1560) trim(aline),trim(infile)
                   go to 9890
                 end if
               end if
             end if

           end do

! -- The attributes header to the MF6 time series file is written.

           write(6,330) trim(tsfile)
330        format(' - writing file ',a,'...')
           open(unit=20,file=tsfile,action='write',iostat=ierr)
           if(ierr.ne.0)then
             write(amessage,340) trim(tsfile)
340          format('Cannot write to file ',a,'.')
             go to 9890
           end if
           write(20,350)
350        format('BEGIN ATTRIBUTES')
           if(numseries.eq.1)then
             write(20,360) trim(sname(outseries(1)))
360          format('   NAME ',a)
           else
             do jseries=1,numseries
               rname(jseries)=sname(outseries(jseries))
             end do
             copy=0                ! an array
             do jseries=2,numseries
               aname=rname(jseries)
               do kseries=1,jseries-1
                 if(rname(kseries).eq.aname)copy(jseries)=copy(jseries)+1
               end do
             end do
             do jseries=2,numseries
               if(copy(jseries).gt.0)then
                 call writint(acopy,copy(jseries))
                 rname(jseries)=trim(rname(jseries))//'_'//trim(acopy)
               end if
             end do
             write(20,370) (trim(rname(jseries)),jseries=1,numseries)
370          format('   NAMES',100(1x,a))
           end if
           if(numseries.eq.1)then
             write(20,380) trim(outmethod(1))
380          format('   METHOD ',a)
           else
             write(20,390) (trim(outmethod(jseries)),jseries=1,numseries)
390          format('   METHODS',100(1x,a))
           end if
           write(20,400)
400        format('END ATTRIBUTES')

! -- The actual time series is now recorded.

           write(20,*)
           write(20,*)
           write(20,410)
410        format('BEGIN TIMESERIES')
           if(tdelay.gt.0.0d0)then
             write(20,420) 0.0d0,(firstval(jseries),jseries=1,numseries)
           end if
           do itime=0,ntime
             dtemp=time(itime)+tdelay
             write(20,420) dtemp,             &
             (value(itime,outseries(jseries))*scale(jseries)+offset(jseries),jseries=1,numseries)
420          format(101(1x,1pg14.7))
           end do
           write(20,430)
430        format('END TIMESERIES')
           close(unit=20)
           write(6,440) trim(tsfile)
440        format(' - file ',a,' written ok.')

         case default
           go to 9050

         end select
       end do

1000   continue
       close(unit=20)
       write(6,*)
       write(6,1010) trim(infile)
1010   format(' - file ',a,' read ok.')

       go to 9900


9000   write(amessage,9010) trim(aline),trim(infile)
9010   format('Insufficient items on line ',a,' of file ',a,'.')
       go to 9890

9020   call writint(aline,jline)
       write(amessage,9030) trim(aline),trim(lumpfile)
9030   format('Error reading line ',a,' of LUMPREM2 output file ',a,'.')
       go to 9890

9040   write(amessage,9045) trim(lumpfile)
9045   format('Unexpected end encountered to LUMPREM2 output file ',a,'.')
       go to 9890

9050   call upcase(keyword)
       write(amessage,9060) trim(keyword),trim(aline),trim(infile),trim(prevkey)
9060   format('Unknown keyword "',a,'" at line ',a,' of file ',a,'. Have ', &
       'you set the number of time series correctly following the previous "',a,'" keyword?')
       go to 9890

9070   write(amessage,9080) trim(lumpfile)
9080   format('Header to LUMPREM2 output file ',a,' is not as expected.')
       go to 9890

9100   write(amessage,9110) trim(lumpfile)
9110   format('The sequence of LUMPREM2 output times in file ',a,' is not the same ',  &
       'as for the previously-read LUMPREM2 output file.')
       go to 9890

9200   write(amessage,9210)
9210   format('Cannot allocate sufficient memory to continue execution.')
       go to 9890

9220   write(amessage,9230) trim(infile),trim(keyword)
9230   format('Premature end encountered to file ',a,' while reading time series ', &
       'information following ',a,' keyword.')
       go to 9890

9890   amessage=' ' //amessage
       call writmess(6,amessage)

9900   continue

       deallocate(time,stat=ierr)
       deallocate(value,stat=ierr)


       end


        subroutine remove_leading_filename(ifail,afile,cline,amessage,aline,infile)

! -- Subroutine REMOVE_LEADING_FILENAME removes the leading filename from a string and reports that
!    filename. It also makes sure that a non-blank character leads the returned string (if it exists).

        implicit none

        integer, intent(out) :: ifail
        character (len=*), intent(out) :: afile
        character (len=*), intent(inout) :: cline
        character (len=*), intent(inout) :: amessage
        character (len=*), intent(in) :: aline
        character (len=*), intent(in) :: infile

        integer           :: nn,mm
        character (len=1) :: aa

! -- Initialization

        ifail=0
        afile=' '

! -- The work is done

        if(cline.eq.' ')then
          write(amessage,20) trim(aline),trim(infile)
          go to 9890
        end if
        cline=adjustl(cline)
        aa=cline(1:1)
        if((aa.eq.'''').or.(aa.eq.'"'))then
          nn=index(cline(2:),aa)
          if(nn.eq.0)then
            write(amessage,10) trim(aline),trim(infile)
10          format('Unbalanced quotes at line ',a,' of file ',a,'.')
            go to 9890
          else if (nn.eq.1)then
            write(amessage,20) trim(aline),trim(infile)
20          format('Missing string at line ',a,' of file ',a,'.')
            go to 9890
          end if
          afile=cline(2:nn)
          cline=adjustl(cline(nn+2:))
          if(afile.eq.' ')then
            write(amessage,20) trim(aline),trim(infile)
            go to 9890
          end if
        else
          nn=index(cline,' ')
          mm=index(cline,char(9))
          if(mm.eq.0)then
            continue
          else if(nn.eq.0)then
            nn=mm
          else
            nn=min(nn,mm)
          end if
          afile=cline(1:nn-1)
          cline=adjustl(cline(nn+1:))
        end if
        go to 9990

9890    ifail=1

9990    continue
        return
        end subroutine remove_leading_filename






        subroutine writint(atemp,ival)

!       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
        end



        SUBROUTINE LOWCASE(ASTRNG)

! -- Subroutine LOWCASE converts a string to lower case.

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,len_trim(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END



        SUBROUTINE UPCASE(ASTRNG)

! -- Subroutine upcase converts a string to upper case.

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,len_trim(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.97).AND.(J.LE.122)) ASTRNG(I:I)=CHAR(J-32)
10      CONTINUE
        RETURN

        END SUBROUTINE UPCASE




        SUBROUTINE LINESPLIT(IFAIL,NUM,LW,RW,CLINE)

! -- Subroutine LINESPLIT splits a string into blank-delimited fragments.

        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM,NBLNK
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=len_trim(CLINE)
        IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
          CALL TABREM(CLINE)
          NBLC=len_trim(CLINE)
        ENDIF
        IF(NBLC.EQ.0) THEN
          IFAIL=-1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
          IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.   &
          (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LW(NW)=I
        DO 30 I=LW(NW)+1,NBLC
          IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.     &
          (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30      CONTINUE
        RW(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RW(NW)=I-1
        J=RW(NW)
        GO TO 5

        END SUBROUTINE LINESPLIT


        SUBROUTINE TABREM(CLINE)

! -- Subroutine TABREM removes tabs from a string.

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
        END SUBROUTINE TABREM


        SUBROUTINE intREAD(IFAIL,CLINE,iTEMP)

! -- Subroutine intREAD reads a real number from a string.

        INTEGER IFAIL
        integer iTEMP
        CHARACTER*6 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(i   )'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) iTEMP
        RETURN

100     IFAIL=1
        RETURN

        END SUBROUTINE INTREAD



        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

! -- Subroutine REALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') len_trim(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END



      subroutine writmess(iunit,amessage)

        implicit none

        integer iunit,jend,i,nblc,junit,leadblank,itake,j
        character*(*) amessage
        character (len=20) ablank

        ablank=' '
        itake=0
        j=0
        junit=iunit

        if(amessage.eq.' ')then
          write(junit,*)
          return
        end if
        write(junit,*)
        do i=1,min(20,len(amessage))
          if(amessage(i:i).ne.' ')go to 21
20      end do
21      leadblank=i-1
        nblc=len_trim(amessage)
5       jend=j+78-itake
        if(jend.ge.nblc) go to 100
        do i=jend,j+1,-1
        if(amessage(i:i).eq.' ') then
          if(itake.eq.0) then
             write(junit,'(a)') amessage(j+1:i)
             itake=2+leadblank
          else
             write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:i)
          end if
          j=i
          go to 5
        end if
        end do
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
          itake=2+leadblank
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        j=jend
        go to 5
100     jend=nblc
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        return

      end

