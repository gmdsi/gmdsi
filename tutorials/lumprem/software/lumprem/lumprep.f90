       program lumprep

! -- Program LUMPREP creates a series of LUMPREM input datasets, corresponding template files, and a partial PEST control file.

       implicit none

       integer, parameter :: MAX_LUMPMOD=30

       integer          :: numdays
       integer          :: dummyintval
       integer          :: i_start_date,i_end_date,i_nday_out,i_silofile,i_rainfile,i_epotfile,  &
                           i_vegfile,i_irrigfile,i_maxvol,i_irrigvolfrac,i_rdelay,i_mdelay,      &
                           i_ks,i_m,i_l,i_mflowmax,i_offset,i_factor1,i_factor2,i_power,i_vol,   &
                           i_batch,i_pest
       integer          :: nrbuf,nmbuf
       integer          :: irrigcode_all
       integer          :: nstep,mxiter
       integer          :: iline,ifail,ierr,i
       integer          :: dds,mms,yys,dde,mme,yye
       integer          :: simlength,nsimdays,nday_out
       integer          :: i_lumpmod,j_lumpmod
       integer          :: noutdays_approx,itemp,noutdays
       integer          :: npar,npargp,nobs,nobsgp,nprior,ntplfle,ninsfle
       integer          :: maxvol_fixed(MAX_LUMPMOD),irrigvolfrac_fixed(MAX_LUMPMOD),  &
                           rdelay_fixed(MAX_LUMPMOD),mdelay_fixed(MAX_LUMPMOD)
       integer          :: ks_fixed(MAX_LUMPMOD),m_fixed(MAX_LUMPMOD),l_fixed(MAX_LUMPMOD), &
                           mflowmax_fixed(MAX_LUMPMOD)
       integer          :: offset_fixed(MAX_LUMPMOD),factor1_fixed(MAX_LUMPMOD),    &
                           factor2_fixed(MAX_LUMPMOD),power_fixed(MAX_LUMPMOD)
       integer          :: cropfac_fixed(MAX_LUMPMOD),gamma_fixed(MAX_LUMPMOD),     &
                           gwirrigfrac_fixed(MAX_LUMPMOD)
       integer          :: lw(10),rw(10)


       double precision :: dummyval,dummyvaltest
       double precision :: cropfac_all,gamma_all,gwirrigfrac_all
       double precision :: irrigvolfrac,rdelay,mdelay
       double precision :: tol
       double precision :: ks,m,l,mflowmax,maxvol,vol
       double precision :: offset,factor1,factor2,power
       double precision :: surface
       double precision :: maxvol_par(MAX_LUMPMOD),irrigvolfrac_par(MAX_LUMPMOD),  &
                           rdelay_par(MAX_LUMPMOD),mdelay_par(MAX_LUMPMOD)
       double precision :: ks_par(MAX_LUMPMOD),m_par(MAX_LUMPMOD),l_par(MAX_LUMPMOD), &
                           mflowmax_par(MAX_LUMPMOD)
       double precision :: offset_par(MAX_LUMPMOD),factor1_par(MAX_LUMPMOD),    &
                           factor2_par(MAX_LUMPMOD),power_par(MAX_LUMPMOD)
       double precision :: cropfac_par(MAX_LUMPMOD),gamma_par(MAX_LUMPMOD),     &
                           gwirrigfrac_par(MAX_LUMPMOD)

       character (len=10)   :: aline,aname
       character (len=12)   :: apar1,apar2,apar3,apar4
       character (len=15)   :: evapcolstring,atrans
       character (len=20)   :: adate,atemp20
       character (len=25)   :: keyword,kw
       character (len=256)  :: infile,rainfile,epotfile,vegfile,irrigfile
       character (len=256)  :: lumpfile,tempfile,outfile,pestfile,silofile,qfile
       character (len=256)  :: batchfile
       character (len=1000) :: cline
       character (len=2000) :: amessage
       character (len=3)    :: lumpname(MAX_LUMPMOD)

       integer, allocatable :: outday(:)
       real, allocatable    :: rain(:),evap(:)

! -- Initialization

       dummyval=-1.1d30
       dummyvaltest=-1.0d30
       dummyintval=-99999999

       i_start_date=0
       i_end_date=0
       i_nday_out=0
       i_silofile=0
       i_rainfile=0
       i_epotfile=0
       i_vegfile=0
       i_irrigfile=0
       i_maxvol=0
       i_irrigvolfrac=0
       i_rdelay=0
       i_mdelay=0
       i_ks=0
       i_m=0
       i_l=0
       i_mflowmax=0
       i_offset=0
       i_factor1=0
       i_factor2=0
       i_power=0
       i_vol=0
       i_lumpmod=0
       i_batch=0
       i_pest=0
       noutdays_approx=dummyintval

! -- Default values

       nrbuf=50
       nmbuf=10
       cropfac_all=dummyval
       gamma_all=dummyval
       gwirrigfrac_all=dummyval
       irrigcode_all=dummyintval
       irrigvolfrac=0.5d0
       rdelay=5.0d0
       mdelay=1.0d0
       offset=0.0d0
       factor1=1.0d-5
       factor2=1.0d-5
       power=0.0d0
       surface=100.0d0
       maxvol=0.3d0
       nstep=1
       mxiter=500
       tol=1.0d-5
       ks=0.1d0
       m=0.5d0
       l=0.5d0
       mflowmax=0.1d0

! -- The control file is opened.

100    write(6,110,advance='no')
110    format(' Enter name of LUMPREP control file: ')
       read(5,*) infile
       open(unit=10,file=infile,status='old',err=100)
       write(6,*)
       write(6,115) trim(infile)
115    format(' - reading file ',a,'...')
       write(6,*)

! -- Keywords are now read and action is taken.

       iline=0
       do
         iline=iline+1
         call writint(aline,iline)
         read(10,'(a)',end=1000) cline
         cline=adjustl(cline)
         if(cline.eq.' ') cycle
         if(cline(1:1).eq.'#') cycle
         call lowcase(cline)
         call linesplit(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9000
         keyword=cline(lw(1):rw(1))
         select case(keyword)

         case('start_date')
           call upcase(keyword)
           if(i_start_date.ne.0) then
             go to 9100
           else
             i_start_date=1
           end if
           adate=cline(lw(2):rw(2))
           call getdate2(ifail,adate,dds,mms,yys)
           if(ifail.ne.0) go to 9130
           write(6,120) dds,mms,yys
120        format(' START_DATE',t30,'= ',i2.2,'/'i2.2,'/',i4)
           if(i_end_date.ne.0)then
             nsimdays=simlength(ifail,dds,mms,yys,dde,mme,yye,amessage)
             if(ifail.ne.0) go to 9890
           end if

         case('end_date')
           call upcase(keyword)
           if(i_end_date.ne.0) then
             go to 9100
           else
             i_end_date=1
           end if
           adate=cline(lw(2):rw(2))
           call getdate2(ifail,adate,dde,mme,yye)
           if(ifail.ne.0) go to 9130
           write(6,121) dde,mme,yye
121        format(' END_DATE',t30,'= ',i2.2,'/'i2.2,'/',i4)
           if(i_start_date.ne.0)then
             nsimdays=simlength(ifail,dds,mms,yys,dde,mme,yye,amessage)
             if(ifail.ne.0) go to 9890
           end if

         case('nday_out')
           call upcase(keyword)
           if(i_nday_out.ne.0) then
             go to 9100
           else
             i_nday_out=1
           end if
           if((i_start_date.eq.0).or.(i_end_date.eq.0)) go to 9030
           atemp20=cline(lw(2):rw(2))
           if((atemp20.eq.'month').or.(atemp20.eq.'monthly'))then
             nday_out=-1
             write(6,125)
125          format(' NDAY_OUT',t30,'= monthly')
           else
             call intread(ifail,cline(lw(2):rw(2)),nday_out)
             if(ifail.ne.0) go to 9070
             if(nday_out.le.0) go to 9170
             call writint(atemp20,nday_out)
             write(6,126) trim(atemp20)
126          format(' NDAY_OUT',t30,'= ',a)
           end if

         case('steps_per_day')
           call upcase(keyword)
           call intread(ifail,cline(lw(2):rw(2)),nstep)
           if(nstep.le.0) go to 9170
           call writint(atemp20,nstep)
           write(6,128) trim(atemp20)
128        format(' STEPS_PER_DAY',t30,'= ',a)

         case('silofile')
           call upcase(keyword)
           if((i_start_date.eq.0).or.(i_end_date.eq.0)) go to 9030
           cline=cline(lw(2):)
           call remove_leading_filename(ifail,silofile,cline,amessage,aline,infile)
           if(ifail.ne.0) go to 9890
           call remove_leading_filename(ifail,evapcolstring,cline,amessage,aline,infile)
           if(ifail.ne.0) then
             write(amessage,140) trim(aline),trim(infile)
140          format('No EVAPCOLSTRING string, or corrupted EVAPCOLSTRING string, ', &
             'supplied on line ',a,' of file ',a,'.')
             go to 9890
           end if
           atemp20='SILOFILE'
           write(6,150) trim(atemp20),trim(silofile)
150        format(' ',a,t30,'= ',a)
           atemp20='EVAPCOLSTRING'
           write(6,151) trim(atemp20),trim(evapcolstring)
151        format(' ',a,t30,'= "',a,'"')
           if(i_silofile.eq.0)then
             allocate(rain(nsimdays),evap(nsimdays),stat=ierr)
             if(ierr.ne.0) go to 9200
           end if
           write(6,160) trim(silofile)
160        format(' - reading file ',a,'...')
           call read_silo_file(ifail,nsimdays,dds,mms,yys,dde,mme,yye,rain,evap,evapcolstring,   &
           silofile,cline,amessage)
           if(ifail.ne.0) then
             go to 9890
           else
             write(6,170) trim(silofile)
170          format(' - file ',a,' read ok')
           end if
           rain=rain*0.001d0            ! array
           evap=evap*0.001d0            ! array
           i_silofile=1

         case('rainfile')
           call upcase(keyword)
           if(i_silofile.eq.0)then
             write(amessage,175) trim(infile)
175          format('A RAINFILE keyword must not be provided before a SILOFILE keyword in file ',a,'.')
             go to 9890
           end if
           cline=cline(lw(2):)
           call remove_leading_filename(ifail,rainfile,cline,amessage,aline,infile)
           if(ifail.ne.0) go to 9890
           write(6,176) trim(rainfile)
176        format(' RAINFILE',t30,'= ',a)
           write(6,180) trim(rainfile)
180        format(' - writing file ',a,'...')
           call write_lumprem_climate_file(ifail,nsimdays,rain,rainfile,amessage)
           if(ifail.ne.0) then
             go to 9890
           else
             write(6,190) trim(rainfile)
190          format(' - file ',a,' written ok')
           end if
           i_rainfile=1

         case('epotfile')
           call upcase(keyword)
           if(i_silofile.eq.0)then
             write(amessage,195) trim(infile)
195          format('A EPOTFILE keyword must not be provided before a SILOFILE keyword in file ',a,'.')
             go to 9890
           end if
           cline=cline(lw(2):)
           call remove_leading_filename(ifail,epotfile,cline,amessage,aline,infile)
           if(ifail.ne.0) go to 9890
           write(6,196) trim(epotfile)
196        format(' EPOTFILE',t30,'= ',a)
           write(6,180) trim(epotfile)
           call write_lumprem_climate_file(ifail,nsimdays,evap,epotfile,amessage)
           if(ifail.ne.0) then
             go to 9890
           else
             write(6,190) trim(epotfile)
           end if
           i_epotfile=1

         case('vegfile')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),cropfac_all)
           if(ifail.eq.0)then
             vegfile=' '
             if(cropfac_all.lt.0.0d0)then
               write(amessage,210) trim(aline),trim(infile)
210            format('Crop factor cannot be negative at line ',a,' of file ',a,'.')
               go to 9890
             end if
             call linesplit(ifail,3,lw,rw,cline)
             if(ifail.ne.0)then
               write(amessage,220) trim(aline),trim(infile)
220            format('If a value is supplied for the crop factor on line ',a,  &
               ' of file ',a,' then it must be supplied for gamma as well.')
               go to 9890
             end if
             call drealread(ifail,cline(lw(3):rw(3)),gamma_all)
             if(ifail.ne.0)then
               write(amessage,230) trim(aline),trim(infile)
230            format('Cannot read value for gamma on line ',a,' of file ',a,'.')
               go to 9890
             end if
             if(gamma_all.le.0.0d0)then
               write(amessage,231) trim(aline),trim(infile)
231            format('Gamma must be positive at line ',a,' of file ',a,'.')
               go to 9890
             end if
             write(atemp20,228) cropfac_all
228          format(1pg14.7)
             atemp20=adjustl(atemp20)
             write(6,227) trim(atemp20)
227          format(' CROPFAC',t30,'= ',a)
             write(atemp20,228) gamma_all
             atemp20=adjustl(atemp20)
             write(6,226) trim(atemp20)
226          format(' GAMMA',t30,'= ',a)
           else
             cropfac_all=dummyval
             gamma_all=dummyval
             cline=cline(lw(2):)
             call remove_leading_filename(ifail,vegfile,cline,amessage,aline,infile)
             if(ifail.ne.0) go to 9890
             write(6,323) trim(vegfile)
323          format(' VEGFILE',t30,'= ',a)
           end if
           i_vegfile=1

         case('irrigfile')
           call upcase(keyword)
           call intread(ifail,cline(lw(2):rw(2)),irrigcode_all)
           if(ifail.eq.0)then
             irrigfile=' '
             if((irrigcode_all.ne.0).and.(irrigcode_all.ne.1))then
               write(amessage,240) trim(aline),trim(infile)
240            format('IRRIGCODE must be 0 or 1 at line ',a,' of file ',a,'.')
               go to 9890
             end if
             call linesplit(ifail,3,lw,rw,cline)
             if(ifail.ne.0)then
               write(amessage,250) trim(aline),trim(infile)
250            format('If a value is supplied for IRRIGCODE on line ',a,  &
               ' of file ',a,' then it must also be supplied for GWIRRIGFRAC.')
               go to 9890
             end if
             call drealread(ifail,cline(lw(3):rw(3)),gwirrigfrac_all)
             if(ifail.ne.0)then
               write(amessage,260) trim(aline),trim(infile)
260            format('Cannot read value for GWIRRIGFRAC on line ',a,' of file ',a,'.')
               go to 9890
             else
               if((gwirrigfrac_all.lt.0.0).or.(gwirrigfrac_all.gt.1.0d0))then
                 write(amessage,270) trim(aline),trim(infile)
270              format('GWIRRIGFRAC must not be less than zero or greater than one on ', &
                 'line ',a,' of file ',a,'.')
                 go to 9890
               end if
             end if
             call writint(atemp20,irrigcode_all)
             write(6,271) trim(atemp20)
271          format(' IRRIGCODE',t30,'= ',a)
             write(atemp20,228) gwirrigfrac_all
             atemp20=adjustl(atemp20)
             write(6,272) trim(atemp20)
272          format(' GWIRRIGFRAC',t30,'= ',a)
           else
             irrigcode_all=dummyintval
             gwirrigfrac_all=dummyval
             cline=cline(lw(2):)
             call remove_leading_filename(ifail,irrigfile,cline,amessage,aline,infile)
             if(ifail.ne.0) go to 9890
             write(6,229) trim(irrigfile)
229          format(' IRRIGFILE',t30,'= ',a)
           end if
           i_irrigfile=1

         case('maxvol')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),maxvol)
           if(ifail.ne.0) go to 9070
           if(maxvol.le.0.0d0) go to 9170
           write(atemp20,'(1pg14.7)') maxvol
           atemp20=adjustl(atemp20)
           write(6,292) trim(atemp20)
292        format(' MAXVOL',t30,'= ',a)
           i_maxvol=1

         case('irrigvolfrac')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),irrigvolfrac)
           if(ifail.ne.0) go to 9070
           if((irrigvolfrac.le.0.0d0).or.(irrigvolfrac.ge.1.0))then
             write(amessage,290) trim(keyword),trim(aline),trim(infile)
290          format(a,' must be between zero and one at line ',a,' of file ',a,'.')
             go to 9890
           end if
           write(atemp20,'(1pg14.7)') irrigvolfrac
           atemp20=adjustl(atemp20)
           write(6,293) trim(atemp20)
293        format(' IRRIGVOLFRAC',t30,'= ',a)
           i_irrigvolfrac=1

         case('rdelay')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),rdelay)
           if(ifail.ne.0) go to 9070
           if(rdelay.le.0.0d0) go to 9170
           write(atemp20,'(1pg14.7)') rdelay
           atemp20=adjustl(atemp20)
           write(6,294) trim(atemp20)
294        format(' RDELAY',t30,'= ',a)
           i_rdelay=1

         case('mdelay')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),mdelay)
           if(ifail.ne.0) go to 9070
           if(mdelay.le.0.0d0) go to 9170
           write(atemp20,'(1pg14.7)') mdelay
           atemp20=adjustl(atemp20)
           write(6,295) trim(atemp20)
295        format(' MDELAY',t30,'= ',a)
           i_mdelay=1

         case('ks')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),ks)
           if(ifail.ne.0) go to 9070
           if(ks.le.0.0d0) go to 9170
           write(atemp20,'(1pg14.7)') ks
           atemp20=adjustl(atemp20)
           write(6,296) trim(atemp20)
296        format(' KS',t30,'= ',a)
           i_ks=1

         case('m')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),m)
           if(ifail.ne.0) go to 9070
           if(m.le.0.0d0) go to 9170
           write(atemp20,'(1pg14.7)') m
           atemp20=adjustl(atemp20)
           write(6,297) trim(atemp20)
297        format(' M',t30,'= ',a)
           i_m=1

         case('l')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),l)
           if(ifail.ne.0) go to 9070
           if(l.le.0.0d0) go to 9170
           write(atemp20,'(1pg14.7)') l
           atemp20=adjustl(atemp20)
           write(6,298) trim(atemp20)
298        format(' L',t30,'= ',a)
           i_l=1

         case('mflowmax')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),mflowmax)
           if(ifail.ne.0) go to 9070
           if(mflowmax.le.0.0d0) go to 9170
           write(atemp20,'(1pg14.7)') mflowmax
           atemp20=adjustl(atemp20)
           write(6,299) trim(atemp20)
299        format(' MFLOWMAX',t30,'= ',a)
           i_mflowmax=1

         case('offset')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),offset)
           if(ifail.ne.0) go to 9070
           write(atemp20,'(1pg14.7)') offset
           atemp20=adjustl(atemp20)
           write(6,301) trim(atemp20)
301        format(' OFFSET',t30,'= ',a)
           i_offset=1

         case('factor1')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),factor1)
           if(ifail.ne.0) go to 9070
           if(factor1.le.0.0) go to 9170               ! Is this the best thing to do?
           write(atemp20,'(1pg14.7)') factor1
           atemp20=adjustl(atemp20)
           write(6,302) trim(atemp20)
302        format(' FACTOR1',t30,'= ',a)
           i_factor1=1

         case('factor2')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),factor2)
           if(ifail.ne.0) go to 9070
           if(factor2.le.0.0) go to 9170                ! Is this the best thing to do?
           write(atemp20,'(1pg14.7)') factor2
           atemp20=adjustl(atemp20)
           write(6,303) trim(atemp20)
303        format(' FACTOR2',t30,'= ',a)
           i_factor2=1

         case('power')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),power)
           if(ifail.ne.0) go to 9070
           write(atemp20,'(1pg14.7)') power
           atemp20=adjustl(atemp20)
           write(6,304) trim(atemp20)
304        format(' POWER',t30,'= ',a)
           i_power=1

         case('surface')
           call upcase(keyword)
           call drealread(ifail,cline(lw(2):rw(2)),surface)
           if(ifail.ne.0) go to 9070
           write(atemp20,'(1pg16.9)') surface
           atemp20=adjustl(atemp20)
           write(6,305) trim(atemp20)
305        format(' SURFACE',t30,'= ',a)

         case('vol')
           call upcase(keyword)
           if(i_maxvol.eq.0)then
             write(amessage,318) trim(infile)
318          format('If a VOL keyword appears in file ',a,', it must be preceded by a MAXVOL keyword.')
             go to 9890
           end if
           call drealread(ifail,cline(lw(2):rw(2)),vol)
           if(ifail.ne.0) go to 9070
           if(vol.le.0.0d0) go to 9170
           if(vol.gt.maxvol)then
             write(amessage,319) trim(keyword),trim(aline),trim(infile)
319          format(a,' cannot exceed value previously supplied for MAXVOL at line ',a,' of file ',a,'.')
             go to 9890
           end if
           write(atemp20,'(1pg14.7)') vol
           atemp20=adjustl(atemp20)
           write(6,322) trim(atemp20)
322        format(' VOL',t30,'= ',a)
           i_vol=1

         case('lumprem_model_name')
           call upcase(keyword)
           i_lumpmod=i_lumpmod+1
           if(i_lumpmod.gt.MAX_LUMPMOD)then
             call writint(atemp20,MAX_LUMPMOD)
             write(amessage,320) atemp20
320          format('An input file for this program can cite a maximum of ',a,' LUMPREM_MODEL_NAME keywords.')
             go to 9890
           end if
           cline=cline(lw(2):)
           call remove_leading_filename(ifail,aname,cline,amessage,aline,infile)
           if(ifail.ne.0) then
             write(amessage,330) trim(keyword),trim(aline),trim(infile)
330          format('Cannot read ',a,' from line ',a,' of file ',a,'.')
             go to 9890
           end if
           if(len_trim(aname).gt.3)then
             write(amessage,340) trim(keyword),trim(aline),trim(infile)
340          format(a,' must be 3 characters or less at line ',a,' of file ',a,'.')
             go to 9890
           end if
           call lowcase(aname)
           do i=1,len_trim(aname)
             if(aname(i:i).eq.' ')then
               write(amessage,341) trim(keyword),trim(aline),trim(infile)
341            format(a,' must not have an enclosed space at line ',a,' of file ',a,'.')
               go to 9890
             end if
           end do
           lumpname(i_lumpmod)=aname
           write(6,339) trim(aname)
339        format(' LUMPREM_MODEL_NAME',t30,'= ','"',a,'"')
           if(i_lumpmod.gt.1)then
             do j_lumpmod=1,i_lumpmod-1
               if(aname.eq.lumpname(j_lumpmod))then
                 write(amessage,345) trim(aname),trim(aline),trim(infile)
345              format('LUMPREM_MODEL_NAME "',a,'" repeated at line ',a,' of file ',a,'.')
                 go to 9890
               endif
             end do
           end if
           if(i_nday_out.eq.0)then
             kw='NDAY_OUT'
             go to 9250
           end if
           if(i_maxvol.eq.0)then
             kw='MAXVOL'
             go to 9250
           end if
           if(i_vegfile.eq.0)then
             kw='VEGFILE'
             go to 9250
           end if
           if(i_rainfile.eq.0)then
             kw='RAINFILE'
             go to 9250
           end if
           if(i_epotfile.eq.0)then
             kw='EPOTFILE'
             go to 9250
           end if
           if(i_irrigfile.eq.0)then
             kw='IRRIGFILE'
             go to 9250
           end if
           lumpfile='lr_'//trim(aname)//'.in'
           tempfile='lr_'//trim(aname)//'.tpl'

           write(6,360) trim(lumpfile)
360        format(' - writing file ',a,'...')
           outfile=lumpfile
           open(unit=20,file=lumpfile,action='write',err=9300)
           write(20,370)
370        format('* earth properties')
           write(20,380) maxvol,irrigvolfrac
380        format(4(1x,1pg14.7))
           write(20,380) rdelay,mdelay
           write(20,380) ks,m,l,mflowmax
           write(20,400)
400        format('* volume to elevation')
           write(20,380) offset,factor1,factor2,power
           write(20,405)
405        format('* topographic surface')
           write(20,406) surface
406        format(1x,1pg16.9)
           write(20,410)
410        format('* initial conditions')
           if(i_vol.eq.0) vol=maxvol*0.5
           write(20,380) vol
           write(20,420) nrbuf,nmbuf
420        format(2i10)
           write(20,430) (0.0,i=1,nrbuf)
430        format(20(1x,f3.1))
           write(20,430) (0.0,i=1,nmbuf)
           write(20,440)
440        format('* solution parameters')
           write(20,450) nstep,mxiter,tol
450        format(i10,i10,1x,1pg14.7)
           write(20,455)
455        format('* timing information')
           if(noutdays_approx.eq.dummyintval)then
             noutdays_approx=numdays(dds,mms,yys,dde,mme,yye)
             if(nday_out.eq.-1)then
               noutdays_approx=noutdays_approx/27+2
             else
               itemp=max(nday_out-1,1)
               noutdays_approx=noutdays_approx/itemp+2
             end if
             allocate(outday(noutdays_approx),stat=ierr)
             if(ierr.ne.0) go to 9200
             call calc_outday(noutdays_approx,nday_out,noutdays,outday,dds,mms,yys,dde,mme,yye)
           end if
           write(20,420) nsimdays,noutdays
           write(20,431) (outday(i),i=1,noutdays)
431        format(20i5)
           write(20,441)
441        format('* data filenames')
           if(cropfac_all.gt.dummyvaltest)then
             write(20,380) cropfac_all,gamma_all
           else
             call addquote(vegfile,qfile)
             write(20,451) trim(qfile)
451          format(a)
           end if
           call addquote(rainfile,qfile)
           write(20,451) trim(qfile)
           call addquote(epotfile,qfile)
           write(20,451) trim(qfile)
           if(irrigcode_all.gt.dummyintval)then
             write(20,453) irrigcode_all,gwirrigfrac_all
453          format(i3,2x,1pg14.7)
           else
             call addquote(irrigfile,qfile)
             write(20,451) trim(qfile)
           end if
           close(unit=20)
           write(6,460) trim(lumpfile)
460        format(' - file ',a,' written ok')

           write(6,360) trim(tempfile)
           outfile=tempfile
           open(unit=20,file=tempfile,action='write',err=9300)
           write(20,470)
470        format('ptf $')
           write(20,370)
           maxvol_par(i_lumpmod)=maxvol
           maxvol_fixed(i_lumpmod)=0
           irrigvolfrac_par(i_lumpmod)=irrigvolfrac
           if(irrigcode_all.eq.0)then
             irrigvolfrac_fixed(i_lumpmod)=1
           else
             irrigvolfrac_fixed(i_lumpmod)=0
           end if
           apar1='maxvol_'//trim(aname)
           apar2='irigvf_'//trim(aname)
           write(20,480) trim(apar1),trim(apar2)
480        format('$',a,t14,'$  $',a,t30,'$')
           rdelay_par(i_lumpmod)=rdelay
           mdelay_par(i_lumpmod)=mdelay
           rdelay_fixed(i_lumpmod)=0
           mdelay_fixed(i_lumpmod)=0
           apar1='rdelay_'//trim(aname)
           apar2='mdelay_'//trim(aname)
           write(20,480) trim(apar1),trim(apar2)
           apar1='ks_'//trim(aname)
           apar2='m_'//trim(aname)
           apar3='l_'//trim(aname)
           apar4='mfmax_'//trim(aname)
           write(20,490) trim(apar1),trim(apar2),trim(apar3),trim(apar4)
490        format('$',a,t14,'$  $',a,t30,'$  $',a,t46,'$  $',a,t62,'$')
           ks_par(i_lumpmod)=ks
           m_par(i_lumpmod)=m
           l_par(i_lumpmod)=l
           mflowmax_par(i_lumpmod)=mflowmax
           ks_fixed(i_lumpmod)=0
           m_fixed(i_lumpmod)=0
           l_fixed(i_lumpmod)=0
           mflowmax_fixed(i_lumpmod)=0
           write(20,400)
           apar1='offset_'//trim(aname)
           apar2='fac1_'//trim(aname)
           apar3='fac2_'//trim(aname)
           apar4='power_'//trim(aname)
           write(20,490) trim(apar1),trim(apar2),trim(apar3),trim(apar4)
           offset_par(i_lumpmod)=offset
           factor1_par(i_lumpmod)=factor1
           factor2_par(i_lumpmod)=factor2
           power_par(i_lumpmod)=power
           if((offset.eq.0.0d0).and.(power.eq.0.0d0))then
             offset_fixed(i_lumpmod)=1
             factor1_fixed(i_lumpmod)=1
             factor2_fixed(i_lumpmod)=1
             power_fixed(i_lumpmod)=1
           else
             offset_fixed(i_lumpmod)=0
             factor1_fixed(i_lumpmod)=0
             factor2_fixed(i_lumpmod)=0
             power_fixed(i_lumpmod)=0
           end if
           write(20,405)
           write(20,406) surface
           write(20,410)
           write(20,380) vol
           write(20,420) nrbuf,nmbuf
           write(20,430) (0.0, i=1,nrbuf)
           write(20,430) (0.0,i=1,nmbuf)
           write(20,440)
           write(20,450) nstep,mxiter,tol
           write(20,455)
           write(20,420) nsimdays,noutdays
           write(20,431) (outday(i),i=1,noutdays)
           write(20,441)
           if(cropfac_all.gt.dummyvaltest)then
             apar1='crfac_'//trim(aname)
             apar2='gamma_'//trim(aname)
             write(20,480) trim(apar1),trim(apar2)
             cropfac_par(i_lumpmod)=cropfac_all
             gamma_par(i_lumpmod)=gamma_all
             cropfac_fixed(i_lumpmod)=0
             gamma_fixed(i_lumpmod)=0
           else
             call addquote(vegfile,qfile)
             write(20,451) trim(qfile)
             cropfac_par(i_lumpmod)=dummyval
             gamma_par(i_lumpmod)=dummyval
             cropfac_fixed(i_lumpmod)=2
             gamma_fixed(i_lumpmod)=2
           end if
           call addquote(rainfile,qfile)
           write(20,451) trim(qfile)
           call addquote(epotfile,qfile)
           write(20,451) trim(qfile)
           if(irrigcode_all.gt.dummyintval)then
             apar1='gwirfr_'//trim(aname)
             write(20,510) irrigcode_all,trim(apar1)
510          format(i5,2x,'$',a,t22,'$')
             gwirrigfrac_par(i_lumpmod)=gwirrigfrac_all
             if(irrigcode_all.eq.1)then
               gwirrigfrac_fixed(i_lumpmod)=0
             else
               gwirrigfrac_fixed(i_lumpmod)=1
             end if
           else
             call addquote(irrigfile,qfile)
             write(20,451) trim(qfile)
             gwirrigfrac_par(i_lumpmod)=dummyval
             gwirrigfrac_fixed(i_lumpmod)=2
           end if
           close(unit=20)
           write(6,460) trim(tempfile)

         case('batch_file_name')
           call upcase(keyword)
           cline=cline(lw(2):)
           call remove_leading_filename(ifail,batchfile,cline,amessage,aline,infile)
           if(ifail.ne.0) go to 9890
           write(6,520) trim(batchfile)
520        format(' BATCH_FILE_NAME',t30,'= ',a)
           i_batch=1

         case('pest_control_file')
           call upcase(keyword)
           cline=cline(lw(2):)
           call remove_leading_filename(ifail,pestfile,cline,amessage,aline,infile)
           if(ifail.ne.0) go to 9890
           write(6,525) trim(pestfile)
525        format(' PEST_CONTROL_FILE',t30,'= ',a)
           if(i_lumpmod.eq.0)then
             write(amessage,530) trim(aline),trim(infile)
530          format('A PEST_CONTROL_FILE keyword must be preceeded by at least one ',  &
             'LUMPREM_MODEL_NAME keyword at line ',a,' of file ',a,'.')
             go to 9890
           end if
           if(i_batch.eq.0)then
             write(amessage,540) trim(aline),trim(infile)
540          format('A PEST_CONTROL_FILE keyword must be preceded by at least one ',   &
             'BATCH_FILE_NAME keyword at line ',a,' of file ',a,'.')
             go to 9890
           end if
           outfile=pestfile
           write(6,180) trim(pestfile)
           open(unit=20,file=pestfile,action='write',err=9300)
           npargp=15
           nobs=0
           nobsgp=0
           nprior=0
           npar=0
           ntplfle=i_lumpmod
           ninsfle=0
           do i=1,i_lumpmod
             if(maxvol_fixed(i).ne.2) npar=npar+1
             if(irrigvolfrac_fixed(i).ne.2) npar=npar+1
             if(rdelay_fixed(i).ne.2) npar=npar+1
             if(mdelay_fixed(i).ne.2) npar=npar+1
             if(ks_fixed(i).ne.2) npar=npar+1
             if(m_fixed(i).ne.2) npar=npar+1
             if(l_fixed(i).ne.2) npar=npar+1
             if(mflowmax_fixed(i).ne.2) npar=npar+1
             if(offset_fixed(i).ne.2) npar=npar+1
             if(factor1_fixed(i).ne.2) npar=npar+1
             if(factor2_fixed(i).ne.2) npar=npar+1
             if(power_fixed(i).ne.2) npar=npar+1
             if(cropfac_fixed(i).ne.2) npar=npar+1
             if(gamma_fixed(i).ne.2) npar=npar+1
             if(gwirrigfrac_fixed(i).ne.2) npar=npar+1
           end do
           write(20,1115)
1115       format('pcf')
           write(20,1116)
1116       format('* control data')
           write(20,1117)
1117       format('restart estimation')
           write(20,1120) npar,nobs,npargp,nprior,nobsgp
1120       format(5i10)
           write(20,1123) ntplfle,ninsfle
1123       format(2i10,'  single  point  1   0   0')
           write(20,1125)
1125       format('10.0  -3.0  0.3  0.03  10  999')
           write(20,1127)
1127       format('10.0   10.0    0.001')
           write(20,1130)
1130       format('0.1   boundscale')
           write(20,1132)
1132       format('50  0.005  4  4  0.005  4')
           write(20,1135)
1135       format('1  1  1')
           write(20,1137)
1137       format('* singular value decomposition')
           write(20,1138)
1138       format('1')
           write(20,1140)
1140       format('10000  5.0e-7')
           write(20,1142)
1142       format('0')
           write(20,1150)
1150       format('* parameter groups')
           write(20,1160)
1160       format('maxvol     relative  0.015   0.00005    switch   2.0  parabolic')
           write(20,1170)
1170       format('irigvf     relative  0.015   0.0001     switch   2.0  parabolic')
           write(20,1180)
1180       format('rdelay     relative  0.015   0.0001     switch   2.0  parabolic')
           write(20,1190)
1190       format('mdelay     relative  0.015   0.0001     switch   2.0  parabolic')
           write(20,1200)
1200       format('ks         relative  0.015   0.0        switch   2.0  parabolic')
           write(20,1210)
1210       format('m          relative  0.015   0.0        switch   2.0  parabolic')
           write(20,1220)
1220       format('l          relative  0.015   0.0        switch   2.0  parabolic')
           write(20,1230)
1230       format('mfmax      relative  0.015   0.0001     switch   2.0  parabolic')
           write(20,1240)
1240       format('offset     absolute  0.1     0.0        switch   2.0  parabolic')
           write(20,1250)
1250       format('fac1       relative  0.015   0.0        switch   2.0  parabolic')
           write(20,1260)
1260       format('fac2       relative  0.015   0.0        switch   2.0  parabolic')
           write(20,1270)
1270       format('power      absolute  0.015   0.0        switch   2.0  parabolic')
           write(20,1280)
1280       format('crfac      relative  0.015   0.0001     switch   2.0  parabolic')
           write(20,1290)
1290       format('gamma      relative  0.015   0.0        switch   2.0  parabolic')
           write(20,1295)
1295       format('gwirfr     relative  0.015   0.0001     switch   2.0  parabolic')
           write(20,1300)
1300       format('* parameter data')
           do i=1,i_lumpmod
             apar1='maxvol_'//trim(lumpname(i))
             if(maxvol_fixed(i).ne.2)then
               if(maxvol_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1310) trim(apar1),trim(atrans),maxvol_par(i)
1310           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'0.001     10.0     maxvol    1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='irigvf_'//trim(lumpname(i))
             if(irrigvolfrac_fixed(i).ne.2)then
               if(irrigvolfrac_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1320) trim(apar1),trim(atrans),irrigvolfrac_par(i)
1320           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'0.001     1.0      irigvf    1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='rdelay_'//trim(lumpname(i))
             if(rdelay_fixed(i).ne.2)then
               if(rdelay_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1330) trim(apar1),trim(atrans),rdelay_par(i)
1330           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'0.001     50.0     rdelay    1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='mdelay_'//trim(lumpname(i))
             if(mdelay_fixed(i).ne.2)then
               if(mdelay_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1340) trim(apar1),trim(atrans),mdelay_par(i)
1340           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'0.001     10.0     mdelay    1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='ks_'//trim(lumpname(i))
             if(ks_fixed(i).ne.2)then
               if(ks_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1350) trim(apar1),trim(atrans),ks_par(i)
1350           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'1.0e-5    100.0    ks        1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='m_'//trim(lumpname(i))
             if(m_fixed(i).ne.2)then
               if(m_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1360) trim(apar1),trim(atrans),m_par(i)
1360           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'1.0e-2    10.0     m         1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='l_'//trim(lumpname(i))
             if(l_fixed(i).ne.2)then
               if(l_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1361) trim(apar1),trim(atrans),l_par(i)
1361           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'0.1       1.0      l         1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='mfmax_'//trim(lumpname(i))
             if(mflowmax_fixed(i).ne.2)then
               if(mflowmax_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,13611) trim(apar1),trim(atrans),mflowmax_par(i)
13611          format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'0.01      2.0      mfmax     1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='offset_'//trim(lumpname(i))
             if(offset_fixed(i).ne.2)then
               if(offset_fixed(i).eq.0)then
                 atrans='none'
               else
                 atrans='fixed'
               end if
               write(20,1362) trim(apar1),trim(atrans),offset_par(i)
1362           format(a,t15,a,t25,'relative',t35,1pg14.7,t55,'-1000.0   10000.0  offset    1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='fac1_'//trim(lumpname(i))
             if(factor1_fixed(i).ne.2)then
               if(factor1_fixed(i).eq.0)then
                 atrans='log'                         ! Is this the best thing to do?
               else
                 atrans='fixed'
               end if
               write(20,1370) trim(apar1),trim(atrans),factor1_par(i)
1370           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'1.0e-5    10000.0  fac1      1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='fac2_'//trim(lumpname(i))
             if(factor2_fixed(i).ne.2)then
               if(factor2_fixed(i).eq.0)then
                 atrans='log'                         ! Is this the best thing to do?
               else
                 atrans='fixed'
               end if
               write(20,1380) trim(apar1),trim(atrans),factor2_par(i)
1380           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'1.0e-5    10000.0  fac2      1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='power_'//trim(lumpname(i))
             if(power_fixed(i).ne.2)then
               if(power_fixed(i).eq.0)then
                 atrans='none'
               else
                 atrans='fixed'
               end if
               write(20,1390) trim(apar1),trim(atrans),power_par(i)
1390           format(a,t15,a,t25,'relative',t35,1pg14.7,t55,'-3.0      3.0      power     1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='crfac_'//trim(lumpname(i))
             if(cropfac_fixed(i).ne.2)then
               if(cropfac_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1391) trim(apar1),trim(atrans),cropfac_par(i)
1391           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'0.001     3.0      crfac     1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='gamma_'//trim(lumpname(i))
             if(gamma_fixed(i).ne.2)then
               if(gamma_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1392) trim(apar1),trim(atrans),gamma_par(i)
1392           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'0.001     100.0    gamma     1.0   0.0  1')
             end if
           end do
           do i=1,i_lumpmod
             apar1='gwirfr_'//trim(lumpname(i))
             if(gwirrigfrac_fixed(i).ne.2)then
               if(gwirrigfrac_fixed(i).eq.0)then
                 atrans='log'
               else
                 atrans='fixed'
               end if
               write(20,1393) trim(apar1),trim(atrans),gwirrigfrac_par(i)
1393           format(a,t15,a,t25,'factor',  t35,1pg14.7,t55,'0.001     0.99     gwirfr    1.0   0.0  1')
             end if
           end do
           write(20,1400)
1400       format('* model input/output')
           do i=1,i_lumpmod
             aname=lumpname(i)
             tempfile='lr_'//trim(aname)//'.tpl'
             lumpfile='lr_'//trim(aname)//'.in'
             write(20,1410) trim(tempfile),trim(lumpfile)
1410         format(a,2x,a)
           end do
           write(20,1420)
1420       format('* model input/output')
           call addquote(batchfile,qfile)
           write(20,'(a)') trim(qfile)
           close(unit=20)
           write(6,190) trim(pestfile)

           write(6,180) trim(batchfile)
           outfile=batchfile
           open(unit=20,file=batchfile,action='write',err=9300)
           write(20,1425)
1425       format('REM LUMPREM output files are deleted.')
           write(20,*)
           do i=1,i_lumpmod
             outfile='lr_'//trim(lumpname(i))//'.out'
             write(20,1430) trim(outfile)
1430         format('del ',a)
           end do
           write(20,*)
           write(20,1435)
1435       format('REM LUMPREM models are run.')
           write(20,*)
           do i=1,i_lumpmod
             lumpfile='lr_'//trim(lumpname(i))//'.in'
             outfile='lr_'//trim(lumpname(i))//'.out'
             write(20,1440) trim(lumpfile),trim(outfile)
1440         format('lumprem ',a,' ',a)
           end do
           close(unit=20)
           write(6,190) trim(batchfile)

           i_pest=1

         case default
           go to 9050

         end select
       end do

1000   continue
       close(unit=10)
       write(6,*)
       write(6,1010) trim(infile)
1010   format(' - file ',a,' read ok')

2000   continue
       go to 9900

9000   write(amessage,9010) trim(aline),trim(infile)
9010   format('Insufficient items on line ',a,' of file ',a,'.')
       go to 9890

9030   write(amessage,9040) trim(keyword),trim(infile)
9040   format('The START_DATE and END_DATE keywords must be listed before the ',a,  &
       ' keyword in file ',a,'.')
       go to 9890

9050   call upcase(keyword)
       write(amessage,9060) trim(keyword),trim(aline),trim(infile)
9060   format('Unknown keyword "',a,'" at line ',a,' of file ',a,'.')
       go to 9890

9070   write(amessage,9080) trim(keyword),trim(aline),trim(infile)
9080   format('Cannot read number following ',a,' keyword on line ',a,' of file ',a,'.')
       go to 9890

9100   write(amessage,9110) trim(keyword),trim(aline),trim(infile)
9110   format('The ',a,' keyword can be supplied only once. Violation occurs at line ',a,' of file ',a,'.')
       go to 9890

9130   write(amessage,9140) trim(aline),trim(infile)
9140   format('Illegal date at line ',a,' of file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(keyword),trim(aline),trim(infile)
9160   format(a,' must not be negative at line ',a,' of file ',a,'.')
       go to 9890

9170   write(amessage,9180) trim(keyword),trim(aline),trim(infile)
9180   format(a,' must be positive at line ',a,' of file ',a,'.')
       go to 9890

9200   write(amessage,9210)
9210   format('Cannot allocate memory required to continue execution.')
       go to 9890

9250   write(amessage,9260) trim(kw),trim(keyword),trim(infile)
9260   format('A ',a,' keyword must precede a ',a,' keyword in file ',a,'.')
       go to 9890

9300   write(amessage,9310) trim(outfile)
9310   format('Cannot write to file ',a,'.')
       go to 9890


9890   amessage=' ' //amessage
       call writmess(6,amessage)

9900   continue

       deallocate(outday,rain,evap,stat=ierr)

       end



        integer function simlength(ifail,dds,mms,yys,dde,mme,yye,amessage)

! -- Function SIMLENGTH evaluates the total simulation time.

        implicit none

        integer, intent(out)           :: ifail
        integer, intent(in)            :: dds,mms,yys,dde,mme,yye
        character (len=*), intent(out) :: amessage

        integer                        :: numdays

        ifail=0

        simlength=numdays(dds,mms,yys,dde,mme,yye)
        if(simlength.lt.0)then
          write(amessage,10)
10        format('END_DATE of simulation precedes simulation START_DATE.')
          go to 9890
        end if
        simlength=simlength+1
        go to 9990

9890    ifail=1
9990    continue
        return
        end function simlength



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



        subroutine read_silo_file(ifail,ndays,dds,mms,yys,dde,mme,yye,rain,evap,evapcolstring,   &
        silofile,cline,amessage)

! -- Subroutine READ_SILO_FILE reads a silo climatic data file. It fills the rainfall and potential
!    evaporation arrays for the current case.

        implicit none
        integer, intent(out)  :: ifail
        integer, intent(in)   :: ndays
        integer, intent(in)   :: dds,mms,yys,dde,mme,yye
        real, intent(out)     :: rain(ndays),evap(ndays)
        character (len=*), intent(in)    :: evapcolstring
        character (len=*), intent(in)    :: silofile
        character (len=*), intent(inout) :: cline
        character (len=*), intent(out)   :: amessage

        integer              :: numdays
        integer              :: ierr,iline
        integer              :: iflag
        integer              :: yyo,mmo,ddo,yy,mm,dd
        integer              :: iday,jday,jdayold
        integer              :: raincol,evapcol,icol,ncol
        integer              :: lw(100),rw(100)
        double precision     :: dtemp
        character (len=10)   :: aline
        character (len=20)   :: atemp20
        character (len=1000) :: clineold

! -- Initialization

        ifail=0
        cline=' '

! -- Open the silo file.

        open(unit=11,file=silofile,status='old',action='read',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,10) trim(silofile)
10        format('Cannot open file ',a,' to read SILO climatic data.')
          go to 9890
        end if

! -- Look for the actual data.

        iline=0
        do
          iline=iline+1
          clineold=cline
          read(11,'(a)',end=9000) cline
          if(index(cline,'(yyyymmdd)').ne.0) exit
        end do

! -- We find the columns that we must read.

        call lowcase(clineold)
        if(clineold.eq.' ')then
          call writint(aline,iline-1)
          write(amessage,35) trim(aline),trim(silofile)
35        format('Line ',a,' of file ',a,' is not expected to be blank.')
          go to 9890
        end if
        call linesplit(ifail,1,lw,rw,clineold)
        if(clineold(lw(1):rw(1)).ne.'date') then
          call writint(aline,iline-1)
          write(amessage,30) trim(aline),trim(silofile)
30        format('First entry on line ',a,' of file ',a,' expected to be "Date".')
          go to 9890
        end if
        raincol=0
        evapcol=0
        do icol=1,100
          call linesplit(ifail,icol,lw,rw,clineold)
          if(ifail.ne.0)then
            if(raincol.eq.0)then
              write(amessage,20) trim(silofile)
20            format('Cannot find rain data column in file ',a,'.')
              go to 9890
            else if(evapcol.eq.0)then
              call writint(aline,iline)
              write(amessage,21) trim(evapcolstring),trim(aline),trim(silofile)
21            format('Cannot find EVAPCOLSTRING string "',a,'" on line ',a,' of file ',a,'.')
              go to 9890
            end if
          end if
          atemp20=clineold(lw(icol):rw(icol))
          if(atemp20.eq.'rain')then
            raincol=icol
          else if(trim(atemp20).eq.trim(evapcolstring))then
            evapcol=icol
          end if
          if((raincol.ne.0).and.(evapcol.ne.0))then
            ncol=icol
            go to 29
          end if
        end do
29      continue
        call lowcase(cline)
        call linesplit(ifail,ncol,lw,rw,cline)
        if(ifail.ne.0) go to 9150
        if(cline(lw(1):rw(1)).ne.'(yyyymmdd)')then
          call writint(aline,iline)
          write(amessage,40) trim(aline),trim(silofile)
40        format('First item on line ',a,' of file ',a,' expected to be "(yyyymmdd)".')
          go to 9890
        end if
        if(cline(lw(raincol):rw(raincol)).ne.'(mm)')then
          call writint(aline,iline)
          write(amessage,42) trim(aline),trim(silofile)
42        format('Rain expected to be in mm on line ',a,' of file ',a,'.')
          go to 9890
        end if
        if(cline(lw(evapcol):rw(evapcol)).ne.'(mm)')then
          call writint(aline,iline)
          write(amessage,44) trim(aline)
44        format('Evaporation expected to be in mm on line ',a,' of file ',a,'.')
          go to 9890
        end if

! -- Now we read data from the columns, but only for the days within the simulation.

        iflag=0
        jday=0
        yy=0
        mm=0
        dd=0
        do
          iline=iline+1
          read(11,'(a)',end=9100) cline
          if(cline.eq.' ') cycle
          call linesplit(ifail,1,lw,rw,cline)
          atemp20=cline(lw(1):rw(1))
          yyo=yy
          mmo=mm
          ddo=dd
          read(atemp20,50,err=9200) yy,mm,dd
50        format(i4,i2,i2)
          if(yy.lt.yys) cycle
          if((yy.eq.yys).and.(mm.lt.mms))cycle
          iday=numdays(dds,mms,yys,dd,mm,yy)
          jdayold=jday
          jday=iday+1
          if(jday.lt.1) cycle
          if(jday.ne.jdayold+1)then
            call writint(aline,iline)
            write(amessage,55) trim(aline),trim(silofile)
55          format('Days not in consecutive order at line ',a,' of file ',a,'.')
            go to 9890
          end if
          call linesplit(ifail,ncol,lw,rw,cline)
          if(ifail.ne.0) go to 9150
          call drealread(ifail,cline(lw(raincol):rw(raincol)),dtemp)
          if(ifail.ne.0)then
            call writint(aline,iline)
            write(amessage,70) trim(aline),trim(silofile)
70          format('Cannot read rain on line ',a,' of file ',a,'.')
            go to 9890
          end if
          rain(jday)=dtemp
          if(dtemp.lt.0.0)then
            call writint(aline,iline)
            write(amessage,71) trim(aline),trim(silofile)
71          format('Negative rain on line ',a,' of file ',a,'.')
            go to 9890
          end if
          call drealread(ifail,cline(lw(evapcol):rw(evapcol)),dtemp)
          if(ifail.ne.0)then
            call writint(aline,iline)
            write(amessage,80) trim(aline),trim(silofile)
80          format('Cannot read evaporation on line ',a,' of file ',a,'.')
            go to 9890
          end if
          evap(jday)=dtemp
          if(dtemp.lt.0.0)then
            call writint(aline,iline)
            write(amessage,81) trim(aline),trim(silofile)
81          format('Negative potential evaporation on line ',a,' of file ',a,'.')
            go to 9890
          end if
          if(jday.eq.ndays) exit
        end do
        go to 9990

9000    write(amessage,9010) trim(silofile)
9010    format('Cannot find daily climatic data in file ',a,'.')
        go to 9890

9100    write(amessage,9110) trim(silofile)
9110    format('Climate data in file ',a,' does not span entirety of LUMPREM simulation time.')
        go to 9890

9150    call writint(aline,iline)
        write(amessage,9160) trim(aline),trim(silofile)
9160    format('Insufficient entries on line ',a,' of file ',a,'.')
        go to 9890

9200    call writint(aline,iline)
        write(amessage,9210) trim(aline),trim(silofile)
9210    format('Cannot read date from first entry of line ',a,' of file ',a,'.')
        go to 9890

9890    ifail=1

9990    continue
        close(unit=11,iostat=ierr)
        return
        end


        subroutine write_lumprem_climate_file(ifail,ndays,cdata,cfile,amessage)

! -- Subroutine WRITE_LUMPREM_CLIMATE_FILE writes a rainfall or evaporation file for the use of LUMPREM.

        implicit none

        integer, intent(out)           :: ifail
        integer, intent(in)            :: ndays
        real, intent(in)               :: cdata(ndays)
        character (len=*), intent(in)  :: cfile
        character (len=*), intent(out) :: amessage

        integer                        :: iday,ierr

        ifail=0
        open(unit=20,file=cfile,action='write',err=9000)
        do iday=1,ndays
          write(20,20,err=9100) iday,cdata(iday)
20        format(1x,i10,4x,1pg14.7)
        end do
        go to 9990

9000    write(amessage,9010) trim(cfile)
9010    format('Cannot open file ',a,' for output.')
        go to 9890

9100    write(amessage,9110) trim(cfile)
9110    format('Cannot write to file ',a,'.')
        go to 9890

9890    ifail=1
9990    continue
        close(unit=20,iostat=ierr)

        end



        subroutine calc_outday(noutdays_approx,nday_out,noutdays,outday,ds,ms,ys,de,me,ye)

! -- Subroutine CALC_OUTDAY fills the OUTDAY array.

        implicit none

        integer, intent(in)  :: noutdays_approx
        integer, intent(in)  :: nday_out
        integer, intent(out) :: noutdays
        integer, intent(out) :: outday(noutdays_approx)
        integer, intent(in)  :: ds,ms,ys,de,me,ye

        logical              :: leap
        integer              :: numdays
        integer              :: nday_tot
        integer              :: nd,ii
        integer              :: dd,mm,yy

        nday_tot=numdays(ds,ms,ys,de,me,ye)
        nday_tot=nday_tot+1

        if(nday_out.gt.0)then
          nd=0
          ii=0
          do
            nd=nd+nday_out
            if(nd.lt.nday_tot)then
              ii=ii+1
              outday(ii)=nd
            else
              ii=ii+1
              outday(ii)=nday_tot
              noutdays=ii
              go to 9990
            end if
          end do
        else
          ii=0
          mm=ms
          yy=ys
20        continue
          if(ii.ge.1)then
            mm=mm+1
            if(mm.gt.12)then
              yy=yy+1
              mm=1
            end if
          end if
          if((mm.eq.9).or.(mm.eq.4).or.(mm.eq.6).or.(mm.eq.11))then
            dd=30
          else if(mm.eq.2)then
            if(leap(yy))then
              dd=29
            else
              dd=28
            end if
          else
            dd=31
          end if
          nd=numdays(ds,ms,ys,dd,mm,yy)
          nd=nd+1
          if(nd.lt.nday_tot)then
            ii=ii+1
            outday(ii)=nd
            go to 20
          else
            ii=ii+1
            outday(ii)=nday_tot
            noutdays=ii
            go to 9990
          end if
        end if

9990    continue

        return
        end


        subroutine getdate2(ifail,adate,dd,mm,yy)

! -- subroutine getdate2 is like getdate; but it does not destroy the adate
!    string.

        logical leap
        integer ifail,dd,mm,yy,n
        character*(*) adate
        character*(15) tdate

        ifail=0
        adate=adjustl(adate)
        tdate=adate(1:min(15,len(adate)))
        tdate=adjustl(tdate)
        n=index(tdate,'/')
        if(n.eq.0) go to 9999
        if(n.eq.1) go to 9999
        if(n.eq.2)then
          read(tdate(1:1),'(i1)',err=9999) dd
        else if(n.eq.3)then
          read(tdate(1:2),'(i2)',err=9999) dd
        else
          go to 9999
        end if
        tdate=tdate(n+1:)
        n=index(tdate,'/')
        if(n.eq.0) go to 9999
        if(n.eq.1) go to 9999
        if(n.eq.2) then
          read(tdate(1:1),'(i1)',err=9999) mm
        else if(n.eq.3) then
          read(tdate(1:2),'(i2)',err=9999) mm
        else
          go to 9999
        end if
        tdate=tdate(n+1:)
        n=len_trim(tdate)
        if(n.ne.4) go to 9999
        read(tdate(1:4),'(i4)',err=9999) yy

        if((mm.lt.1).or.(mm.gt.12)) go to 9999
        if((mm.eq.1).or.(mm.eq.3).or.(mm.eq.5).or.                   &
        (mm.eq.7).or.(mm.eq.8).or.(mm.eq.10).or.(mm.eq.12))then
          if(dd.gt.31) go to 9999
        else if((mm.eq.4).or.(mm.eq.6).or.(mm.eq.9).or.              &
        (mm.eq.11))then
          if(dd.gt.30) go to 9999
        else
          if(leap(yy)) then
            if(dd.gt.29) go to 9999
          else
            if(dd.gt.28) go to 9999
          end if
        end if

        return

9999    ifail=1
        return
        end



        logical function leap(year)

! -- Function LEAP returns .true. if a year is a leap year.

! -- Revision history:-
!       June-November, 1995: version 1.

        integer year

        leap = ( mod(year,4).eq.0 .and. mod(year,100).ne.0 ) .or.  &
        ( mod(year,400).eq.0 .and. year.ne.0 )

        return
        end



        integer function numdays(DR,MR,YR,D,M,Y)

! -- Function numdays calculates the number of days between dates
!    D-M-Y and DR-MR-YR. If the former preceeds the latter the answer is
!    negative.

! -- Arguments are as follows:-
!       dr,mr,yr:     days, months and years of first date
!       d,m,y:        days, months and years of second date
!       numdays returns the number of elapsed days


        integer, intent(in)     :: dr,mr,yr,d,m,y

        INTEGER FLAG,I,J,DA(12),YE,ME,DE,YL,ML,DL
        logical leap

        DATA DA /31,28,31,30,31,30,31,31,30,31,30,31/

! --    THE SMALLER OF THE TWO DATES IS NOW CHOSEN TO DO THE COUNTING FROM.

        IF(Y.LT.YR)GO TO 10
        IF((Y.EQ.YR).AND.(M.LT.MR)) GO TO 10
        IF((Y.EQ.YR).AND.(M.EQ.MR).AND.(D.LT.DR)) GO TO 10
        FLAG=0
        YE=YR
        ME=MR
        DE=DR
        YL=Y
        ML=M
        DL=D
        GO TO 20
10      FLAG=1
        YE=Y
        ME=M
        DE=D
        YL=YR
        ML=MR
        DL=DR

! --    IN THE ABOVE THE POSTSCRIPT "E" STANDS FOR EARLIER DATE, WHILE
!       "L" STANDS FOR THE LATER DATE.

20      numdays=0
        IF((ME.EQ.ML).AND.(YL.EQ.YE))THEN
        numdays=DL-DE
        IF(FLAG.EQ.1) numdays=-numdays
        RETURN
        END IF

        DO 30 J=ME,12
        IF((ML.EQ.J).AND.(YE.EQ.YL))GOTO 40
        numdays=numdays+DA(J)
        IF((J.EQ.2).AND.(leap(ye)))numdays=numdays+1
30      CONTINUE
        GO TO 50
40      numdays=numdays+DL-DE
        IF(FLAG.EQ.1)numdays=-numdays
        RETURN

50      DO 60 I=YE+1,YL
        DO 70 J=1,12
        IF((YL.EQ.I).AND.(ML.EQ.J))GO TO 80
        numdays=numdays+DA(J)
        IF((J.EQ.2).AND.(leap(i))) numdays=numdays+1
70      CONTINUE
60      CONTINUE
        call sub_error('NUMDAYS')
        RETURN

80      numdays=numdays+DL-DE
        IF(FLAG.EQ.1) numdays=-numdays

        RETURN
        end


        subroutine sub_error(subname)

! -- Subroutine sub_error names the subroutine causing a run-time error.

! -- Arguments are as follows:-
!       subname:  name of offending subroutine


        character (len=*)               ::subname

        write(6,10) trim(subname)
10      format(/,' *** PROGRAMMING ERROR CALLING SUBROUTINE ',a,' ***')
        stop

       end



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


      subroutine addquote(afile,aqfile)

! -- Subroutine ADDQUOTE adds quotes to a filename if it has a space in it.

! -- Arguments are as follows:-
!        afile:       the name of the file
!        aqfile:      the name of the file with quotes added

        character (len=*), intent(in)   :: afile
        character (len=*), intent(out)  :: aqfile
        integer nbb

        if(index(trim(afile),' ').eq.0)then
          aqfile=afile
        else
          aqfile(1:1)='"'
          aqfile(2:)=trim(afile)
          nbb=len_trim(aqfile)+1
          aqfile(nbb:nbb)='"'
        end if

        return
      end subroutine addquote
