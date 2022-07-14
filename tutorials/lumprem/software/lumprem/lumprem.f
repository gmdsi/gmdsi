        program lumprem

        implicit none

        integer maxdelay
        parameter(maxdelay=500)

        integer i,numdrain,nummac,nstep,mxiter,numdays,noutdays,
     +  ierr,iline,iveg,nveg,idayold,ifin,imod,iday1,iday2,
     +  iday
        integer itemp
        integer ifile,ifail
        integer irrigcode_all
        integer narg
        integer lw(10),rw(10)
        double precision maxvol,rdelay,mdelay,ks,m,l,mflowmax,vol
        double precision tol,rtemp,
     +  cropfac1,gamma1,recharge,mrecharge,runoff,
     +  evapn,rainfall,rbal,deltav,oldvol,totd,totm,ototd,ototm,
     +  totrain,totevpn,totrech,totmrech,tottrech,totrun,totbal,
     +  potevapn,totepot,totirrigation,totgwithdrawal,totresidepot,
     +  totnetrecharge
        double precision irrigation,gwithdrawal,irrigvolfrac
        double precision dfac,cdiff,gdiff,dday1,dday2,ddiff,iddiff
        double precision offset,factor1,factor2,power,dtemp,elevation
        double precision elevmin,elevmax
        double precision datum,dwt
        double precision cropfac_all,gamma_all
        double precision gwirrigfrac_all
        double precision drainsub(maxdelay),macsub(maxdelay)
        character*1 aa
        character*10 aline
        character*100 atext
        character*256 infle,outfle,cgfle,rainfle,epotfle,irrigfle,afile
        character*256 cline
        character*1000 errmsg

        integer, allocatable :: ioutday(:),vday(:)
        integer, allocatable :: irrigcode(:)
        double precision, allocatable :: cropfac(:),gamma(:),rain(:),
     +  epot(:)
        double precision, allocatable :: cropfac_day(:),gamma_day(:)
        double precision, allocatable :: gwirrigfrac(:)

C -- Obtain names of input files.

        narg=iargc()
        if(narg.eq.1)then
          write(errmsg,9)
9         format('Two command-line arguments are required or none.')
          go to 9998
        else if(narg.ge.2)then
          call getarg(1,infle)
          call getarg(2,outfle)
          open(unit=10,file=infle,status='old',action='read',
     +    iostat=ierr)
          if(ierr.ne.0)then
            write(errmsg,15) trim(infle)
15          format('Cannot open file ',a,' to read LUMPREM2 ',
     +      'input dataset.')
            go to 9998
          end if
        else if(narg.eq.0)then
40        write(6,50,advance='no')
50        format(' Enter name of LUMPREM2 control file: ')
          read(5,'(a)') infle
          if(infle.eq.' ') go to 40
          open(unit=10,file=infle,status='old',action='read',err=40)
69        write(6,70,advance='no')
70        format(' Enter name for LUMPREM2 output file: ')
          read(5,'(a)') outfle
          if(outfle.eq.' ') go to 69
          write(6,*)
        end if

C -- Initialize accumulators.

        totrain=0.0
        totevpn=0.0
        totrech=0.0
        totmrech=0.0
        tottrech=0.0
        totrun=0.0
        totbal=0.0
        totepot=0.0
        totirrigation=0.0
        totgwithdrawal=0.0
        totresidepot=0.0
        totnetrecharge=0.0

C -- Initialize certain variables.

        do i=1,maxdelay
          drainsub(i)=0.0
          macsub(i)=0.0
        end do

C -- Read soil properties.

        iline=1
        read(10,'(a)',err=9400,end=9370) cline
        call lowcase(cline)
        if(cline(1:7).ne.'* earth')then
          atext='* earth properties'
          go to 9550
        end if
        iline=iline+1
        read(10,*,err=9400,end=9370) maxvol,irrigvolfrac
        if((irrigvolfrac.gt.1.0).or.(irrigvolfrac.lt.0.0))then
          write(errmsg,10)
10        format('Error: IRRIGVOLFRAC must not be greater than 1.0 ',
     +    'or less than 0.0.')
          go to 9998
        end if
        iline=iline+1
        read(10,*,err=9400,end=9370) rdelay,mdelay
        iline=iline+1
        read(10,*,err=9400,end=9370) ks,m,l,mflowmax

C -- Read volume to elevation conversion parameters

        iline=iline+1
        read(10,'(a)',err=9400,end=9370) cline
        call lowcase(cline)
        if(cline(1:8).ne.'* volume') then
          atext='* volume to elevation'
          go to 9550
        end if
        iline=iline+1
        read(10,'(a)',err=9400,end=9370) cline
        call linesplit(ifail,4,lw,rw,cline)
        if(ifail.ne.0) go to 9350
        call drealread(ifail,cline(lw(1):rw(1)),offset)
        if(ifail.ne.0) go to 9400
        call drealread(ifail,cline(lw(2):rw(2)),factor1)
        if(ifail.ne.0) go to 9400
        call drealread(ifail,cline(lw(3):rw(3)),factor2)
        if(ifail.ne.0) go to 9400
        call drealread(ifail,cline(lw(4):rw(4)),power)
        if(ifail.ne.0) go to 9400
        call linesplit(ifail,5,lw,rw,cline)
        if(ifail.ne.0)then
          elevmin=-1.0d20
          elevmax=1.0d20
        else
          call drealread(ifail,cline(lw(5):rw(5)),elevmin)
          if(ifail.ne.0) go to 9400
          call linesplit(ifail,6,lw,rw,cline)
          if(ifail.ne.0)then
            elevmax=1.0d20
          else
            call drealread(ifail,cline(lw(6):rw(6)),elevmax)
            if(ifail.ne.0) go to 9400
          end if
        end if
        if(elevmin.gt.elevmax)then
          call writint(aline,iline)
          write(errmsg,11) trim(aline),trim(infle)
11        format('Minimum elevation is greater than maximum ',
     +    'elevation at line ',a,' of file ',a,'.')
          go to 9998
        end if
        iline=iline+1
        read(10,'(a)',err=9400,end=9370) cline
        call lowcase(cline)
        if(cline(1:6).ne.'* topo') then
          atext='* topographic surface'
          go to 9550
        end if
        iline=iline+1
        read(10,*,err=9400,end=9370) datum

C -- Read initial conditions.

        iline=iline+1
        read(10,'(a)',err=9400,end=9370) cline
        call lowcase(cline)
        if(cline(1:6).ne.'* init') then
          atext='* initial conditions'
          go to 9550
        end if
        iline=iline+1
        read(10,*,err=9400,end=9370) vol
        iline=iline+1
        read(10,*,err=9400,end=9370) numdrain,nummac
        if((numdrain.gt.maxdelay).or.(nummac.gt.maxdelay)) go to 9000
        iline=iline+1
        read(10,*,err=9420,end=9420) (drainsub(i),i=1,numdrain)
        iline=iline+1
        read(10,*,err=9440,end=9440) (macsub(i),i=1,nummac)
        totd=0.0
        totm=0.0
        do 150 i=1,maxdelay
          totd=totd+drainsub(i)
          totm=totm+macsub(i)
150     continue

C -- Read solution parameters.

        iline=iline+1
        read(10,'(a)',err=9400,end=9370) cline
        call lowcase(cline)
        if(cline(1:7).ne.'* solut') then
          atext='* solution parameters'
          go to 9550
        end if
        iline=iline+1
        read(10,*,err=9460,end=9460) nstep,mxiter,tol

C -- Read timing information

        iline=iline+1
        read(10,'(a)',err=9400,end=9370) cline
        call lowcase(cline)
        if(cline(1:8).ne.'* timing') then
          atext='* timing information'
          go to 9550
        end if
        iline=iline+1
        read(10,*,err=9480,end=9480) numdays,noutdays
        allocate(rain(numdays),epot(numdays),
     +  ioutday(noutdays),stat=ierr)
        if(ierr.ne.0) go to 9050
        allocate(cropfac_day(numdays),gamma_day(numdays),stat=ierr)
        if(ierr.ne.0) go to 9050
        allocate(irrigcode(numdays),gwirrigfrac(numdays),stat=ierr)
        if(ierr.ne.0) go to 9050
        iline=iline+1
        read(10,*,err=9480,end=9480) (ioutday(i),i=1,noutdays)

C -- Read input data filenames. For two of these files parameter values may be provided instead of filenames.

        iline=iline+1
        read(10,'(a)',err=9400,end=9370) cline
        call lowcase(cline)
        if(cline(1:6).ne.'* data') then
          atext='* data filenames'
          go to 9550
        end if

C -- Either a filename or crop factor and gamma values may be read from the next line.

        cgfle=' '
        iline=iline+1
        read(10,'(a)',end=9370) cline
        cline=adjustl(cline)
        ifile=0
        if((cline(1:1).eq.'''').or.(cline(1:1).eq.'"'))ifile=1
        if(ifile.eq.0)then
          call linesplit(ifail,2,lw,rw,cline)
          if(ifail.ne.0)then
            ifile=1
            go to 155
          else
            call drealread(ifail,cline(lw(1):rw(1)),cropfac_all)
            if(ifail.ne.0) then
              ifile=1
              go to 155
            end if
            call drealread(ifail,cline(lw(2):rw(2)),gamma_all)
            if(ifail.ne.0)then
              write(errmsg,153)
153           format('Error: cannot read GAMMA value from LUMPREM2 ',
     +        'control file.')
              go to 9998
            end if
          end if
        end if
155     continue
        if(ifile.eq.1)then
          aa=cline(1:1)
          if((aa.eq.'''').or.(aa.eq.'"'))then
            do i=1,len_trim(cline)
              if(cline(i:i).eq.aa) cline(i:i)=' '
            end do
            cline=adjustl(cline)
          end if
          cgfle=cline
        end if

        iline=iline+1
        read(10,'(a)',err=9500,end=9500) rainfle
        iline=iline+1
        read(10,'(a)',err=9520,end=9520) epotfle

C -- Either a filename or irrigation code and groundwater extraction fraction may be read from the next line.

        irrigfle=' '
        iline=iline+1
        read(10,'(a)',end=9370) cline
        cline=adjustl(cline)
        ifile=0
        if((cline(1:1).eq.'''').or.(cline(1:1).eq.'"'))ifile=1
        if(ifile.eq.0)then
          call linesplit(ifail,2,lw,rw,cline)
          if(ifail.ne.0)then
            ifile=1
            go to 158
          else
            call intread(ifail,cline(lw(1):rw(1)),irrigcode_all)
            if(ifail.ne.0) then
              ifile=1
              go to 158
            end if
            call drealread(ifail,cline(lw(2):rw(2)),gwirrigfrac_all)
            if(ifail.ne.0)then
              write(errmsg,156)
156           format('Error: cannot read GWIRRIGFRAC value from ',
     +        'LUMPREM2 control file.')
              go to 9998
            end if
          end if
        end if
158     continue
        if(ifile.eq.1)then
          aa=cline(1:1)
          if((aa.eq.'''').or.(aa.eq.'"'))then
            do i=1,len_trim(cline)
              if(cline(i:i).eq.aa) cline(i:i)=' '
            end do
            cline=adjustl(cline)
          end if
          irrigfle=cline
        end if

        close(unit=10)
        write(6,159) trim(infle)
159     format(' - file ',a,' read ok.')

C -- Open the vegetation and climate files.

        if(cgfle.ne.' ')
     +  open(unit=10,file=cgfle,status='old',err=9150)
        open(unit=11,file=rainfle,status='old',err=9200)
        open(unit=12,file=epotfle,status='old',err=9250)
        if(irrigfle.ne.' ')
     +  open(unit=13,file=irrigfle,status='old',err=9300)

C -- The vegetation file is read.

        if(cgfle.eq.' ')then
          do iday=1,numdays
            cropfac_day(iday)=cropfac_all
          end do
          do iday=1,numdays
            gamma_day(iday)=gamma_all
          end do
          go to 475
        end if

        iline=1
330     continue
          read(10,*,end=350)
          iline=iline+1
        go to 330
350     allocate(vday(iline),cropfac(iline),gamma(iline),stat=ierr)
        if(ierr.ne.0) then
          write(errmsg,360)
360       format('Cannot allocate sufficient memory to continue ',
     +    'exection.')
          go to 9998
        endif
        rewind(unit=10)
        iveg=0
        iline=0
        afile=cgfle
380     continue
          iline=iline+1
          read(10,'(a)',end=450) cline
          if(cline.eq.' ') go to 380
          iveg=iveg+1
          call linesplit(ifail,3,lw,rw,cline)
          if(ifail.ne.0) go to 9590
          call intread(ifail,cline(lw(1):rw(1)),vday(iveg))
          if(ifail.ne.0) go to 9570
          call drealread(ifail,cline(lw(2):rw(2)),cropfac(iveg))
          if(ifail.ne.0) go to 9570
          call drealread(ifail,cline(lw(3):rw(3)),gamma(iveg))
          if(ifail.ne.0) go to 9570
          if(iveg.eq.1)then
            if(vday(iveg).ne.1) then
              write(errmsg,400)
400           format('Error: first day cited in vegetation file must ',
     +        'be day one of simulation.')
              go to 9998
            end if
          else
            if(vday(iveg).le.vday(iveg-1))then
              write(errmsg,401)
401           format('Error: consecutive entries in vegetation file ',
     +        'pertain to non-increasing days.')
              go to 9998
            end if
          end if
        go to 380
450     close (unit=10)
        nveg=iveg
        if(vday(nveg).lt.numdays)then
          write(errmsg,470)
470       format('Error: last day cited in vegetation file must ',
     +    'be greater than or equal to model simulation time.')
          go to 9998
        end if

C -- We write vegetation data to daily arrays.

        if(nveg.eq.1)then
          do iday=1,numdays
           cropfac_day(iday)=cropfac(1)
          end do
          do iday=1,numdays
           gamma_day(iday)=gamma(1)
          end do
        else
          do iveg=1,nveg-1
            dday1=dble(vday(iveg))
            dday2=dble(vday(iveg+1))
            cropfac1=cropfac(iveg)
            gamma1=gamma(iveg)
            ddiff=dday2-dday1
            iddiff=1.0d0/ddiff
            cdiff=cropfac(iveg+1)-cropfac(iveg)
            gdiff=gamma(iveg+1)-gamma(iveg)
            do iday=vday(iveg),vday(iveg+1)
              if(iday.lt.1) cycle
              if(iday.gt.numdays) go to 475
              dfac=(dble(iday)-dday1)*iddiff
              cropfac_day(iday)=cropfac1+cdiff*dfac
              gamma_day(iday)=gamma1+gdiff*dfac
            end do
          end do
        end if
475     continue

C -- The rainfall file is read.

        afile=rainfle
        iline=0
        idayold=0
550     continue
          iline=iline+1
          read(11,'(a)',end=700) cline
          if(cline.eq.' ') go to 550
          call linesplit(ifail,2,lw,rw,cline)
          if(ifail.ne.0) go to 9590
          call intread(ifail,cline(lw(1):rw(1)),iday)
          if(ifail.ne.0) go to 9570
          call drealread(ifail,cline(lw(2):rw(2)),rtemp)
          if(ifail.ne.0) go to 9570
          if(iday.lt.1) then
            write(errmsg,570)
570         format('Error: zero or negative day cited in ',
     +      'rainfall file.')
            go to 9998
          end if
          if(iday.le.idayold)then
            write(errmsg,590)
590         format('Error: days are not in correct order in rainfall ',
     +      'file.')
            go to 9998
          end if
          if(iday.le.numdays) rain(iday)=rtemp
          if(idayold.lt.numdays)then
            if(idayold.lt.iday-1)then
              do 610 i=idayold+1,min(iday-1,numdays)
                rain(i)=0.0
610           continue
            end if
          end if
          idayold=iday
        go to 550
700     close(unit=11)
        if(idayold.lt.numdays)then
          do 750 i=idayold+1,numdays
            rain(i)=0.0
750       continue
        end if

C -- The evaporation file is read.

        iline=0
        idayold=0
        afile=epotfle
850     continue
          iline=iline+1
          read(12,'(a)',end=1000) cline
          if(cline.eq.' ') go to 850
          call linesplit(ifail,2,lw,rw,cline)
          if(ifail.ne.0) go to 9590
          call intread(ifail,cline(lw(1):rw(1)),iday)
          if(ifail.ne.0) go to 9570
          call drealread(ifail,cline(lw(2):rw(2)),rtemp)
          if(ifail.ne.0) go to 9570
          if(iday.lt.1) then
            write(errmsg,870)
870         format('Error: zero or negative day in evaporation file.')
            go to 9998
          end if
          if(iline.eq.1)then
            if(iday.ne.1) then
              write(errmsg,880)
880          format('Error: first line of evaporation file must pertain',
     +       ' to day one.')
              go to 9998
            end if
          end if
          if(iday.le.idayold)then
            write(errmsg,890)
890         format('Error: days are not in correct order in ',
     +      'evaporation file.')
            go to 9998
          end if
          if(iday.le.numdays) epot(iday)=rtemp
          if(iday.eq.1) go to 920
          if(idayold.le.numdays)then
            if(idayold.lt.iday-1) then
              rtemp=epot(idayold)
              do 910 i=idayold+1,min(iday-1,numdays)
                epot(i)=rtemp
910           continue
            end if
          end if
920       idayold=iday
        go to 850
1000    close(unit=12)
        if(idayold.lt.numdays)then
          rtemp=epot(idayold)
          do 1050 i=idayold+1,numdays
            epot(i)=rtemp
1050      continue
        end if

C -- The irrigation file is read.

        if(irrigfle.eq.' ')then
          do iday=1,numdays
            irrigcode(iday)=irrigcode_all
          end do
          do iday=1,numdays
            gwirrigfrac(iday)=gwirrigfrac_all
          end do
          go to 2310
        end if

        iline=0
        idayold=0
        afile=irrigfle
2200    continue
          iline=iline+1
          read(13,'(a)',end=2300) cline
          if(cline.eq.' ') go to 2200
          call linesplit(ifail,3,lw,rw,cline)
          if(ifail.ne.0) go to 9590
          call intread(ifail,cline(lw(1):rw(1)),iday)
          if(ifail.ne.0) go to 9570
          call intread(ifail,cline(lw(2):rw(2)),itemp)
          if(ifail.ne.0) go to 9570
          call drealread(ifail,cline(lw(3):rw(3)),dtemp)
          if(ifail.ne.0) go to 9570
          if(iday.lt.1) then
            write(errmsg,2210)
2210        format('Error: zero or negative day in irrigation file.')
            go to 9998
          end if
          if(iline.eq.1)then
            if(iday.ne.1) then
              write(errmsg,2220)
2220          format('Error: first line of irrigation file must pertain',
     +        ' to day one.')
              go to 9998
            end if
          end if
          if(iday.le.idayold)then
            write(errmsg,2230)
2230        format('Error: days are not in correct order in ',
     +      'irrigation file.')
            go to 9998
          end if
          if(iday.le.numdays) then
            irrigcode(iday)=itemp
            gwirrigfrac(iday)=dtemp
          end if
          if((itemp.ne.0).and.(itemp.ne.1))then
            call writint(aline,iline)
            write(errmsg,2240) trim(aline),trim(irrigfle)
2240        format('Irrigation code must be zero or 1 at line ',a,
     +      ' of file ',a,'.')
            go to 9998
          end if
          if((dtemp.lt.0.0).or.(dtemp.gt.1.0))then
            call writint(aline,iline)
            write(errmsg,2250) trim(aline),trim(irrigfle)
2250        format('Groundwater irrigation source fraction must ',
     +      'not be less than zero or greater than one at line ',a,
     +      ' of file ',a,'.')
            go to 9998
          end if
          if(iday.eq.1) go to 2290
          if(idayold.le.numdays)then
            if(idayold.lt.iday-1) then
              itemp=irrigcode(idayold)
              dtemp=gwirrigfrac(idayold)
              do i=idayold+1,min(iday-1,numdays)
                irrigcode(i)=itemp
                gwirrigfrac(i)=dtemp
              end do
            end if
          end if
2290      continue
          idayold=iday
        go to 2200
2300    close(unit=13)
        if(idayold.lt.numdays)then
          itemp=irrigcode(idayold)
          dtemp=gwirrigfrac(idayold)
          do i=idayold+1,numdays
            irrigcode(i)=itemp
            gwirrigfrac(i)=dtemp
          end do
        end if
2310    continue

C -- The header is written to the LUMPREM2 output file and initial conditions
C    are recorded.

        dtemp=min(vol,maxvol)
        if(dtemp.lt.1.0e-10) dtemp=1.0e-10                       ! arbitrary - needed for a negative power
        elevation=offset+factor1*dtemp+factor2*(dtemp**power)
        if(elevation.lt.elevmin) elevation=elevmin
        if(elevation.gt.elevmax) elevation=elevmax
        dwt=datum-elevation
        open(unit=10,file=outfle,action='write',iostat=ierr)
        if(ierr.ne.0)then
          write(errmsg,1249) trim(outfle)
1249      format('Cannot write to file ',a,'.')
          go to 9998
        end if
        write(10,1250)
1250    format('  days     volume       vol_drain     ',
     +  'vol_macro      delta_vol      del_vol_drain  ',
     +  'del_vol_macro  ',
     +  'rainfall       irrigation     recharge       ',
     +  'macro_rech     total_rech     gw_withdrawal  ',
     +  'net_recharge   runoff         ',
     +  'pot_evap       evaporation    gw_pot_evap     balance        ',
     +  'elevation      depth-to-water')
        write(10,1260) 0,vol,totd,totm,0.0,0.0,0.0,0.0,0.0,0.0,
     +  0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,elevation,dwt
1260    format(1x,i5,20(1x,g14.7))

C -- Subroutine RECHMOD is called once for every time at which output is
C    required.

        ifin=0
        imod=0
        oldvol=vol
        ototd=totd
        ototm=totm
1300    continue
          imod=imod+1
          if(imod.gt.noutdays) go to 2000
          if(imod.eq.1)then
            iday1=0
          else
            iday1=idayold
          end if
          iday2=ioutday(imod)
          if(iday2.ge.numdays)then
            iday2=numdays
            ifin=1
          end if
1600      call rechmod(mxiter,tol,iday2-iday1,maxdelay,nstep,maxvol,
     +    irrigvolfrac,cropfac_day(iday1+1),gamma_day(iday1+1),ks,l,m,
     +    mflowmax,rdelay,
     +    mdelay,vol,drainsub,macsub,rain(iday1+1),epot(iday1+1),
     +    recharge,mrecharge,runoff,evapn,rainfall,potevapn,
     +    irrigation,gwithdrawal,
     +    irrigcode(iday1+1),gwirrigfrac(iday1+1))

C -- Some aspects of the water balance are calculated.

          deltav=vol-oldvol
          totd=0.0
          totm=0.0
          do 1700 i=1,maxdelay
            totd=totd+drainsub(i)
            totm=totm+macsub(i)
1700      continue
        rbal=rainfall+irrigation-deltav-recharge-mrecharge-runoff-evapn
     +    -(totd-ototd)-(totm-ototm)

C -- Data is written to the output file.

          dtemp=min(vol,maxvol)
          if(dtemp.lt.1.0e-10) dtemp=1.0e-10                       ! arbitrary - needed for a negative power
          elevation=offset+factor1*dtemp+factor2*(dtemp**power)
          if(elevation.lt.elevmin) elevation=elevmin
          if(elevation.gt.elevmax) elevation=elevmax
          dwt=datum-elevation
          write(10,1260) iday2,vol,totd,totm,deltav,(totd-ototd),
     +    (totm-ototm),rainfall,irrigation,recharge,
     +    mrecharge,recharge+mrecharge,gwithdrawal,
     +    recharge+mrecharge-gwithdrawal,runoff,
     +    potevapn,evapn,potevapn-evapn,rbal,elevation,dwt
          idayold=iday2

C -- Accumulators are updated.

          totrain=totrain+rainfall
          totrech=totrech+recharge
          totmrech=totmrech+mrecharge
          tottrech=tottrech+recharge+mrecharge
          totrun=totrun+runoff
          totevpn=totevpn+evapn
          totbal=totbal+rbal
          totepot=totepot+potevapn
          totresidepot=totresidepot+potevapn-evapn
          totirrigation=totirrigation+irrigation
          totgwithdrawal=totgwithdrawal+gwithdrawal
          totnetrecharge=tottrech-totgwithdrawal
          oldvol=vol
          ototd=totd
          ototm=totm
          if(ifin.eq.1) go to 2000
        go to 1300

C -- Grand totals are recorded.

2000    write(10,*)
        write(10,1800) totrain,totirrigation,totrech,totmrech,
     +  tottrech,totgwithdrawal,totnetrecharge,totrun,
     +  totepot,totevpn,totresidepot,totbal
1800    format(1x,'total',6(1x,14x),12(1x,g14.7))
        close(unit=10)
        write(6,2050) trim(outfle)
2050    format(' - file ',a,' written ok.')
        go to 9999

C -- Some error messages.

9000    write(errmsg,9010)
9010    format('Too many entries in subsurface delay buffers: ',
     +  'redimension MAXDELAY and recompile LUMPREM2.')
        go to 9998
9050    write(errmsg,9060)
9060    format('Insufficient memory for LUMPREM2 execution.')
        go to 9998
9150    write(errmsg,9160)
9160    format('Cannot open vegetation file.')
        go to 9998
9200    write(errmsg,9210)
9210    format('Cannot open rainfall file.')
        go to 9998
9250    write(errmsg,9260)
9260    format('Cannot open evaporation file.')
        go to 9998
9300    write(errmsg,9310)
9310    format('Cannot open irrigation file.')
        go to 9998
9350    continue
        call writint(aline,iline)
        write(errmsg,9360) trim(aline),trim(infle)
9360    format('Insufficient entries on line ',a,' of file ',a,'.')
        go to 9998
9370    continue
        write(errmsg,9380) trim(infle)
9380    format('Premature end encountered to file ',a,'.')
        go to 9998
9400    continue
        call writint(aline,iline)
        write(errmsg,9410) trim(aline),trim(infle)
9410    format('Error reading data on line ',a,' of file ',a,'.')
        go to 9998
9420    write(errmsg,9430) trim(infle)
9430    format('Error reading initial contents of recharge ',
     +  'delay buffer from file ',a,'.')
        go to 9998
9440    write(errmsg,9450) trim(infle)
9450    format('Error reading initial contents of macropore ',
     +  'recharge delay buffer from file ',a,'.')
        go to 9998
9460    write(errmsg,9470) trim(infle)
9470    format('Error reading solution parameters from file ',a,'.')
        go to 9998
9480    write(errmsg,9490) trim(infle)
9490    format('Error reading timing information from file ',a,'.')
        go to 9998
9500    write(errmsg,9510) trim(infle)
9510    format('Error encountered reading name of rainfall file ',
     +  'from file ',a,'.')
        go to 9998
9520    write(errmsg,9530) trim(infle)
9530    format('Error encountered reading name of potential ',
     +  'evaporation file from file ',a,'.')
        go to 9998
9550    call writint(aline,iline)
        write(errmsg,9560) trim(atext),trim(aline),trim(infle)
9560    format('Text string "',a,'" expected on or about line ',a,
     +  ' of file ',a,'.')
        go to 9998
9570    call writint(aline,iline)
        write(errmsg,9580) trim(aline),trim(afile)
9580    format('Error reading data on line ',a,' of file ',a,'.')
        go to 9998
9590    call writint(aline,iline)
        write(errmsg,9595) trim(aline),trim(afile)
9595    format('Insufficient entries on line ',a,' of file ',a,'.')
        go to 9998

9998    call writerr(errmsg)

9999    continue
        deallocate(ioutday,vday,cropfac,gamma,rain,epot,stat=ierr)
        deallocate(cropfac_day,gamma_day,stat=ierr)
        deallocate(irrigcode,gwirrigfrac,stat=ierr)

        end


        SUBROUTINE WRITERR(ERRMSG)

C -- Subroutine WRITERR formats and writes an error message.

        INTEGER J,JEND,I,NBLC,ITAKE
        CHARACTER*(*) ERRMSG

        write(6,*)
        ITAKE=0
        J=0
        NBLC=len_trim(ERRMSG)
5       JEND=J+78-ITAKE
        IF(JEND.GE.NBLC) GO TO 100
        DO 10 I=JEND,J+1,-1
        IF(ERRMSG(I:I).EQ.' ') THEN
          IF(ITAKE.EQ.0) THEN
            WRITE(6,'(A)',ERR=200) ' '//ERRMSG(J+1:I)
            ITAKE=4
          ELSE
            WRITE(6,'(A)',ERR=200) '     '//ERRMSG(J+1:I)
          END IF
          J=I
          GO TO 5
        END IF
10      CONTINUE
        IF(ITAKE.EQ.0)THEN
          WRITE(6,'(A)',ERR=200) ' '//ERRMSG(J+1:JEND)
          ITAKE=4
        ELSE
          WRITE(6,'(A)',ERR=200) '     '//ERRMSG(J+1:JEND)
        END IF
        J=JEND
        GO TO 5
100     JEND=NBLC
        IF(ITAKE.EQ.0)THEN
          WRITE(6,'(A)',ERR=200) ' '//ERRMSG(J+1:JEND)
        ELSE
          WRITE(6,'(A)',ERR=200) '     '//ERRMSG(J+1:JEND)
        END IF
        RETURN

200     STOP
        END



        subroutine writint(atemp,ival)

c       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
        end



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



        SUBROUTINE INTREAD(IFAIL,CLINE,iTEMP)

C -- Subroutine INTREAD reads a real number from a string.

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
        END




         SUBROUTINE LINESPLIT(IFAIL,NUM,LW,RW,CLINE)

C -- Subroutine LINESPLIT splits a string into blank-delimited fragments.

        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM,NBLNK
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=LEN_TRIM(CLINE)
        IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
          CALL TABREM(CLINE)
          NBLC=LEN_TRIM(CLINE)
        ENDIF
        IF(NBLC.EQ.0) THEN
          IFAIL=-1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
          IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.
     +    (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LW(NW)=I
        DO 30 I=LW(NW)+1,NBLC
          IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.
     +    (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30      CONTINUE
        RW(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RW(NW)=I-1
        J=RW(NW)
        GO TO 5

        END


        SUBROUTINE TABREM(CLINE)

C -- Subroutine TABREM removes tabs from a string.

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
        END


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

