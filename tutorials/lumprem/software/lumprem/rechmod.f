        subroutine rechmod(mxiter,tol,ndays,subdim,nstep,maxvol,
     +  irrigvolfrac,cropfac_day,gamma_day,ks,l,m,mflowmax,rdelay,
     +  mdelay,vol,drainsub,macsub,rain,epot,recharge,mrecharge,
     +  runoff,evapn,rainfall,potevapn,irrigation,gwithdrawal,
     +  irrigcode,gwirrigfrac)

        implicit none

C -- Subroutine arguments are first declared.

        integer ndays,subdim,nstep,mxiter
        integer irrigcode(ndays)
        double precision maxvol,irrigvolfrac,ks,l,m,
     +  mflowmax,vol,recharge,mrecharge,runoff,rdelay,mdelay,
     +  evapn,tol,drainage,evap,rainfall,potevapn
        double precision irrigation,gwithdrawal,vvol
        double precision rain(ndays),epot(ndays),drainsub(subdim)
        double precision macsub(subdim)
        double precision cropfac_day(ndays),gamma_day(ndays)
        double precision gwirrigfrac(ndays)

C -- Other variables are declared.

        integer irdelay,imdelay,i,iday,istep,iter
        integer ircode
        double precision train,tstep,tepot,cropfac,dayrech,daymrech,
     +  dayevap,
     +  dayrunoff,tvol,vd,otvol,rtemp1,rtemp2,tempvol,frdelay,
     +  fmdelay,gamma
        double precision gwfrac,dayirrigation,daygwithdrawal,dirrig,
     +  odirrig

c	write(99,2)						!debug
c2	format(' Entering subroutine RECHMOD')			!debug
c	do i=1,ndays						!debug
c	  write(99,1) rain(i),epot(i),                          !debug
c     +               cropfac_day(i),gamma_day(i)               !debug
c 1       format(4(1x,1pg14.7))                                 !debug
c	end do							!debug
c	write(99,*)						!debug


C -- Initialization

        vvol=irrigvolfrac*maxvol

C -- Variables accumulated over a single RECHMOD call are zeroed.

        rainfall=0.0
        recharge=0.0
        mrecharge=0.0
        runoff=0.0
        evapn=0.0
        potevapn=0.0
        irrigation=0.0
        gwithdrawal=0.0
        tstep=1.0/dble(nstep)

C -- Any water in the recharge or macropore flow recharge buffers that
C    should already have been assigned to recharge is now claimed.

	irdelay=int(rdelay)
	frdelay=rdelay-irdelay
	irdelay=irdelay+1
	imdelay=int(mdelay)
	fmdelay=mdelay-imdelay
	imdelay=imdelay+1
	if(irdelay+1.le.subdim) then
	  do 100 i=irdelay+1,subdim
	    recharge=recharge+drainsub(i)
	    drainsub(i)=0.0
100	  continue
	end if
	if(imdelay+1.le.subdim) then
	  do 150 i=imdelay+1,subdim
	    mrecharge=mrecharge+macsub(i)
	    macsub(i)=0.0
150	  continue
	end if

C -- Cycle through the days.

	do 1000 iday=1,ndays

C -- First the amount of rainfall and potential evaporation per timestep
C    is established.

	  train=rain(iday)*tstep
	  tepot=epot(iday)
	  cropfac=cropfac_day(iday)
	  gamma=gamma_day(iday)
	  ircode=irrigcode(iday)
	  gwfrac=gwirrigfrac(iday)

C -- Next daily accumulators are zeroed.

	  dayrech=0.0
	  daymrech=0.0
	  dayevap=0.0
	  dayrunoff=0.0
	  dayirrigation=0.0
	  daygwithdrawal=0.0

          do 700 istep=1,nstep
            tvol=vol
            otvol=vol
            odirrig=0.0d0
            do 400 iter=1,mxiter
c	      vd=tvol/maxvol				!fully implicit
              vd=(tvol+vol)*0.5/maxvol			!Crank-Nicholson
              if(vd.gt.1.0) then
                vd=1.0
              else if(vd.lt.0.0)then
                vd=0.0		!will respective subroutines handle this
              end if
              rtemp1=drainage(vd,ks,m,l)*tstep
              rtemp2=evap(vd,tepot,cropfac,gamma)*tstep
              tvol=vol-rtemp1-rtemp2+train
              if(tvol.lt.0.0) tvol=0.0
              dirrig=0.0
              if(ircode.ne.0)then
                if(tvol.lt.vvol)then
                  dirrig=vvol-tvol
                  tvol=vvol
                end if
              end if
              tempvol=tvol
              if(tvol.gt.maxvol) tvol=maxvol
              if(iter.eq.1) go to 390
              if(ircode.eq.0)then
                if(abs(tvol-otvol).le.tol*maxvol) go to 600
              else
                if(abs(tvol-otvol).le.tol*maxvol) then
	          if(abs(dirrig-odirrig).le.tol*(dirrig+odirrig)*0.5)
     +            go to 600
                end if
              end if
390           otvol=tvol
              odirrig=dirrig
400	    continue
	    write(6,510) iday
510	    format(' Subroutine RECHMOD: day ',i3,
     +      ':- iteration limit exceeded')
	    stop

600	    continue
c	    write(6,610) iday,istep,iter
c610	    format(' day ',i3,':  step ',i3,':  iterations = ',i3)

            dayrech=dayrech+rtemp1
            dayevap=dayevap+rtemp2
            dayirrigation=dayirrigation+dirrig
            daygwithdrawal=daygwithdrawal+gwfrac*dirrig
	    if(tempvol.gt.maxvol) then
	      rtemp1=tempvol-maxvol
	      if(rtemp1.lt.mflowmax*tstep) then
	        daymrech=daymrech+rtemp1
	      else
	        daymrech=daymrech+mflowmax*tstep
	        dayrunoff=dayrunoff+rtemp1-mflowmax*tstep
	      end if
	      tempvol=maxvol
	    end if
	  vol=tempvol
700	  continue

	  runoff=runoff+dayrunoff
	  evapn=evapn+dayevap
	  irrigation=irrigation+dayirrigation
	  gwithdrawal=gwithdrawal+daygwithdrawal

C -- Next the recharge and macropore recharge delay buffers are shifted,
C    emptied from the bottom, and filled from the top.

	  recharge=recharge+drainsub(irdelay)
	  mrecharge=mrecharge+macsub(imdelay)
	  if(irdelay.gt.1) then
	    do 800 i=irdelay,2,-1
	      drainsub(i)=drainsub(i-1)
800	    continue
	  end if
	  if(imdelay.gt.1) then
	    do 850 i=imdelay,2,-1
	      macsub(i)=macsub(i-1)
850	    continue
	  end if
	  drainsub(1)=dayrech
	  macsub(1)=daymrech
	  recharge=recharge+(1.0-frdelay)*drainsub(irdelay)
	  drainsub(irdelay)=frdelay*drainsub(irdelay)
	  mrecharge=mrecharge+(1.0-fmdelay)*macsub(imdelay)
	  macsub(imdelay)=fmdelay*macsub(imdelay)
	  rainfall=rainfall+rain(iday)
	  potevapn=potevapn+epot(iday)

1000	continue

	return
	end



	double precision function drainage(vd,ks,m,l)

C -- Function DRAINAGE calculates drainage as a function of the amount
C    of water in the upper moisture store.

	implicit none

	double precision vd,ks,m,l,rtemp

	if(vd.le.0.0)then
	  drainage=0.0
	else if(vd.ge.1.0)then
	  drainage=ks
	else
	  rtemp=1.0-vd**(1.0/m)
	  rtemp=rtemp**m
	  rtemp=(1.0-rtemp)
	  drainage=ks*(vd**l)*rtemp*rtemp
	end if

	return
	end



	double precision function evap(vd,epot,cropfac,gamma)

C -- Subroutine evap calculates evaporation rate as a function amount of
C    water in the upper moisture store.

	implicit none

	double precision vd,epot,cropfac,gamma,rtemp

	if(vd.le.0.0)then
	  evap=0.0
	else if(vd.ge.1.0)then
	  evap=cropfac*epot
	else
	  rtemp=exp(-gamma*vd)
	  evap=cropfac*epot*(1.0-rtemp)/(1.0-2.0*exp(-gamma)+rtemp)
	end if
	return
	end

