BEGIN_PROVIDER  [integer,maxdet]
&BEGIN_PROVIDER [integer,nsites]
&BEGIN_PROVIDER [integer,ntrou ]
&BEGIN_PROVIDER [integer,countbit]
&BEGIN_PROVIDER [integer,isz     ]
&BEGIN_PROVIDER [integer,nalpha  ]
&BEGIN_PROVIDER [integer,nbeta   ]
&BEGIN_PROVIDER [integer,nt1     ]
&BEGIN_PROVIDER [integer,nt2     ]
&BEGIN_PROVIDER [integer,ndet    ]

    implicit none
    BEGIN_DOC
    ! provides rank
    END_DOC
    
    nsites=16
    ntrou=1
    countbit=0
    nt1=ceiling(gamma(dble(nsites+1))/(gamma(dble(nsites-ntrou+1))*gamma(dble(ntrou+1))))/2
    print *,'nt1',nt1
!C fix for parity
    isz=0
    if(mod(nsites-ntrou+isz,2).eq.0)then
        nalpha=(nsites-ntrou+isz)/2
        nbeta=(nsites-ntrou-isz)/2
    else
        nalpha=(nsites-ntrou+isz+1)/2
        nbeta=(nsites-ntrou-isz-1)/2
    endif
    nt2=ceiling(gamma(dble(nsites-ntrou+1))/(gamma(dble(nalpha+1))*gamma(dble(nbeta+1))))
    print *,'nt1=',nt1,'nt2=',nt2,'nsites=',nsites,'ntrou=',ntrou,'nalpha=',nalpha,'nbeta=',nbeta,'ndet=',nt1*nt2
    ndet=nt1*nt2
    maxdet=ndet
END_PROVIDER
