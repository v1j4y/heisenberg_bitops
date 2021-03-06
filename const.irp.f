BEGIN_PROVIDER [integer,maxdet]
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
    
    nsites=5
    ntrou=2
    countbit=0
    nt1=exp(lgamma(float(nsites+1))-lgamma(float(nsites-ntrou+1))-lgamma(float(ntrou+1)))
!C fix for parity
    isz=0
    if(mod(nsites-ntrou+isz,2).eq.0)then
        nalpha=(nsites-ntrou+isz)/2
        nbeta=(nsites-ntrou-isz)/2
    else
        nalpha=(nsites-ntrou+isz+1)/2
        nbeta=(nsites-ntrou-isz-1)/2
    endif
    nt2=exp(lgamma(float(nsites-ntrou+1))-lgamma(float(nalpha+1))-lgamma(float(nbeta+1)))
    print *,'nt1=',nt1,'nt2=',nt2,'nsites=',nsites,'ntrou=',ntrou,'nalpha=',nalpha,'nbeta=',nbeta
    ndet=nt1*nt2
    maxdet=ndet
END_PROVIDER
