BEGIN_PROVIDER [integer,n]
&BEGIN_PROVIDER [integer,maxdet]
&BEGIN_PROVIDER [integer,nsites]
&BEGIN_PROVIDER [integer,ntrou ]
&BEGIN_PROVIDER [integer,countbit]

    implicit none
    BEGIN_DOC
    ! provides rank
    END_DOC
    
    nsites=4
    ntrou=2
    maxdet=1000
    countbit=0
    n=exp(lgamma(float(nsites+1))-lgamma(float(nsites-ntrou+1))-lgamma(float(ntrou+1)))
END_PROVIDER
