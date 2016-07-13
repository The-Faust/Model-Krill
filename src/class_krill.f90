module class_krill

    implicit none

    ! hide the type-bound procedure implementation procedures
    private

    ! constants used within classes
    integer, parameter, public :: M_norvegica = 0
    integer, parameter, public :: T_raschii = 1

    integer, parameter, public :: SEX_M = 0
    integer, parameter, public :: SEX_F = 1
    real, parameter :: EPS = 10e-6
    real, parameter :: SIZE_MAX = 50


    ! allows acces to the krill class
    public :: Krill
    type Krill
        private

		!! Description of caracteristic of individual krill
		real :: sizer
		real :: mass
		real :: dev_freq
		real :: molt_size
		integer :: sex
		integer :: species 

		!! Parameters of ingestion, respiration, develop and arrhenius functions
		real :: aw        ! The regression constant of length(mm)/mass(mgC) relationship (From Agersted et Nielsen 2014)
		real :: bw        ! The regression coefficient of length(mm)/mass(mgC) relationship (From Agersted et Nielsen 2014)
		real :: ei        ! The activation energy of the process considered in eV
		real :: a_molt    ! The regression constant of IMP(day) relationship from Sameoto 1976 Journal of the Fisheries Board of Canada (CJFAS) 33:2568-2576 (GSL)
		real :: b_molt    ! The regression coefficient of IMP(day)/temp(°C) relationship from Sameoto 1976 Journal of the Fisheries Board of Canada (CJFAS) 33:2568-2576 (GSL)
		real :: k0        ! A scaling constant at T0 in l.h^–1.mgC^-3/4
		real :: h0        ! A scaling constant at T0 in h.mgCfood^-1.mgC^-3/4
		real :: A         ! Assimilation efficiency coefficent in %
		real :: r0        ! A scaling constant at T0 in mgC^3/4.h^-1
		real :: p_zoo     ! Proportion of zooplankton used for feed
		real :: p_phyto   ! Proportion of phytoplankton used for feed
		real :: w_molt    ! Percentage of mass loss due to moulting exuvie in % of mass (From Sameoto 1976)

		!! environment
		real :: T
		real :: phyto
		real :: zoo

    contains

		private

		! Methods
		procedure :: arrhenius
		procedure :: breath
		procedure, public :: debug
		procedure, public :: develop
		procedure, public :: grow
		procedure :: ingest
		procedure, public :: molt
		procedure, public :: to_string

		! getters 
		procedure, public :: get_size
		procedure, public :: get_mass
		procedure, public :: get_dev_freq
		procedure, public :: get_molt_size
		procedure, public :: get_sex
		procedure, public :: get_specie

		procedure, public :: get_aw
		procedure, public :: get_bw
		procedure, public :: get_ei
		procedure, public :: get_a_molt
		procedure, public :: get_b_molt
		procedure, public :: get_k0
		procedure, public :: get_h0
		procedure, public :: get_A
		procedure, public :: get_r0
		procedure, public :: get_p_zoo
		procedure, public :: get_p_phyto
		procedure, public :: get_w_molt

		procedure, public :: get_T
		procedure, public :: get_phyto
		procedure, public :: get_zoo

		! Constructor
		procedure, public :: init_krill    ! initialisator for a krill object
		
	end type Krill

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! arrhenius returns a temperature dependance term (unitless)
    ! Arrhenius function of temperature : Universal Temperature Dependance (Gillooly et al. 2001)
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real function arrhenius(this)
        class(Krill)    :: this
        real, parameter :: k=8.62e-5       ! The Boltzmann's constant in eV.K-1
        real, parameter :: T0=273.15       ! Reference temperature (the frozing point of water) in K

        arrhenius = exp(this%ei*this%T/(k*(this%T+T0)*T0))
    end function arrhenius


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! breath returns the energy lost by respiration in mg C.day^-1
    !
    ! Respiration: basal metabolism + activity (swimming) metabolism
    ! but NOT specific dynamic action as it is included in assimilation coeffcient
    ! Data from Angelique (TODO)
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real function breath(this)
		class(Krill) :: this

        breath  = this%r0 * this%mass * this%arrhenius() * 24
    end function breath


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! debug prints a representation of the Krill in the console
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine debug(this)
            class(Krill) :: this

            write(*, *) "== Krill =="
            write(*, '(T4, A, T17, A)'), "species: ", this%to_string()
            select case (this%sex)
                case (SEX_F)
                    write(*, '(T4, A, T17, A)'), "sex: ", "F"
                case (SEX_M)
                    write(*, '(T4, A, T17, A)'), "sex: ", "M"
            end select
            write(*, '(T4, A, T16, ES10.3)'), "size: ", this%sizer
            write(*, '(T4, A, T16, ES10.3)'), "mass: ", this%mass
            write(*, '(T4, A, T16, ES10.3)'), "dev_freq: ", this%dev_freq
            write(*, '(T4, A, T16, ES10.3)'), "molt_size: ", this%molt_size
            write(*, '(T4, A, T16, ES10.3)'), "A: ", this%A
            write(*, '(T4, A, T16, ES10.3)'), "a_molt: ", this%a_molt
            write(*, '(T4, A, T16, ES10.3)'), "aw: ", this%aw
            write(*, '(T4, A, T16, ES10.3)'), "b_molt: ", this%b_molt
            write(*, '(T4, A, T16, ES10.3)'), "bw: ", this%bw
            write(*, '(T4, A, T16, ES10.3)'), "ei: ", this%ei
            write(*, '(T4, A, T16, ES10.3)'), "h0: ", this%h0
            write(*, '(T4, A, T16, ES10.3)'), "k0: ", this%k0
            write(*, '(T4, A, T16, ES10.3)'), "p_phyto: ", this%p_phyto
            write(*, '(T4, A, T16, ES10.3)'), "p_zoo: ", this%p_zoo
            write(*, '(T4, A, T16, ES10.3)'), "r0: ", this%r0
            write(*, '(T4, A, T16, ES10.3/)'), "w_molt: ", this%w_molt
    end subroutine debug


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! develop returns the develop time (intermoult period) in days
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real function develop(this)
		class(Krill) :: this

        if (develop < 3.0) then
            develop = 3.0
		else
			develop = this%a_molt+this%b_molt*this%T		
        end if

    end function develop


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! grow computes and updates the new mass of the individual
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine grow(this)
        class(Krill) :: this
        real :: ingest, breath

        this%mass = this%mass + this%ingest() - this%breath()
    end subroutine grow


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! ingest returns the energy won by ingestion in mg C.day^-1
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real function ingest(this)
            class(Krill) :: this
            real :: enc_k     ! Encounter kernel in L. h^-1. Increases with temperature
            real :: clear     ! Prey encounter rate mass unit.h^-1
            real :: hand      ! Handling time in h. Decreases with temperature.

            enc_k  = this%mass**(3/4) * this%k0 * this%arrhenius()
            clear  = (this%zoo * enc_k * this%p_zoo) + (this%phyto * enc_k * this%p_phyto)
            hand   = 1 / (this%mass ** (3/4)) * this%h0 * this%arrhenius()
            ingest = ((this%A)*(clear**2) / (1+hand*clear**2))*24
    end function ingest


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! molt computes and updates the development freq of the individual. Also, when the freq hits 0.4 (or above), 
    ! computes the molt_factor which fixes the next size of the individual. Thus, when the freq reaches 1 (and above), 
    ! it updates the individual's size with the previously computed molt_factor.
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine molt(this, T)
        class(Krill) :: this
        real :: T

        this%dev_freq = this%dev_freq + 1 / this%develop()

        ! NOTE: moulting is irreversibly engaged after 40% of the IMP.
        ! The new individuals length is completely decided at this time
        ! according to the allometric relationship
        if (this%dev_freq > 0.4 .and. this%molt_size < EPS) then
            this%molt_size = (this%mass / this%aw)** (1 / this%bw)
        end if

        ! At the end of the development phase, the individual molts
        if (this%dev_freq > 1.0) then
            this%mass = this%mass * (1 - this%w_molt)
            this%sizer = this%molt_size
            this%dev_freq = this%dev_freq - 1.0
            this%molt_size = 0
        end if
    end subroutine molt


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! to_string returns a string representation of the given species
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    character(30) function to_string(this)
        class(Krill) :: this

        select case (this%species)
            case (0)
                to_string = "M_norvegica"
            case (1)
                to_string = "T_raschii"
        end select
    end function to_string


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! accessors
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    real function get_size(this)
        class(Krill) :: this
        get_size = this%sizer
    end function get_size

    real function get_mass(this)
        class(Krill) :: this
        get_mass = this%mass
    end function get_mass

	real function get_dev_freq(this)
		class(Krill) :: this
		get_dev_freq = this%dev_freq
	end function get_dev_freq

	real function get_molt_size(this)
		class(Krill) :: this
		get_molt_size = this%molt_size
	end function get_molt_size

	real function get_sex(this)
		class(Krill) :: this
		get_sex = this%sex
	end function get_sex

	real function get_specie(this)
		class(Krill) :: this
		get_specie = this%species
	end function get_specie

	real function get_aw(this)
		class(Krill) :: this
		get_aw = this%aw
	end function get_aw

	real function get_bw(this)
		class(Krill) :: this
		get_bw = this%bw
	end function get_bw

	real function get_ei(this)
		class(Krill) :: this
		get_ei = this%ei
	end function get_ei

	real function get_a_molt(this)
		class(Krill) :: this
		get_a_molt = this%a_molt
	end function get_a_molt

	real function get_b_molt(this)
		class(Krill) :: this
		get_b_molt = this%b_molt
	end function get_b_molt

	real function get_k0(this)
		class(Krill) :: this
		get_k0 = this%k0
	end function get_k0

	real function get_h0(this)
		class(Krill) :: this
		get_h0 = this%h0
	end function get_h0

	real function get_A(this)
		class(Krill) :: this
		get_A = this%A
	end function get_A

	real function get_r0(this)
		class(Krill) :: this
		get_r0= this%r0
	end function get_r0

	real function get_p_zoo(this)
		class(Krill) :: this
		get_p_zoo = this%p_zoo
	end function get_p_zoo

	real function get_p_phyto(this)
		class(Krill) :: this
		get_p_phyto = this%p_phyto
	end function get_p_phyto

	real function get_w_molt(this)
		class(Krill) :: this
		get_w_molt = this%w_molt
	end function get_w_molt

	real function get_T(this)
		class(Krill) :: this
		get_T = this%T
	end function get_T

	real function get_phyto(this)
		class(Krill) :: this
		get_phyto = this%phyto
	end function get_phyto

	real function get_zoo(this)
		class(Krill) :: this
		get_zoo = this%zoo
	end function get_zoo



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! new_krill initializes a new Krill object
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine init_krill(this, sizer, sex, species, aw, bw, ei, a_molt, b_molt, k0, h0, A, r0, &
		&					 p_zoo, p_phyto, w_molt, T, phyto, zoo)
        ! creating a krill object
        class(Krill) :: this

        real :: sizer
        integer :: sex
        integer :: species

		real, optional :: aw       
		real, optional :: bw       
		real, optional :: ei       
		real, optional :: a_molt   
		real, optional :: b_molt   
		real, optional :: k0       
		real, optional :: h0       
		real, optional :: A        
		real, optional :: r0       
		real, optional :: p_zoo    
		real, optional :: p_phyto  
		real, optional :: w_molt 

		real, optional :: T
		real, optional :: phyto
		real, optional :: zoo  

        ! preconditions
        if (sex /= SEX_M .and. sex /= SEX_F) then
                stop 'SEX_ERROR'
        endif

        if (sizer < 0 .and. sizer > SIZE_MAX) then
            stop 'SIZE_ERROR'
        endif
	

        ! giving values to attributes
        this%sex = sex
        this%sizer = sizer
        this%mass = this%aw * (this%sizer ** this%bw)
        this%dev_freq = 0.0
        this%molt_size = 0.0


        ! Parameters specific of M. norvegica for allometric relationship, arrhenius,
        ! ingestion, respiration, development and moult equations
        if(species == 0) then
			if(present(aw)) then
                    this%aw = aw
			else 
	            this%aw = 7.5e-5
			end if 

			if(present(bw)) then
                    this%bw = bw
			else 
		    	this%bw = 3.79
			end if 
		
			if(present(ei)) then
                this%ei = ei
			else 
		    	this%ei = 0.2
			end if 

			if(present(k0)) then
                this%k0 = k0
			else 
		    	this%k0 = 1.0
			end if 

			if(present(h0)) then
                this%h0 = h0
			else 
		    	this%h0 = 150.0
			end if 
		
			if(present(A)) then
                this%A = A
			else 
		    	this%A = 0.6
			end if 

			if(present(r0)) then
                this%r0 = r0
			else 
		    	this%r0 = 10e-3
			end if 

			if(present(w_molt)) then
                this%w_molt = w_molt
			else 
		    	this%w_molt = 0.062
			end if 

			if(present(a_molt)) then
                this%a_molt = a_molt
			else 
		   		this%a_molt = 20.62
			end if 

			if(present(b_molt)) then
                this%b_molt = b_molt
			else 
		    	this%b_molt = -1.16
			end if 

			if(present(p_phyto)) then
                this%p_phyto = p_phyto
			else 
		    	this%p_phyto = 0.2
			end if 

			if(present(p_zoo)) then
                this%p_zoo = p_zoo
			else 
		    	this%p_zoo = 0.8
			endif
       	endif

        ! Parameters specific of T.raschii for allometric relationship, arrhenius,
        ! ingestion, respiration, development and moult equations
        if(species == 1) then
			if(present(aw)) then
                this%aw = aw
			else 
	            this%aw = 0.000717
			end if 

			if(present(bw)) then
                this%bw = bw
			else 
		    	this%bw = 3.17
			end if 

			if(present(ei)) then
                this%ei = ei
			else 
		    	this%ei = 0.2
			end if 

			if(present(k0)) then
                this%k0 = k0
			else 
		    	this%k0 = 1.0
			end if 

			if(present(h0)) then
                this%h0 = h0
			else 
		    	this%h0 = 1
			end if 
		
			if(present(A)) then
                this%A = A
			else 
		    	this%A = 0.6
			end if 

			if(present(r0)) then
                this%r0 = r0
			else 
		    	this%r0 = 10e-3
			end if 

			if(present(w_molt)) then
                this%w_molt = w_molt
			else 
		    	this%w_molt = 0.05
			end if 

			if(present(a_molt)) then
                this%a_molt = a_molt
			else 
		    	this%a_molt = 20.62
			end if 

			if(present(b_molt)) then
                this%b_molt = b_molt
			else 
		    	this%b_molt = -1.16
			end if 

			if(present(p_phyto)) then
                this%p_phyto = p_phyto
			else 
		    	this%p_phyto = 0.8
			end if 

			if(present(p_zoo)) then
            	this%p_zoo = p_zoo
			else 
		    	this%p_zoo = 0.2
			endif
		endif

		if(present(phyto)) then
			this%phyto = phyto
		else
			this%phyto = 1
		endif

		if(present(zoo)) then
			this%zoo = zoo
		else
			this%zoo = 1
		endif

		if(present(T)) then
			this%T = T
		else
			this%T = 1
		endif

    end subroutine init_krill
end module class_krill
