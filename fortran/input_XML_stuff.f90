      module input_XML_stuff
    
        implicit none 
        !integer:: mgas_st , jgas_st     ! <genreal> section 
        integer::nhgen_xml, ncurvin_xml, ncase_xml, mgas_xml,jgas_xml  ! <genreal> section 
		logical::nhgen_ok , ncurvin_ok , ncase_ok,  mgas_ok ,jgas_ok   ! flags that parameter was readed 
        
		real::tlqb(10,3)       ! <Telq> Tag parameter
		
        integer::ndt,ndtb   ! <Vdt> tag parameters
        
        logical :: r_xml(8) ! array of xml reading flags -> .true. = read xml ; .false. = read old style 
        
		contains 
		subroutine init_flags ()
		
		nhgen_ok = .false. 
		ncurvin_ok = .false. 
		ncase_ok = .false. 
		mgas_ok = .false.  
		jgas_ok = .false. 
		
		end subroutine init_flags
		
		subroutine init_XML_flags
		r_xml = .false. !all read in old style 
		
		 ! fill flags -> .true. = xml style
		r_xml(1) = .true.  ! general
		r_xml(2) = .true.  ! passing
		r_xml(3) = .true.  ! expert
		r_xml(4) = .true.  ! CorCat
		r_xml(5) = .false.  ! Materialsd
		r_xml(6) = .false.  ! FEmodel
		r_xml(7) = .false.  ! Conditions
		r_xml(8) = .false.  ! Loads
		end subroutine init_XML_flags 
		
		
		
      end module 
