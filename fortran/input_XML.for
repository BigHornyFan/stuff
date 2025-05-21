      module xml_reader_Hef_CC
          ! Abudalipov 
          ! Module inlude procedures that read HEFEST_CC 
          ! input file in XML format 
          ! See -> read_xml(file_name)
          
          ! include outer parameters 
          use input_XML_stuff
      
          !including xml_parser
          Use  flib_dom
          Use  m_AttributeProcessing
          USE  m_dom_parse 
          
          
          !start including HEFEST_CC modules
          use hefest_rerad
	    use hefest_bubbling
          use ross_param, only: absorb, fl_ross, cuta
          use bk01
          use exch
          use keys
          use len_array
          use opt_1
          use opt_2
          use pointers
          use sv_h
          use num_out
          use como, only:a
          use model
!
!         CORE -
          use CORE_models
          use auxil
          use CORE_param   ! - use full if possible - errors may occur !!
!
          Use FPMC_HEF
!! copying from read_XML
!........................................
!n !DEC$ IF DEFINED (__CC_FPR__)
!n 	use CC_FPR, only : fp_masses
!n !DEC$ ENDIF

!DEC$ IF DEFINED (__CHEM_CC__)
	    use chem_CC
!DEC$ ENDIF

!DEC$ IF DEFINED (__CHEM_CONCR__)
	    use chem_concr
!DEC$ ENDIF
          !end including HEFEST_CC modules
         

        implicit none 
        type(fnode), pointer ::myDoc,myNode0     ! parser parameters
        type(fnodeList), pointer ::myList        ! also parser parameter
       
        integer  logio ! index for log outpet
       
      
       !mirror parameters for inner paramters of input.for
       !integer::nhgen_xml, ncurvin_xml, ncase_xml, mgas_xml,jgas_xml      
       integer::matn, ncompin, numcm
       real :: rho(190), tlat(190), hlat(190),cv(8,190),angg(190)   ! <materials> inner parameters
       real :: genm(190), ptemp(8,190),cond1(8,190),cond2(8,190)    ! <materials> inner parameters
       integer :: mtype(2,190),ncgenm(190), mnum(190)               ! <materials> inner parameters
       character*(*) :: inpname*10
       dimension inpname(190)
       character*20 filnam_xml ! from <conditions>
       integer:: kan, kanb , incrit              ! parameters of chanales? 
       ! INNER FEMODEL PARAMETERS
       integer::ifree  ! flag if node/elem is free 
       integer:: ilast, infem, ino, iar, ibloc, msacrm
       integer:: bias, elemax, nblc 
       real :: hlow, vfill, gaps(2)
       character(len=20) :: icm_fem
       
       
       
       contains
      
          ! subroutine get the adress and open this file
          subroutine read_xml(inp_file_name)  
               integer::st                                          ! number of characters in file adress
               character(len=*) :: inp_file_name                        ! name xml file to open 
                              
                
               ! initialise flags from stuff -> all flags = .false. -> not readed
               call init_flags()
               
               !inp_file_name = 'D:\HEFEST_CC\test_new_input.task'
               write (*,*) '-----------------------------------'
               write (*,*) '--Start reading file in XML format-'
               write (*,*) '-----------------------------------'
               write (*,*) 'input from:' , inp_file_name
               
               call open_log()
               !open(newunit=logtest,file=inp_file_name,iostat=st)
               write(logio,*) st
               write(logio,*) 'logging OK'
               write(logio,*) 'Start parsing with adress:'
               write(logio,*) inp_file_name
          myDoc =>parsefile(trim(inp_file_name))     ! open xml file
               if(st.ne.0) write(logio,*) 'xml parsing error'
               !write(logio,*) '1'
               myList => getElementsByTagName(myDoc, "Hefest_CC")
               !write(logio,*) '2'
               if (getLength(MyList) ==0) then
                   write(logio,*) 'No tag <Hefest_cc> in input file'
               else 
                   !write(logio,*) '3'
                   myNode0 => item(myList,0)
                   
                   !write(logio,*) '4'
                   
                   
                   ! now all the procedures should be called separated 
                   ! from input.for -> see "Abudalipov" in input.for
                   
                   ! xml reading <General>              ! 10
                   !call read_general()
                   !call READ_loads
                   
                   ! xml reading <Passing>              ! 310
                   !call read_passing()
                   
                   ! xml reading <Expert>               ! 400 
                   !call read_expert()
                   
                   ! xml reading <CorCat>               ! 500
                   !call read_corcat()
                   
                   ! xml reading <Materials>            ! 600
                   !call read_materials()
                   
                   ! xml reading <FEModel>              ! 650 
                   !call read_femodel()
                   
                   ! xml reading <Conditions>           ! 700
                   !call read_conditions()
                   
                   ! xnl reading <Loads>                 ! 800
                   !call read_loads()
                   
               end if
               
               !closing log should be called later
               !call close_log()
          
          end subroutine read_xml
          
          
          ! reading <Loads> Tag
          subroutine read_loads()
            implicit none 
            type(fnode),pointer::lnode,snode      !node of <Loads> Tag; node for child tags;
            type(fnodelist),pointer::s_list       !list for reading sub_tags; 
            integer:: j,k,jj    !loop counters 
            integer:: iread     !flag if attr was readed 
            integer:: nbuf      !int buffer
            integer:: ncrv, npoi ! reading tables -> n of curve ; n of points
            character(len=20) :: npbuf ! string buffer
            
            integer :: del(190) !delete after test!
            
            write(logio,*) 'Start reading <Loads> Tag'
            s_list=>getElementsByTagName(MyNode0,"Loads")
            if(getLength(s_list) == 0) then 
                call warning('No tag <conditions> in input file',0)
                return
            end if 
            lnode=>item(s_list,0)
            if (.not.associated(lnode)) then
                call warning('cant link to load') 
                return 
            end if 
            
            ! reading <Table>'s
            s_list=>getElementsByTagName(lnode,"Table")
            ! check if trere are any tables in <Load> 
            if (getLength(s_list) == 0 ) then 
                call warning('No Tables in Loads!', 0) 
                !return
            end if
            
            ! Taking <Table>'s by one 
            do j = 1 , getLength(s_list) 
                snode=>item(s_list,j-1)     ! we start from 1 -> s_list shoud be taken from j-1
              !call  read_t(snode,a(n98),a(n99),a(n99a))       ! call for reading table
              call  read_t(snode,a(n98),a(n99),a(n99a))       ! call for reading table
              !call read_t(snode,a(n98:n98), a(n99:n99),a(n99a:n99a))
            end do 
            
            ! reading <Formula>'s
            s_list=>getElementsByTagName(lnode,"Formula")
            ! check if trere are any <Formula>'s in <Loads> 
            if (getLength(s_list) == 0 ) then 
                call warning('No Formula in Load!', 0) 
                return
            end if
            
            if (getLength(s_list) > 20 ) then 
                call warning('too many formulas, max = 20') 
                return
            end if
            
            ! Taking <Formula>'s by one 
            do j = 1 , getLength(s_list)  
                snode=>item(s_list,j-1)                     ! we start from 1 -> s_list shoud be taken from j-1
                call GIAt(snode,"ncurve", ncrv, iread)           !  -  read curve number
                if (iread /= 0) then 
                    call warning('No NCURVE in formula',1)
                    cycle
                end if 
                call GCAt(snode, "formula", npbuf, iread)        !  -  get formula name
            end do 
            
            
            ! i guess it's ok to close log hear 
            call close_log()
          end subroutine read_loads
          
          ! reading <Conditions> Tag 
          subroutine read_conditions()
            implicit none 
            type(fnode),pointer::conds,snode      !node of <conditions> Tag; node for child tags;
            type(fnodelist),pointer::s_list       !list for reading sub_tags; 
            integer:: j,k,jj , iread          ! loop counters , flag if attr. was readed 
            character(len=20) :: cbuf         ! buffer for reading strings
            logical :: en_rad_ok              ! flag that <Enrad_bc> readed -> needed for <En_matrix> 
            ! INNER PARAMERES OF <Comditions>
            integer:: in_water                !  this plate water flag
            integer:: n_cbcb                    !  this boundary order
            integer:: ncont,ncontf,nplate,ic_cbc  ! don't know what it is...
            integer :: i_rad, j_rad           ! loop counters
            character*20 filnam       ! idk..
            character(len=20) :: icm_enc      ! name of file 
            integer:: isort,ipenetr           ! same
            real :: bufn ! real buffer
            integer :: ibufa
            ! INPUT POINTERS
            integer:: ic_rbc 
            
            write(logio,*) 'Start reading <Conditions> Tag'
            s_list=>getElementsByTagName(MyNode0,"Conditions")
            if(getLength(s_list) == 0) then 
                call warning('No tag <conditions> in input file',0)
                return
            end if 
            conds=>item(s_list,0)
            if (.not.associated(conds)) then
                call warning('cant link to conditions') 
                return 
            end if 
            
            ! initialize <Conditions> 
            !ic_tbc=0      !  counter of t.b.c. inputed
            !ic_fbc=0      !  counter of f.b.c. inputed
            !ic_cbc=0      !  counter of c.b.c. inputed
            ic_rbc=0      !  counter of r.b.c. inputed
            !ic_ibc=0      !  counter of i.b.c. inputed
            !ic_enc=0      !  counter of e.r.b.c. inputed
            nplate=0      !  this boundary number
            ncontc=0
            
            ! reading <Het_gen_elem> Tag 
            if (is_Tag(conds, snode,"Het_gen_elem")) then 
            
                kan = 10 ! set up default channel
                
                call GCAt(snode, "free" , cbuf, iread) 
                if(iread==0.and.(cbuf(1:3)=='Yes'.or.cbuf(1:3)=='yes'))
     *               iread = 1 
                call GCAt(snode, "file", icm_fem, iread)
                if (iread==0.and.icm_fem(1:6)/='nofile') then 
                    kan = 15 ! set input channel to the external file
                    call openf(icm_fem,kan,0)   !  open file with hgen data
                end if 
                kanb=kan
                
                call GCAt(snode, "matr", icm_fem, iread)! IDK if it Used ?
                if (iread==0) then 
              if(nhgen_xml==0)call warning('No FEs for hgen in matr!')
                    kanb=-kan ! set input channel to the external file
                end if 
                
                !09-07-2016      call hgin(a(n46),a(n47a),nhgen_xml,numel, kan)
                call hgin(a(n2),a(n46),a(n47a),nhgen_xml,numel, 
     *                                  kanb,ifree)
                call assgn_a(a(n47a),a(n48d),nhgen_xml)
                ifree=0
                if(kan.ne.10) then 
                    close(kan,status='keep')
                    kan = 10 ! restore default channel
                endif
                a(n47:n47+numel-1) = 0.     
                !in_hgen=1        !  whether heat generation data were inputed
                
            end if 
            
            ! reading <Induct> Tag  
            ! array can be readed in old format but other attributes cant
            if (is_Tag(conds, snode , "Induct") ) then 
                !call GIAt(snode, "induct" , induct)    ! order of cbc boundaty
                call GIAr(snode,indmt,check=induct)     ! order of cbc boundaty
                call GRAt(snode,"rmind",rmind,iread)              !   read heat generation parametres
                !if(iread.ne.)call GRAt_old(snode,rmind,induct+2)    ! array 
                call GRAt(snode,"rmxind",rmxind,iread)            !   read heat generation parametres
                call GRAt(snode,"zmind",zmind,iread)              !   read heat generation parametres
                call GRAt(snode,"zmxind",zmxind,iread)            !   read heat generation parametres
                call GRAt(snode,"decrm",decrm,iread)              !   decrement
                call GRAt(snode,"pampl",pampl,iread)              !   power amplitude
            end if  
            
            ! reading <Init_temp> Tag  
            if (is_Tag(conds, snode , "Init_temp") ) then
            
                call GCAt(snode, "free" , cbuf, iread) 
                if(iread==0.and.(cbuf(1:3)=='Yes'.or.cbuf(1:3)=='yes'))
     *               iread = 1 
                
                ino = 1 
                kan = 10    ! wrong logic! 
                
                call GCAt(snode, "file", icm_fem, iread)
                if (iread==0.and.icm_fem(1:6)/='nofile') then 
                    kan = 15                    ! set input channel to the external file
                    call openf(icm_fem,kan,0)   !  open file with hgen data
                end if
                 
                kanb = kan
                 
                call GCAt(snode,"matr",cbuf,iread)  ! IDK 
                if (iread == 0)  kanb=-kan 
                
                call itin(a(n1),a(n2),a(n102),nit,numnp,numel,
     *                                       temprat,kanb,ifree)
                ifree = 0 
                
                if(kan.ne.10) then 
                    close(kan,status='keep')
                    kan = 10 ! restore default channel
                endif
                !in_temp = 1 ! we dont actualy need this 
            end if  
            
            ! reading <Temp_bc>
            if (is_Tag(conds, snode , "Temp_bc") ) then
                
                ino = 1 
                kan = 10 
                
                call GCAt(snode, "file" , cbuf, iread) 
                if(iread==0.and.cbuf(1:6)/='nofile') then
                     ino = -1 
                     kan = 15   ! set input channel to the external file
                     call openf(icm_fem,kan,0)   !  open file with hgen data
                end if
                
                call  tbcinp(a(n16),a(n17),a(n18),ntbc,kan)
                
                if(kan.ne.10) then 
                    close(kan,status='keep')
                    kan = 10 ! restore default channel
                endif
                !in_tbc=1         !  no need this parameter
            end if 
            
            ! reading <Flux_bc>
            if (is_Tag(conds, snode , "Flux_bc") ) then
                
                call GCAt(snode, "free" , cbuf, iread) 
                if(iread==0.and.(cbuf(1:3)=='Yes'.or.cbuf(1:3)=='yes'))
     *               iread = 1 
     
                ino = 1 
                kan = 10 
                
                call GCAt(snode, "file" , cbuf, iread) 
                if(iread==0.and.cbuf(1:6)/='nofile') then
                     kan = 15   ! set input channel to the external file
                     call openf(icm_fem,kan,0)   !  open file with hgen data
                end if
                
                call fbcinp(a(n1),a(n19),a(n20),a(n21),a(n22),a(n22a)
     1          ,a(n89),nfbc,igeom, ncontf, nrup,kan,ifree)     !   ncontf - not used
                
                if(kan.ne.10) then 
                    close(kan,status='keep')
                    kan = 10 ! restore default channel
                endif
                !in_fbc=1         !  no need this parameter
            end if 
            
            ! reading <Conv_bc> Tag
            if (is_Tag(conds, snode , "Conv_bc") ) then
                
                call GCAt(snode, "free" , cbuf, iread) 
                if(iread==0.and.(cbuf(1:3)=='Yes'.or.cbuf(1:3)=='yes'))
     *               iread = 1 
     
                call GIAt(snode,"nplate",nplate)
                ino = 1 
                kan = 10 
                
                call GCAt(snode, "file" , cbuf, iread) 
                if(iread==0.and.cbuf(1:6)/='nofile') then
                     kan = 15   ! set input channel to the external file
                     call openf(icm_fem,kan,0)   !  open file with hgen data
                end if
                
                ncont=0             !  ncont - number of water plates in this cbc-group
                in_water=0          !  this plate water flag
                n_cbcb=0            !  this boundary order
                isort=0             !  boundary sorting type
                ipenetr=0           !  boundary penetration criterion: =1 - nodal, =2 - thickness
                
                call GCAt(snode,"cbc_part",filnam,iread)
                if(iread==0.and.filnam(1:3)=='yes')then              !- new boundary group of plates
                    call GIAt(snode, "n_cbcb", n_cbcb, iread)       ! read No. =n_cbcb of this cbc group
                    if(iread/=0)call GIAt_old(snode, n_cbcb,1)
                    
                    call GIAt(snode, "in_water", in_water, iread)   ! water interaction on these plates  
                    if(iread/=0)call GIAt_old(snode, in_water,2)
                    
                    call GIAt(snode, "isort", isort, iread)         ! how to sort plates in this boundary part
                    if(iread/=0)call GIAt_old(snode, isort,3)
                    
                    call GIAt(snode, "ipenetr", ipenetr, iread)     ! penetration criterion for all plates of this boundary part
                    if(iread/=0)call GIAt_old(snode, ipenetr,4)
                    
                    if(n_cbcb.le.0) call warning ('neg. n_cbcb')
                    if(n_cbcb.gt.1000) call warning ('too large n_cbcb')
                    
                    icbcopt(1,n_cbcb)=in_water  !  1 - water option
                    icbcopt(2,n_cbcb)=isort     !  2 - type of sorting - not used yet
                    icbcopt(3,n_cbcb)=ipenetr   !  3 - penetration criterion
                end if 
                
          call cbcinp(a(n1),a(n24),a(n25),a(n26),a(n27),a(n28),a(n29),
     1        a(n29a),a(n30a),a(n90),nplate,igeom, ncont,ic_cbc,iopcmx,        &
     &              kan,ifree)
                
                if(n_cbcb+in_water.gt.0)    ! flags for b.c. number and water option - positive numbers !!
     *          call assgn_bc_opt(a(n29a),a(n24),a(n62),numel,numedg,
     *                     ic_cbc,nplate,a(n29c),n_cbcb,in_water)
                
                ifree = 0 
                ncontc=ncontc+ncont !  ncontc - total number of water plates in cbc
                ic_cbc=ic_cbc+nplate
                if(ic_cbc.gt.ncbc)call warning('excess conv.b.c. inp')
                
                ! end reading <conv_bc> Tag -> 
                if(ic_cbc.eq.ncbc) then
                    in_cbc=1        !  whether all convection b.c. data already inputed
                else
                  call warning('** not all conv.b.c. inputed: i_cbc=')
                endif
                
! Gather penetration criterion nodes. Here node numbers in ncrit(icrit) are not modified !
                call bound_crit(a(n24),a(n29c),a(n102),ibufa,icbcopt,
     *                      icrit,ncrit,nodhot,ncbc,bufn,bufn,0)
            end if 
            
            ! reading <Rad_bc> Tag
            if (is_Tag(conds, snode , "Rad_bc") ) then
            
                call GCAt(snode, "free" , cbuf, iread) 
                if(iread==0.and.(cbuf(1:3)=='Yes'.or.cbuf(1:3)=='yes'))
     *               iread = 1 
                
                call GIAt(snode, "nplate" , nplate)
                
                call rbcinp (a(n1),a(n49),a(n50),a(n51),a(n52),a(n53),
     1          a(n54),a(n56),a(n56a),a(n91),nplate,igeom,no_rat,ifree)
                ifree=0
                ic_rbc=ic_rbc+nplate
                
                !end reading <Rad_bc>
                if(ic_rbc.eq.nrbc) then
                    in_rbc=1         !  whether radiation b.c. data were inputed
                else
                    in_rbc=0 
                    call warning('all rad. bc data not inputed',0)
                endif
            end if 
            
            ! reading <Inner_bc> Tag
            if (is_Tag(conds, snode , "Inner_bc") ) then
            
                call GCAt(snode, "free" , cbuf, iread) 
                if(iread==0.and.(cbuf(1:3)=='Yes'.or.cbuf(1:3)=='yes'))
     *               iread = 1
                
                call ibcinp (a(n2),a(n65),a(n66),a(n67),a(n68),nibc,
     *               km3,ifree)       
                ifree=0
                
            end if 
            
            en_rad_ok = .false.
            ! reading <Enrad_bc> Tag
            if (is_Tag(conds, snode , "Enrad_bc") ) then
                
                call GCAt(snode, "free" , cbuf, iread) 
                if(iread==0.and.(cbuf(1:3)=='Yes'.or.cbuf(1:3)=='yes'))
     *               iread = 1
                
                ino = 1
                
                call GCAt(snode,"file",icm_enc,iread) 
                if (iread==0.and.icm_enc(1:6)/='nofile') then 
                    ino = -1 
                end if 
                
                if(nrseg.gt.0) then ! read b.c. plates
       call radin2(a(n1),a(n31),a(n32),a(n33),a(n34),a(n35),
     1 a(n36),a(n37),a(n40),a(n40a),a(n41),a(n42),a(n43),a(n92),nband,
     2 necurv,irtyp,igeom,numnp,numel,nrdim,ino*nrseg,0,icm_enc,ifree)! - don't create radiosities
                
                ifree = 0 
                
!see!!!         if(chem_mode==1) then ! if there is core catcher, sort the plates for phi_matrix_cc
                call sortencl(a(n1),a(n31),nrseg,a(n40a),a(n32),
     *              a(n33),a(n34),a(n36),igeom,ipof,zini)
!see!!!         endif 
                
                else 
                 call warning(' zero control data for enclosure nrseg')
                end if 
                
                en_rad_ok = .true. ! flag that we input enrad_bc  -> neeeded for <En_matrix> 
                
            end if 
            
            ! reading <Parameters> Tag 
            if (is_Tag(conds, snode , "Parameters")) then
                call GRAt(snode,"sigma",sigmaB,iread) !-  read Stefan-Bolzman constant
                if (iread == 0) then 
                    tolr=1.e-04
                    itmaxb=100
                end if 
                
                call GCAt(snode,"z_step",cbuf,iread) 
                if(iread==0.and.(cbuf(1:3)=='yes'.or.cbuf(1:3)=='Yes'))
     *              call deprecated_multiple("z_step")
                call GCAt(snode,"r_step",cbuf,iread) 
                if(iread==0.and.(cbuf(1:3)=='yes'.or.cbuf(1:3)=='Yes'))
     *              call deprecated_multiple("r_step")
                
                ! input minimum height of the enclosure b.c.
                call GRAt(snode,"hgh0", hgh0)   !hmin parametres
                !call GRAt(snode,"hgh0", hgh0)   !hmin parametres       next one is just readed, but not used? 
                
                ! ....... on/off Rosseland model    
                call GRAt(snode,"ross_model",fl_ross,iread)     ! standard boundary emissivity in Ross. model
                if (iread == 0) then
                     if ( fl_ross > 1.) call warning('fl_ross > 1!!!')
                     if ( fl_ross /=0 ) wfc0 = 1.e-4            ! reduce enclosure rad if radiative heat transfer
                end if 
                
                
            end if 
            
            ! reading <En_matrix> Tag    ! how to obtain view factor matrix
            if (is_Tag(conds, snode , "Parameters"))then
              if(en_rad_ok) then 
                
                call GIAt(snode, "ivfm" , ivfm,iread)      !  -  read type of VFM data
                if(iread/=0) call warning('no ivfm in <En_matrix>!!')
                if (ivfm > 0) then 
                
                  !..... prepare arrays
                  allocate(phi_buffer(nrseg,nrseg))
                  allocate (plates_areas(nrseg))
	              allocate (erad_rhs(nrseg))
	              allocate (erad_matr(nrseg,nrseg))
                  allocate (plates_buffer(2,nrseg))
                  call atoi (a(n31), plates_buffer, 2*nrseg)
                  
                  !...... find plates areas
                   call calc_areas(nrseg, numnp, plates_buffer, a(n1),
     1                   igeom, plates_areas)
                end if 
                
                !.....  read VF matrix (real(4))
                if(ivfm.eq.1) then
                  call radin2(a(n1),a(n31),a(n32),a(n33),a(n34),a(n35),
     1                   a(n36),a(n37),a(n40),a(n40a),a(n41),a(n42),
     2                   a(n43),a(n92),nband,necurv,irtyp,igeom,numnp,
     3                   numel,nrdim,nrseg,1,icm_enc,ifree)
                  
                  !..... store the matrix in the phi_buffer array
                  do i_rad = 1, nrseg
                    do j_rad = 1, nrseg
              phi_buffer(j_rad,i_rad) = a(n43+(i_rad-1)*nrseg+j_rad-1)
                    enddo
                  enddo
                  
                  !..... initialization VF solution procedure
                  call radin2(a(n1),a(n31),a(n32),a(n33),a(n34),a(n35),
     1                  a(n36),a(n37),a(n40),a(n40a),a(n41),a(n42),
     2                  a(n43),a(n92),nband,necurv,irtyp,igeom,numnp,
     3                  numel,nrdim,nrseg,2,icm_enc,ifree)
                end if 
                  
                if(ivfm.eq.2) then  
                  !.....  create VF matrix -
                  call phi_matrix_cc(nrseg, numnp, plates_buffer,
     1                    a(n1), phi_buffer)
                  call normalise_phi_matrix(nrseg, phi_buffer)
                  if(ipof.le.-2) call print_phi_matrix(nrseg,
     1                               dble(timin),phi_buffer)
                  
                  do i_rad=1,nrseg
                    do j_rad = 1,nrseg
         a(n43+(i_rad-1)*nrseg+j_rad-1) =real(phi_buffer(j_rad,i_rad),4)
                    enddo
                  enddo
                  
                  
                  
                  call radin2(a(n1),a(n31),a(n32),a(n33),a(n34),a(n35),          ! calculate radiosities etc.
     1                     a(n36),a(n37),a(n40),a(n40a),a(n41),a(n42),
     2                     a(n43),a(n92),nband,necurv,irtyp,igeom,numnp,
     3                     numel,nrdim,nrseg,2,icm_enc,ifree)
                  
                  
                end if 
              else 
                call warning('no <Enrad_bc> but there is <En_matrix>')
              end if 
            end if 
            
            ! end reading <Conditions>
          end subroutine read_conditions
          
          ! reading <FEModel> Tag 
          subroutine read_femodel()
            implicit none 
            type(fnode),pointer::fmn,snode        !node of <FEModel> Tag; node for child tags;
            type(fnodelist),pointer::s_list       !list for reading sub_tags; 
            character(len=20) :: cbuf             !buffer for reading charachter attribures 
            integer:: j,k,jj , iread     ! loop counters , flag if attr. was readed 
            
            write(logio,*) 'Start reading <FEModel> Tag'
            s_list=>getElementsByTagName(MyNode0,"FEModel")
            if(getLength(s_list) == 0) then 
                call warning('No tag <FEModel> in input file',0)
                return
            end if 
            fmn=>item(s_list,0)
            if (.not.associated(fmn)) then
                call warning('cant link to FEModel') 
                return 
            end if 
            
            inFEM=0     ! whether nodes and elements for correction were inputed
            msacrm=0    ! sacrifitial material number (if block assignement)
            iar=1
            ilast=0
            nblc=0      ! counter of cylindrical blocks
            ibloc=0
            bias=0.
            hlow=0.
            vfill=1.e10
            gaps(1)=0.02    ! tolerance for fitting SM to mesh
            gaps(2)=0.02 
            
            
            
            ! reading <Nodes> Tag
            s_list=>getElementsByTagName(fmn,"Nodes")
            if (getLength(s_list) > 0) then 
                snode=>item(s_list,0)
                call GCAt(snode,"file",icm_fem,iread)
                if(iread==0)then
                    if(cbuf.eq.'nofile') then
                        ino = 1
                    else 
                        ino = -1 
                    end if 
                else 
                    call warning ('no file in node',0)
                end if
                
                call GCAt(fmn, "free" , cbuf, iread) 
        if(iread==0.and.(cbuf(1:3)=='yes'.or.cbuf(1:3)=='Yes'))ifree = 1
                
                call nodinp(a(n1),a(n102),ino*numnp,icm_fem,ifree)
                ifree=0
                in_node=1        !  whether nodal data were inputed
                
                call ranges(a(n1),numnp,xrange,zrange) ! calculate ranges of the domain
                if(xmelt(1)**2+xmelt(2)**2 .lt. 1.e-6) ! assign default value if not inputed
     *          call assgn_a(xrange,xmelt,2)
                if(zmelt(1)**2+zmelt(2)**2 .lt. 1.e-6) ! assign default value if not inputed 
     *          call assgn_a(zrange,zmelt,2)
                
                ! reading <Scale>       ! Is <scale> and other parameters should be readed ?
                ! end reading <Nodes>
                inFEM=inFEM+1
            end if 
            
            ! reading <Elements> Tag
            s_list=>getElementsByTagName(fmn,"Elements")
            if (getLength(s_list) > 0) then 
                snode=>item(s_list,0)
                !end reading <Elements> 
                
                call GCAt(snode,"file",cbuf,iread)
                if(iread==0)then
                    if(cbuf.eq.'nofile') then
                        ino = 1
                    else 
                        ino = -1 
                        icm_fem = cbuf
                    end if 
                else 
                    call warning ('no file in node',0)
                end if
                
                call GCAt(fmn, "free" , cbuf, iread) 
      if(iread==0.and.(cbuf(1:3)=='yes'.or.cbuf(1:3)=='Yes'))ifree = 1
                call elminp(a(n1),a(n2),a(n3),a(n4),a(n13),
     *   ino*numel,igeom,a(n6),a(n48),a(n48c),a(n94),icm_fem,ifree,0)
                
                ifree=0
                in_elem=1 
                
                !Calculate FEs and edges connectivity arrays a(n61) - a(n64)
                call nodends(a(n2),numel,numnp,a(n64)) 
                !subroutine edgel(km,nodelm,numel,numnp,kmedge,kedge,numedg)
                call edgel(a(n2),a(n64),numel,numnp,a(n61),a(n62),
     *                 a(n63),numedg) 
                
                
                if(icore.eq.2) then 

! connectivities between oxide and metal subgrids - array a(n55) -
        call core_connect(a(n2),a(n55),a(n94),mcore,numel,ipof)

!ss        call prin_kmedge(a(n62),a(n2),numedg,3280)

! data for peripheral melting criterion -
       call crit_side(pr1,pz1,pr2,pz2,                                     &
     & tollin,elemax,a(n1),a(n94),numel,a(n2),linel,mcore,1)

       call crit_side(pr1,pz1,pr2,pz2,                                     &
     & tollin,elemax,a(n1),a(n94),numel,a(n2),linel,mcore,2)
!
!  FP heat generation -
      if(iabs(npp)+matFP.gt.0)                                                         &
     & call ini_comp(a(n1),a(n2),a(n4),a(n6),numnp,numel,nummat,nummin,     &
     & mnin,m_v_f,mcore,partin,c_dat,nbn_dat,npp,c_mass,FPmass,matFP)

                endif
                
                !end readinf <Elemetns> 
                inFEM=inFEM+1
            end if 
            
            ! end reading <FEModel> Tag
          end subroutine 
          
          ! reading <Materials> Tag 
          subroutine read_materials()
            implicit none
            type(fnode),pointer::mats_node,mn,sub_node !nide of <Materials> Tag; node for child tahs; node for group in liq_v
            type(fnodelist),pointer::m_list,sub_list       !list of <Material> tags; list for reading sub_tags;
            integer::j,jj,k,j_mat,iread     ! loop counter, flag if attribute readed
            integer:: npbuf,s_xml,mbuf      ! just a buffer, read "serial" attribute ; remember m buffer
            character(len = 20) :: abuf     ! buffer for reading strings 
            character(len = 20) :: arc(20)  ! buffer for reading strings
            real :: read_mp(32)                 ! array for reading mprop in type=3,4 (max 4*8 type=4)
            !INNER <MATERIALS> PARAMETERS
            integer :: m, icntrl , inb 
            character*5 nam1*3,nam2*7
            integer::  mtp1,mtp2,mtp2b          ! inner parameter of input.for ?
            real :: dthot                       ! inner parameter of input.for ?
            integer :: mcn , m_count            ! inner parameter of input.for ?
            ! integeR :: mgas
            integer :: maxlay , mnw             ! inner parameter of input.for ?
            integer :: n_mat_rem , mtn          ! count and check materials order 
            
            write(logio,*) 'Start reading <Materials> Tag'
            myList=>getElementsByTagName(MyNode0,"Materials")
            if(getLength(myList) == 0) then 
                call warning('No tag <Materials> in input file',0)
                return
            end if 
            mats_node=>item(myList,0)
            if (.not.associated(mats_node)) then
                call warning('cant link to Materials')
                return 
            end if 
            
            n_mat_rem = 0 ! no materials at the start 
            ! 
            matform=1    !  - command input of material data 
            m=0
            m_count=0
            mcn=m_count+1
            mtp2=0
            laym=0
            maxlay=0
            dliq=0   !  array(190)
            !
            !   zero material data -
            !       do k=1,90
            do k=1,190
                do j=1,3
                    nbmr(k,j)=0
                enddo
            tpw(k)=0.e0
            nwm(k)=0
            nord(k)=0
            mtype(1,k)=0

            mtype(2,k)=0
            rho(k)=0 
            tlat(k)=0 
            hlat(k)=0 
            angg(k)=0
            genm(k)=0
            ncgenm(k)=0 
            inpname(k)=' ' 
            lname(k)=' '
            mnum(k)=0
                do jj=1,8
                    ptemp(jj,k)=0 
                    cv(jj,k)=0 
                    cond1(jj,k)=0 
                    cond2(jj,k)=0
                enddo
            enddo

            m_list=>getElementsByTagName(mats_node,"Material")
            if (getLength(m_list) == 0) then 
                call warning ('no materials in <Materials> !',0) 
                return
            end if 
            
            ! readning subtags <Material>
            do j_mat = 1, getLength(m_list)
                n_mat_rem = n_mat_rem + 1 ! +1 material
                mn=>item(m_list,j_mat-1) ! mn -> node of <material> subTag ; we start form 0 -> "j-1"
                m = 1
                call GIAt(mn,"number",m,iread)               !612
                if (iread.ne.0) then
                    call warning ('no mat number !')
                    m=n_mat_rem                         ! no number -> take it from counter
                else 
                    if(m/=n_mat_rem)then 
                        call warning('wrong number in mat')
                    end if 
                end if 
                call checkmatn(m,mnum)  
                call GIAt(mn,"type",mtp1)
                call GIAt(mn,"max_point",mtp2b)     
                mtp2=mtp2b
                mtype(1,m)=mtp1
                mtype(2,m)=mtp2                   ! number of points
                icntrl=1
                call GCAt(mn,"mname",abuf,iread)             !614
                if (iread == 0) then 
                    inpname(m)=abuf
                    nam1=abuf(1:3)
                    nam2=abuf(4:10)
                    lname(m)=nam2(4:7)
                    icntrl=icntrl+1         ! = 2
                else 
                    call warning('no matname!')
                end if 
                
                call GRAt(mn, "dens", rho(m),iread)       !616
                if(iread==0)then 
                    if(rho(m).eq.0.) rho(m)=1.e-3
                
                    !CORE agreed densities -
                    if (icore == 2 .and. iHEF > 1) then 
                        do j = 1,nummin
                            if (mname(j).eq.'Zr') then 
                                matn = mnin(j)
                                if(matn.eq.m) rho(m)=de_zr
                            elseif(mname(j).eq.'UO2') then
                                matn=mnin(j)
                                if(matn.eq.m) rho(m)=dfuel
                            elseif(mname(j).eq.'ZrO2') then
                                matn=mnin(j)
                                if(matn.eq.m) rho(m)=de_zro2
                            elseif(mname(j).eq.'SS') then
                                matn=mnin(j)
                                !if(matn.eq.m) rho(m)=de_ss
                            end if
                        end do 
                    end if 
                    icntrl=icntrl+1  ! =3 
                    
                    ! IDK what it is ...
                    do  jj=1,nummin
                        if(m.ne.mnin(jj)) exit
                        dthot=-50.      !shift of thot
                        !thot =GetTemLiq(masindex(1))+dthot
                        thot =3150. ! [K]
                        !tem=thot
                        inb=jj
                        rhoin(inb)=rho(m)
                    end do
                    
                end if
                
                ! read <Lat_heat> Tag                        !618
                sub_list=>getElementsByTagName(mn,"Lat_heat")
                if (getLength(sub_list) .ne. 0) then 
                    sub_node=>item(sub_list,0) 
                    call GRAt(sub_node, "tlat", tlat(m),iread)
                    if(iread.ne.0)call GRAt_old(sub_node,tlat(m),1)
                    call GRAt(sub_node, "hlat", hlat(m),iread)
                    if(iread.ne.0)call GRAt_old(sub_node,hlat(m),1)
                    call GRAt(sub_node, "dliq", dliq(m),iread)
                    if(iread.ne.0)call GRAt_old(sub_node,dliq(m),1)
                    icntrl = icntrl + 1 ! = 4 
                end if 
                
                ! read <Heat_gen> Tag                       !620
                sub_list=>getElementsByTagName(mn,"Heat_gen")
                if (getLength(sub_list) .ne. 0) then 
                    sub_node=>item(sub_list,0) 
                    call GIAt(sub_node, "ncgenm", ncgenm(m),iread)
                    if(iread.ne.0)call GIAt_old(sub_node,ncgenm(m),1)
                    call GRAt(sub_node, "genm", genm(m),iread)
                   if(iread.ne.0)call GRAt_old(sub_node,genm(m),1,iread)
                    if(iread == 0 .and.genm(m).ne.0.) igenm=1
                end if 
                
                call GRAt(mn,"ortho_angle",angg(m))          !622
                
                ! read <Liqngb> Tag                          !624
                sub_list=>getElementsByTagName(mn,"Liqngb")
                if (getLength(sub_list) .ne. 0) then 
                    sub_node=>item(sub_list,0) 
                    call GIAt(sub_node, "nbmr1", nbmr(m,1),iread)
                    if(iread.ne.0)call GIAt_old(sub_node,nbmr(m,j),1)
                    call GIAt(sub_node, "nbmr2", nbmr(m,2),iread)
                    if(iread.ne.0)call GIAt_old(sub_node,nbmr(m,j),2)
                    call GIAt(sub_node, "nbmr3", nbmr(m,3),iread)
                    if(iread.ne.0)call GIAt_old(sub_node,nbmr(m,j),3)
                    
                    call GIAt(sub_node, "nwm", nwm(m),iread)
                    if(iread.ne.0)call GIAt_old(sub_node,nwm(m),4)
                    
                    call GRAt(sub_node, "tpw", tpw(m),iread)
                    if(iread.ne.0)call GRAt_old(sub_node,tpw(m),5)
                                  
                    if(abs(tpw(m)).lt.1.e-20.and.tlat(m).gt.1.e-20)then
                        tpw(m)=tlat(m)
                    end if 
                    if(nwm(m).eq.0) nwm(m)=iabs(lim(1))
                    
       !  .... set default values -
        if(iabs(nbmr(m,1))+iabs(nbmr(m,2))+iabs(nbmr(m,3))>0) then
            do j=1,3
                if(nbmr(m,j).eq.0) nbmr(m,j)=iabs(lim(1))
            enddo
        endif
       !08-12-2012
       if(nwm(m).eq.0) then
         if((m.ne.m_vess(1)).and.(m.ne.m_vess(2))) nwm(m)=iabs(lim(1))
!!!         if((m.eq.m_vess(1)).and.(invers.eq.-1)) then
         if((m.eq.m_vess(1)).and.(invers.lt. 1)) then
           nwm(m)=iabs(lim(1))
!!!         elseif((m.eq.m_vess(1)).and.(invers.eq.1)) then
         elseif((m.eq.m_vess(1)).and.(invers.ge.1)) then
           nwm(m)=iabs(lim(2))
!!!         elseif((m.eq.m_vess(2)).and.(invers.eq.-1)) then
         elseif((m.eq.m_vess(2)).and.(invers.lt. 1)) then
           nwm(m)=iabs(lim(2))
!!!         elseif((m.eq.m_vess(2)).and.(invers.eq.1)) then
         elseif((m.eq.m_vess(2)).and.(invers.ge.1)) then
           nwm(m)=iabs(lim(1))
         endif
       endif
                !.......... molten debris:
                    do j=1,2
                    if(m.eq.iabs(lim(j))) then
                        if(tlat(m).gt.0.) then
                            tlatli(j) = -tlat(m)
                            !write(99,*) '==== Fixed liquidus for layer',is,':',tlat(m)
                            endif
                            tpwl(j)=tpw(m)
                            do jj=1,3
                                nbmr(m,jj)=iabs(lim(j))
                            enddo
                            nwm(m)=iabs(lim(j))
                        endif
                    enddo
                    npwr=1         ! there is "liq"-material
                end if 
                
                ! reading <Conv_cond> or <Conv_cond2> Tag    !626
                sub_node=>null()
                sub_list=>getElementsByTagName(mn,"Conv_cond")
                if (getLength(sub_list) .ne. 0) then 
                    sub_node=>item(sub_list,0)
                else 
                    sub_list=>getElementsByTagName(mn,"Conv_cond2")     !+2 parameters
                    if (getLength(sub_list) .ne. 0) then 
                        sub_node=>item(sub_list,0)
                    else 
                        !call warning('no convcond at all',0) 
                    end if  
                end if
                if (associated(sub_node)) then
                    call GCAt(sub_node,"flag",abuf,iread)
                    if(iread.ne.0)call GCAt_old(sub_node,abuf,1)
                    if(abuf(1:3)=='yes') then
                        if(getNodeName(sub_node)=="Conv_cond")then
                            npbuf = 6
                        elseif(getNodeName(sub_node)=="Conv_cond2")then
                            npbuf = 8
                        else 
                            call warning ('how?')
                        end if 
                        if(iread == 0) then 
                         call GRAr(sub_node,con_con(:,m),n=npbuf)
                        else    ! this logic need to change...
                            call GCAr(sub_node,arc,20,n=npbuf+1)
                            do k=1,npbuf
                                read(arc(j+1),*)con_con(j,m)
                            end do
                        end if 
                        
                        ! assign default values of the parametres if they inputed nonzero
                    if(abs(con_con(1,m)).lt.1.e-20) con_con(1,m)=0.01 ! for criterion in FEM_CF  

                    if(abs(tcn).lt.abs(con_con(1,m)))  tcn=con_con(1,m)
                    if(abs(dtc).lt.abs(con_con(2,m)))  dtc=con_con(2,m)
                    if(abs(cnr).lt.abs(con_con(3,m)))  cnr=con_con(3,m)
                    if(abs(cnz).lt.abs(con_con(4,m)))  cnz=con_con(4,m)
                    if(abs(tcn0).lt.abs(con_con(5,m))) tcn0=con_con(5,m)  ! tcn0=tcn0+timin default ...
                    if(abs(tcne).lt.abs(con_con(6,m))) tcne=con_con(6,m)
         
                    elseif(abuf(1:2)=='no')then
                        con_con(1:11,m)=0
                    else 
                        call warning('strange flag in <conv_cond2>')
                    end if 
                end if 
                
                call GCAt(mn,"lname",abuf,iread)             !628
                if (iread ==0) then 
                    lname(m) = abuf
                    if (lname(m) .eq. 'shri')then 
                        ishrin = -1
                        mgas_xml=m      !shrink material
                    end if 
                    if (lname(m)(1:3) .eq. 'rad')then
                        call GRAt(mn,"absorb",absorb(m))
                        if (lname(m)(1:4)=='rads')then
                            call GRAt(mn,"cuta",cuta)
                        end if 
                    end if
                end if 
                
                ! reading <M_prop> Tag                        !630 - 638 
                sub_list=>getElementsByTagName(mn,"M_prop")
                if (getLength(sub_list) .ne. 0) then
                  sub_node=>item(sub_list,0)
                  if(mtype(1,m)==1)then     ! Type 1         !630
                    call GRAt(sub_node,"h_cap",cv(1,m),iread)           ! reading heat capacity
                    if(iread.ne.0)call GRAt_old(sub_node,cv(1,m),1)
                    call GRAt(sub_node,"h_cond",cond1(1,m),iread)       ! reading heat conductivity
                    if(iread.ne.0)call GRAt_old(sub_node,cond1(1,m),2)
                    call assgn2_a(cv(1,m),b,1)                          ! and this is what? 
                  end if 
                  if(mtype(1,m)==2)then     ! Type 2         !632
                    call GRAt(sub_node,"h_cap",cv(1,m),iread)           ! reading heat capacity
                    if(iread.ne.0)call GRAt_old(sub_node,cv(1,m),1)
                    call GRAt(sub_node,"h_cond1",cond1(1,m),iread)      ! reading heat conductivity  -X
                    if(iread.ne.0)call GRAt_old(sub_node,cond1(1,m),2)
                    call GRAt(sub_node,"h_cond2",cond2(1,m),iread)      ! reading heat conductivity  -Y
                    if(iread.ne.0)call GRAt_old(sub_node,cond2(1,m),2)
                  end if 
                  if(mtype(1,m)==3)then     ! Type 3         !634
                    if(mtp2 < 2)call warning('incorrect n of points',0)
                    call GRAr(sub_node,read_mp,n=mtp2*3)     
                    ptemp(1:mtp2,m) = read_mp(1:mtp2)             ! reading temperatures
                    cv   (1:mtp2,m) = read_mp(mtp2+1:mtp2*2)      ! reading heat capacityies 
                    cond1(1:mtp2,m) = read_mp(mtp2*2+1:mtp2*3)    ! reading heat conductivites 
                    !check if temeratures are increasing
                    do k=2,mtp2
                        if(ptemp(k,m)<ptemp(k-1,m)) then
                            call warning('decrease T in <mprop>!')
                        end if 
                    end do 
                  end if 
                  if(mtype(1,m)==4)then     ! Type 4         !636
                    if(mtp2 < 2)call warning('incorrect n of points',0)
                    call GRAr(sub_node,read_mp,n=mtp2*4)     
                    ptemp(1:mtp2,m) = read_mp(1:mtp2)             ! reading temperatures
                    cv   (1:mtp2,m) = read_mp(mtp2+1:mtp2*2)      ! reading heat capacityies 
                    cond1(1:mtp2,m) = read_mp(mtp2*2+1:mtp2*3)    ! reading heat conductivites 1
                    cond2(1:mtp2,m) = read_mp(mtp2*3+1:mtp2*4)    ! reading heat conductivites 2
                    !check if temeratures are increasing
                    do k=2,mtp2
                        if(ptemp(j,m)<ptemp(j-1,m)) then
                            call warning('decrease T in <mprop>!')
                        end if 
                    end do
                  end if 
                  if(mtype(1,m)==5)then     ! Type 5         !638
                    call GRAt(sub_node,"h_cap",cv(1,m),iread)           ! reading heat capacity
                    if(iread.ne.0)call GRAt_old(sub_node,cv(1,m),1)
                    call GRAt(sub_node,"h_cond1",cond1(1,m),iread)      ! reading heat conductivity  -X
                    if(iread.ne.0)call GRAt_old(sub_node,cond1(1,m),2)
                    call GRAt(sub_node,"h_cond2",cond2(1,m),iread)      ! reading heat conductivity  -Y
                    if(iread.ne.0)call GRAt_old(sub_node,cond2(1,m),2)
                    
                    ! IDK ....
                    do j=1,2
                        if(m.eq.iabs(lim(j))) then
                            do jj=1,3
                                nbmr(m,jj)=iabs(lim(j))
                            enddo
                            nwm(m)=iabs(lim(j))
                        endif
                    enddo
                  end if 
                  icntrl=icntrl+1  ! =5 
                end if 
                
                call GIAt(mn,"lay_order",nord(m),iread)       !640
                if (iread==0) then
                    laym=laym+1
                    maxlay=max(maxlay,iabs(nord(m))) ! negative nord enumerates outer layers
                end if 
                
                ! reading <Powlow> Tag                     !648  dont know what it is 
                if(is_Tag(mn,sub_node,"Powlow"))then
                    
                    call GRAt(sub_node,"hpowlow",hpowlow,iread)      ! - read heat gen. multiplier
                    if(iread/=0)call GRAt_old(sub_node,hpowlow,1)
                    call GRAt(sub_node,"powlow",powlow,iread)        ! - read power
                    if(iread/=0)call GRAt_old(sub_node,powlow,2)
                    call GIAt(sub_node,"ncurv_hp",ncurv_hp,iread)    ! - read N curve for T dependence
                    if(iread/=0)call GIAt_old(sub_node,ncurv_hp,3)
                
                end if 
                
                ! call GIAt(mn,"endmat",)                     !644 just do this in the end of reading <material>
                ! close reading material before reading "serial" 
                m_count=m_count+1
                mtp2=1
                mbuf=m
                m=0
                if(icntrl.ne.5) then   !  counter of input indispensable parametres
                    call warning ('u forget something... icntrl != 5',0)
                end if 
                ! reading serial 
                s_xml = 0       ! if no serial do (1->0) nothing
                call GIAt(mn,"serial",s_xml)                 !646  dont know if it used
                n_mat_rem = n_mat_rem + s_xml       ! add serial mats to counter
                do j = 1, s_xml
                    mnw=mbuf+j
                    inpname(mnw)=inpname(mbuf)
                    call assgn_mn_a(mtype(1,mbuf),mtype(1,mnw),2,1)
                    call assgn_a(rho(mbuf),rho(mnw),1)
                    call assgn_a(tlat(mbuf),tlat(mnw),1)
                    call assgn_a(hlat(mbuf),hlat(mnw),1)
                    call assgn_a(angg(mbuf),angg(mnw),1)
                    call assgn_a(genm(mbuf),genm(mnw),1)
                    call iassgn_a(ncgenm(mbuf),ncgenm(mnw),1)
                    call assgn_mn_a(ptemp(1,mbuf),ptemp(1,mnw),8,1)
                    call assgn_mn_a(   cv(1,mbuf),   cv(1,mnw),8,1)
                    call assgn_mn_a(cond1(1,mbuf),cond1(1,mnw),8,1)
                    call assgn_mn_a(cond2(1,mbuf),cond2(1,mnw),8,1)
                    do k=1,3
                        nbmr(mnw,k)=nbmr(mbuf,k)  ! - neighbour materials
                    enddo
                    lname(mnw)=lname(mbuf)
                    absorb(mnw)=absorb(mbuf)
                    nwm(mnw)=nwm(mbuf)       ! - resulting material
                    dliq(mnw)=dliq(mbuf)
                    m_count=m_count+1
                    laym=laym+1
                    nord(mnw)=nord(mbuf)+j*isign(1,nord(mbuf))
                    maxlay=max(maxlay,iabs(nord(mnw))) ! negative nord enumerates outer layers
                    !maxlay=max(maxlay,nord(mnw))
                end do
                
            end do
            
            
            ! Initialize inputed materilas data
            if (n_mat_rem /= nummat)then
                call warning ('dismatch nummat and materials count')
            end if 
            
            if(laym.ne.maxlay) then
                call warning ('dismatch layers order')
            end if 
            
            ! fill A array
            inb=1
            do k=1,nummat
                 nam1=inpname(k)
                 call assgn_a(mtype,a(n5),2*nummat)
                 call assgn_a(rho,a(n6),nummat)
                 call assgn_a(tlat,a(n7),nummat)
                 call assgn_a(hlat,a(n8),nummat)
                 call assgn_a(angg,a(n13),nummat)
                 call assgn_a(genm,a(n15),nummat)
                 call assgn_a(ncgenm,a(n14),nummat)
                 call assgn_a(ptemp,a(n9),8*nummat)
                 call assgn_a(  cv,a(n10),8*nummat)
                 call assgn_a(cond1,a(n11),8*nummat)
                 call assgn_a(cond2,a(n12),8*nummat)
            end do 
            
            if(nummin.gt.0) then
                do j=1,nummin
                    mtn=mnin(j)
                    if(rho(mtn).lt.rhomin) rhomin=rho(mtn)
                enddo
            endif
            
            in_matrl=1  !  whether material data were inputed
            ! end reading materials 
             
          end subroutine read_materials
          
          ! reading <Corcat> Tag 
          subroutine read_corcat()
           implicit none 
                type(fnode),pointer::corcat_node,sub_node,gr_node !nide of <passing> Tag; node for child tahs; node for group in liq_v
                type(fnodelist),pointer::sub_list,gr_list !list for reading sub_tags; list of groups 
                
                integer(4), pointer :: i_ar_p(:)  !pointer to read integer arrays 
                real, pointer :: r_ar_p(:)     !pointer to read real arrays 
                integer :: iread   !flag: iread = 0 if attr readed; iread = 1 if attr is unreaded
                integer :: n_array,j,k !counter of array mass; loop counters
                logical :: sacr_ok ! flag if there is a <sacrm> or <msacr> Tag
                integer :: npbuf ,ifem8 ,npen ! buffer for reading integer attributes, inner params
                real::tmpread,ar_r(10)      ! buffer for reading real      attributes
                integer::ar_i(10)           ! buffer for reading integer   attributes, which can be readed by array 
                character(len=20)::ar_c(20) ! buffer for reading character attributes
                write (logio,*)'Start reading expert' 
                
                myList=>getElementsByTagName(MyNode0,"Corcat")
                if(getLength(myList) == 0) then 
                    call warning('No tag <corcat> in input file',0)
                    return
                end if 
                corcat_node=>item(myList,0)
                if (.not.associated(corcat_node)) then
                    call warning('cant link to corcat') 
                end if 
                
                ! reading <Ccpit> Tag                        !512 
                sub_list=>getElementsByTagName(corcat_node,"Ccpit")
                if(getLength(sub_list) > 0) then 
                    sub_node=> item(sub_list,0) 
                    call GRAt(sub_node, "zpit",zpit,iread)
                    if (iread .ne. 0) call GRAt_old(sub_node,zpit,1)
                    call GRAt(sub_node, "hpit",hpit,iread)
                    if (iread .ne. 0) call GRAt_old(sub_node,hpit,2)
                    call GRAt(sub_node, "rpit",rpit,iread)
                    if (iread .ne. 0) call GRAt_old(sub_node,rpit,3)
                end if
                
                call GIAt(corcat_node,"i_hext",i_hext)          !514 
                
                ! read <Flood> Tag                              !516
                sub_list=>getElementsByTagName(corcat_node,"Flood")
                if(getLength(sub_list) > 0) then 
                    sub_node=> item(sub_list,0) 
                    call GRAt(sub_node, "flood_time",flood_time,iread)
                    if(iread.ne.0)call GRAt_old(sub_node,flood_time,1)
                    if(flood_time < 0)then
                        call warning('flood time < 0 ->rel. time!',0)
                        flood_time = - flood_time + timin
                    end if 
                    call GIAt(sub_node, "nflood",nflood,iread)
                    if(iread.ne.0)call GIAt_old(sub_node,nflood,2) 
                    call GRAt(sub_node, "teflood",teflood,iread)
                    if(iread.ne.0)call GRAt_old(sub_node,teflood,3)
                    call GRAt(sub_node, "enflood",enflood,iread)
                    if(iread.ne.0)call GRAt_old(sub_node,enflood,4)
                end if
                
                !call GRAt(corcat_node,"floodrad",)          !518         ! not used ? 
                !call GRAt(corcat_node,"floodcrit",)         !520         ! not used ? 
                
                call GIAt(corcat_node,"ncomin",ncomin)       !521
                call GRAt(corcat_node,"cfZr",cfZr)           !522
                call GIAt(corcat_node,"powCC",ipwCC)         !523 
                
                !reading Tag <Ireloc>                        !526
                sub_list=>getElementsByTagName(corcat_node,"Ireloc")
                if(getLength(sub_list) > 0) then
                  call WCC('ireloc')
                  ifilin=1            !   INFILL is called
                  sub_node=>item(sub_list,0)
                  call GIAt(sub_node,"ireloc",ireloc,iread)
                  if(iread.ne.0)call GIAt_old(sub_node,ireloc,1)
                  
                  call GRAt(sub_node,"treloc",tmpread,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,tmpread,2)
                  if(tmpread>0) treloc=tmpread+timin   ! prescriped time of material relocation 
                  
                  call GRAt(sub_node,"terel",tmpread,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,tmpread,3)
                  if(tmpread>0) terel = tmpread         ! prescriped temperature of relocated material (default=2000 K)
                  
                  call GRAt(sub_node,"hgs",tmpread,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,tmpread,4)
                  if(tmpread>0) hgs = tmpread           ! Heihgt ...
                end if 
                
                call GRAt(corcat_node,"cfsacr",cfsacr)       !527 
                
                !reading <Invuo2> Tag                        !530       
                sub_list=>getElementsByTagName(corcat_node,"Invuo2")
                if(getLength(sub_list) > 0) then 
                  sub_node=>item(sub_list,0) 
                  call GRAt(sub_node,'uo2min',uo2min,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,uo2min,1)
                  call GRAt(sub_node,'uo2max',uo2max,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,uo2max,2)
                  call GRAt(sub_node,'uo2tim',uo2tim,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,uo2tim,3)
                end if 
                
                ! reading <Timencl> Tag                      !532
                sub_list=>getElementsByTagName(corcat_node,"Timencl")
                if(getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0)
                  !call GIAt(sub_node,"n",npbuf,iread)
                  call GRAr(sub_node,tm_erad,check=npbuf)
                  if (iread==0 .and. npbuf > 20) call warning('timencl')
                  if (npbuf<0) then ! if relative values
                    tm_erad(1:abs(npbuf))=timin+tm_erad(1:abs(npbuf))
                  end if
                  timencl=tm_erad(1)
                end if 
                
                call GIAt(corcat_node,"ierbd",ierbd)         !534 
                if (iread==0 .and. ierbd > 0 .and. nrseg == 0) then 
                  call warning('no enclosure b.c. inputed for rebuild')
                end if 
                
                  !reading <Gascond> Tag                       !536
                sub_list=>getElementsByTagName(corcat_node,"Gascond")
                if(getLength(sub_list) > 0) then 
                  sub_node=>item(sub_list,0) 
                  call GRAt(sub_node,'gascond1',gascond(1),iread)
                  if(iread.ne.0)call GRAt_old(sub_node,gascond(1),1)
                  call GRAt(sub_node,'gascond2',gascond(2),iread)
                  if(iread.ne.0)call GRAt_old(sub_node,gascond(2),2)
                end if 
                
                ! reading Tags <Msacr> or <Sacrm>            !538
                sacr_ok = .not. .true.
                sub_list=>getElementsByTagName(corcat_node,"Msacr")
                if(getLength(sub_list) == 0) then
                  sub_list=>getElementsByTagName(corcat_node,"Sacrm")
                  if(getLength(sub_list) > 0) then
                    call WCC('msacr')
                    sacr_ok = .true. 
                  else 
                    write (logio,*)'No <sacrm> or <msacr> in <expert>'
                  end if 
                else 
                  call WCC('msacr')
                  sacr_ok = .true. 
                end if 
                if(sacr_ok)then 
                  sub_node=>item(sub_list,0) 
                  call GIAr(sub_node, msacr, check=npbuf)
                end if 
                
                !reading <Cfh2> Tag                          !539 
                sub_list=>getElementsByTagName(corcat_node,"Cfh2")
                if(getLength(sub_list) > 0) then
                    sub_node=>item(sub_list,0)
                    do j = 1,3 
                        call GRAt_old(sub_node,cfH2(1,j),2*j-1)
                        call GRAt_old(sub_node,cfH2(2,j),j*2)
                    end do
                end if 
  
                call GRAt(corcat_node,"tfeox",tmpread,iread) !540  
                if(iread==0) then 
                    chem_t_fe_ox = tmpread
                end if 
                call GRAt(corcat_node,"qhemabl",tmpread,iread)!542
                if(iread==0) then 
                    chem_q_hem_abl = tmpread
                end if
                call GIAt(corcat_node,"uo2ox",npbuf,iread)   !544       Maybe change the 0/1 system to On/Off ?
                if (iread == 0) then
                    if (npbuf == 0) chem_uo2ox = .true.
                end if 
                
                call GRAt(corcat_node,"themabl",tmpread,iread)!546
                if(iread==0) then 
                    chem_t_hem_abl = tmpread
                end if 
                
                !   include all bub_ elements into <Bubbles> tag? 
                if(is_Tag(corcat_node,sub_node,"Bubbles"))then
                
                !Reading <Bubgas> Tag                         !548 
                !sub_list=>getElementsByTagName(corcat_node,"Bubgas")
                !if(getLength(sub_list) > 0) then
                !    sub_node=>item(sub_list,0)
                !    call GCAr(sub_node,ar_c,20,check = bub_n) !S_LEN = 20 , OK?
                !    allocate(bub_gases(bub_n))
                !    bub_gases(1:bub_n) = ar_c(1:bub_n) 
                !end if 
                
                call GCAr(sub_node,ar_c,20,check = bub_n) !S_LEN = 20 , OK?
                if (bub_n>0) then
                    allocate(bub_gases(bub_n))
                    bub_gases(1:bub_n) = ar_c(1:bub_n)
                end if
                
                call GRAt(sub_node,"bub_rhog",tmpread,iread)!5450
                if (iread == 0) then
                    bub_rhog = tmpread
                end if 
                call GRAt(sub_node,"bub_sigma",tmpread,iread)!552
                if (iread == 0) then
                    bub_sigma = tmpread
                end if 
                call GRAt(sub_node,"bub_mu",tmpread,iread)    !554
                if (iread == 0) then
                    bub_mu = tmpread
                end if 
                call GRAt(sub_node,"bub_r",tmpread,iread)    !556
                if (iread == 0) then 
                  if (tmpread.ne.0) bub_r = tmpread
                end if 
                
                end if
            end subroutine read_corcat    
            
          ! reading <Expert> Tag 
          subroutine read_expert()
                implicit none 
                type(fnode),pointer::expert_node,sub_node,gr_node !nide of <passing> Tag; node for child tahs; node for group in liq_v
                type(fnodelist),pointer::sub_list,gr_list !list for reading sub_tags; list of groups 
                
                integer(4), pointer :: i_ar_p(:)  !pointer to read integer arrays 
                real, pointer :: r_ar_p(:)     !pointer to read real arrays 
                integer :: iread   !flag: iread = 0 if attr readed; iread = 1 if attr is unreaded
                integer :: n_array,j,k !counter of array mass; loop counters
                logical :: sacr_ok ! flag if there is a <sacrm> or <msacr> Tag
                integer :: npbuf ,ifem8 ,npen ! buffer for reading integer attributes, inner params
                real::tmpread,ar_r(10)      ! buffer for reading real    attributes
                integer::ar_i(10) ! buffer for reading attributes, which can be readed by array 
                write (logio,*)'Start reading expert' 
                
                myList=>getElementsByTagName(MyNode0,"Expert")
                if(getLength(myList) == 0) then 
                    call warning('No tag <expert> in input file',0)
                    return
                end if 
                expert_node=>item(myList,0)
                if (.not.associated(expert_node)) then
                    call warning('cant link to expert') 
                end if 
                
                !call GIAt(sub_node,"check",)                   !412 just a check command 
                !call GIAt(sub_node,"ipof",)                    !414 readed in general? 
                call GIAt(expert_node,"icorner",icorner)        !416
                !call GIAt(sub_node,"powintm",)                 !418  no used
                call GRAt(expert_node,"dtsolv",dtsolv)          !420
                if(abs(dtsolv).lt.1.e-30)then
                  call warning('too small dtsolv! dtsolv=1.e-5,falg=0')
                  dtsolv=1.e-5
                end if 
                
                call GRAt(expert_node,"dtmelt",dtmelt)          !422
                call GRAt(expert_node,"timslv",timslv)          !424
                call GRAt(expert_node,"betat",betat)            !426
                call GIAt(expert_node,"npen",npen)              !427
                call GIAt(expert_node,"ichec_all",ichec_all)    !428
                call GIAt(expert_node,"ifem8",ifem8)            !429   inner parameter of input.for

                !readnig CC data? 
                call GRAt(expert_node,"tfeox",tmpread,iread)    !430
                !if (iread ==0)  !call warn_CC("tfeox")   !   --> ?
                if(iread==0) then 
                    call WCC('tfeox')
                    chem_t_fe_ox = tmpread
                end if 
                call GRAt(expert_node,"qhemabl",tmpread,iread)  !432
                if(iread==0) then 
                    call WCC('qhemabl')
                    chem_q_hem_abl = tmpread
                end if
                call GIAt(expert_node,"uo2ox",npbuf,iread)      !434       Maybe change the 0/1 system to On/Off ?
                if (iread == 0) then 
                    call WCC('uo2ox') !chem_uo2ox = .true.
                    if (npbuf == 0) chem_uo2ox = .true.
                end if 
                
                call GRAt(expert_node,"themabl",tmpread,iread)  !436
                if(iread==0) then 
                    call WCC('themabl')
                    chem_t_hem_abl = tmpread
                end if 
                
                !reading <Invuo2> Tag                        !437       possibly a sub tag
                sub_list=>getElementsByTagName(expert_node,"Invuo2")
                if(getLength(sub_list) > 0) then 
                  call WCC ('Invuo2')
                  sub_node=>item(sub_list,0) 
                  call GRAt(sub_node,'uo2min',uo2min,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,uo2min,1)
                  call GRAt(sub_node,'uo2max',uo2max,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,uo2max,2)
                  call GRAt(sub_node,'uo2tim',uo2tim,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,uo2tim,3)
                end if 
                
                !reading <gascond> Tag                       !438
                sub_list=>getElementsByTagName(expert_node,"Gascond")
                if(getLength(sub_list) > 0) then 
                  call WCC ('Gascond')
                  sub_node=>item(sub_list,0) 
                  call GRAt(sub_node,'gascond1',gascond(1),iread)
                  if(iread.ne.0)call GRAt_old(sub_node,gascond(1),1)
                  call GRAt(sub_node,'gascond2',gascond(2),iread)
                  if(iread.ne.0)call GRAt_old(sub_node,gascond(2),2)
                end if 
                
                ! reading Tags <Msacr> or <Sacrm>            !439
                sacr_ok = .not. .true.
                sub_list=>getElementsByTagName(expert_node,"Msacr")
                if(getLength(sub_list) == 0) then
                  sub_list=>getElementsByTagName(expert_node,"Sacrm")
                  if(getLength(sub_list) > 0) then
                    call WCC('msacr')
                    sacr_ok = .true. 
                  else 
                    write (logio,*)'No <sacrm> or <msacr> in <expert>'
                  end if 
                else 
                  call WCC('msacr')
                  sacr_ok = .true. 
                end if 
                
                if(sacr_ok)then 
                  sub_node=>item(sub_list,0) 
                  call GIAr(sub_node, msacr, check=npbuf)
                end if 

                
                call GIAt(expert_node,"itinp",itinp,iread)      !440 
                if(iread==0) icmtrl = 0 ! see Minput(442)
            
            !reading <Minput> Tag                       !442
            if(is_Tag(expert_node,sub_node,"Minput"))then
              ifilin = 1
              icmtrl = icmtrl + 1
              call GRAt(sub_node,"timtrans",timtrans(icmtrl),iread)
              if(timtrans(icmtrl)<0.) then
                if(icmtrl.eq.1)then
                 timtrans(icmtrl)=timin-timtrans(icmtrl)
                elseif(icmtrl.gt.1)then
                 timtrans(icmtrl)=timtrans(icmtrl-1)-timtrans(icmtrl)
                endif
              endif
              
              call GRAr(sub_node,tranmass(icmtrl,:),n=nummin)
              call GRat(sub_node,"tranenth",tranenth(icmtrl))
              call GRat(sub_node,"trantemp",trantemp(icmtrl))
            end if
            
                call GIAt(expert_node,"ntempdata",ntempdata) !444
                
                !reading <Ireloc> Tag                        !446
                sub_list=>getElementsByTagName(expert_node,"Ireloc")
                if(getLength(sub_list) > 0) then
                  call WCC('ireloc')
                  ifilin=1            !   INFILL is called
                  sub_node=>item(sub_list,0)
                  call GIAt(sub_node,"ireloc",ireloc,iread)
                  if(iread.ne.0)call GIAt_old(sub_node,ireloc,1)
                  
                  call GRAt(sub_node,"treloc",tmpread,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,tmpread,2)
                  if(tmpread>0) treloc=tmpread+timin   ! prescriped time of material relocation 
                  
                  call GRAt(sub_node,"terel",tmpread,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,tmpread,3)
                  if(tmpread>0) terel = tmpread         ! prescriped temperature of relocated material (default=2000 K)
                  
                  call GRAt(sub_node,"hgs",tmpread,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,tmpread,4)
                  if(tmpread>0) hgs = tmpread           ! Heihgt ...
                end if 
                
                
                ! include all bub_ elements into <bub> tag? 
                if(is_Tag(expert_node,sub_node,"Bubbels"))then
                
                call GRAt(sub_node,"bub_rhog",tmpread,iread)!448
                if (iread == 0) then
                    call WCC('bub_rhog')
                    bub_rhog = tmpread
                end if 
                call GRAt(sub_node,"bub_sigma",tmpread,iread)!450
                if (iread == 0) then
                    call WCC('bub_rhog')
                    bub_sigma = tmpread
                end if 
                call GRAt(sub_node,"bub_mu",tmpread,iread)    !452
                if (iread == 0) then
                    call WCC('bub_rhog')
                    bub_mu = tmpread
                end if 
                call GRAt(sub_node,"bub_r",tmpread,iread)    !454
                if (iread == 0) then 
                  call WCC('bubr') 
                  if (tmpread.ne.0) bub_r = tmpread
                end if 
                
                end if 
                
                ! reading <Timencl> Tag                      !456
                sub_list=>getElementsByTagName(expert_node,"Timencl")
                if(getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0)
                  !call GIAt(sub_node,"n",npbuf,iread)
                  call GRAr(sub_node,tm_erad,check=npbuf)
                  if (iread==0 .and. npbuf > 20) call warning('timencl')
                  if (npbuf<0) then ! if relative values
                    tm_erad(1:abs(npbuf))=timin+tm_erad(1:abs(npbuf))
                  end if
                  timencl=tm_erad(1)
                end if 
                
                ! reading <Mcrust> Tag                       !458
                sub_list=>getElementsByTagName(expert_node,"Mcrust")
                if(getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0)
                  call GIAt(sub_node,"mcrust",mcrust,iread)  !  crust melting flag
                  if (iread==0 .and. mcrust == 0) mcrust =1
                  if (iread.ne.0)then 
                    call GIAt_old(sub_node,mcrust,1,iread)
                    if (iread==0 .and. mcrust == 0) mcrust =1
                  end if
                  call GRAt(sub_node,"rcr",rcr)              !  maximum radius
                  if (iread.ne.0)call GRAt_old(sub_node,rcr,2)
                  call GRAt(sub_node,"zcr",zcr)              !  minimum z   
                  if (iread.ne.0)call GRAt_old(sub_node,zcr,3)  
                end if 
                
                
                ! reading <Nclim> Tag                        !460
                sub_list=>getElementsByTagName(expert_node,"Nclim")
                if(getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0)
                  call GIAt(sub_node,"nclim1",nclim(1),iread) 
                  if(iread.ne.0)call GIAt_old(sub_node,nclim(1),1)
                  call GIAt(sub_node,"nclim2",nclim(2),iread) 
                  if(iread.ne.0)call GIAt_old(sub_node,nclim(2),2)
                end if 
                
                call GIAt(expert_node,"ierbd",ierbd,iread)      !462
                if (iread==0) call WCC('ierbd')
                if (ierbd.ge.0.and.nrseg.eq.0) call warning
     *              ('no enclosure b.c. inputed for rebuilding')
     
                ! reading <Nura> Tag                         !464
                sub_list=>getElementsByTagName(expert_node,"Nura")
                if(getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0)
                  call GIAt(sub_node,"n",npbuf) 
                  if(npbuf<1.or.npbuf>20)call warning('nura !=(1:20)')
                  call GRAt(sub_node,"data",cvbe(j)) 
                end if 
                
                ! read <Taverad> Tag                         !466
                sub_list=>getElementsByTagName(expert_node,"Taverad")
                if(getLength(sub_list) > 0) then
                if(nrbc.eq.0)call warning('No RBC for taverad')
                  sub_node=>item(sub_list,0)
                  call GRAt(sub_node,"starttime",timtaver,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,timtaver,1) 
                  call GRAt(sub_node,"tavemin",tavemin,iread)
                  if(iread.ne.0)call GRAt_old(sub_node,tavemin,2)  
                end if 
                
                call GIAt(expert_node,"ithic",ithic,iread)      !468
                if (iread == 0 .and. nrbc == 0) then
                  call warning('No RBC for taverad')
                end if 
                
                ! read <Temlim> Tag                          !470
                sub_list=>getElementsByTagName(expert_node,"Temlim")
                if(getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0)
                  call GRAt(sub_node,"ticut",ticut,iread) 
                  if(iread.ne.0)call GRAt_old(sub_node,ticut,1)
                  call GRAt(sub_node,"temcut1",temcut1,iread) 
                  if(iread.ne.0)call GRAt_old(sub_node,temcut1,2)
                  call GRAt(sub_node,"temcut2",temcut2,iread) 
                  if(iread.ne.0)call GRAt_old(sub_node,temcut2,3)
                end if 
                
                ! start reading Layer heat transfer model
                ! Should be in the end of reading <Expert> Tag
                ! here we change expert_node 
                sub_list=>getElementsByTagName(expert_node,"LayerModel")
                if(getLength(sub_list) > 0)then
                    expert_node=>item(sub_list,0) 
                else 
                    return
                end if 
                
                ! reading <Lay_mod> Tag                      !471
                sub_list=>getElementsByTagName(expert_node,"Lay_mod")
                if(getLength(sub_list) > 0)then
                    sub_node=>item(sub_list,0) 
                    call GIAr(sub_node,ictyp(1:4),4)
                    ictyp(5) = sum(ictyp(1:3))
                end if 
                
                ! reading <lay_mat> Tag                      !472
                sub_list=>getElementsByTagName(expert_node,"Lay_mat")
                if(getLength(sub_list) > 0)then
                    sub_node=>item(sub_list,0) 
                    call GIAt(sub_node,"mmelt",mmelt,iread)
                    if(iread/=0)call GIAt_old(sub_node,mmelt,1)
                    call GIAt(sub_node,"mwall",mwall,iread)
                    if(iread/=0)call GIAt_old(sub_node,mwall,2,iread)
                    if(iread/=0.and.mwall<0)then
                        call warning('mwall <0 ! -> mwall = - mwall')
                        mwall = - mwall
                    end if 
                    call GIAt(sub_node,"mtop",mtop,iread)
                    if(iread/=0)call GIAt_old(sub_node,mtop,3)
                end if 
                
                ! end reading Layer heat transfer model
                ! end reading <expert> Tag
          end subroutine read_expert
          
          ! reading <Passing> Tag
          subroutine read_passing()
                implicit none 
                type(fnode),pointer::passing_node,sub_node,gr_node !nide of <passing> Tag; node for child tahs; node for group in liq_v
                type(fnodelist),pointer::sub_list,gr_list !list for reading sub_tags; list of groups 
                type(fnode),pointer:: stnode ! SubTags node pointer 
                
                integer(4), pointer :: i_ar_p(:)  !pointer to read integer arrays 
                real, pointer :: r_ar_p(:)     !pointer to read real arrays 
                integer :: iread   !flag: iread = 0 if attr readed; iread = 1 if attr is unreaded
                integer :: n_array,j,k !counter of array mass; loop counters
                real::telq_ar(3)       !telq array for reading
                !real::tlqb(10,3)       !telq array ?
                integer::itlq          !telq n of array
                logical::vdt_ok        !flag if any of the <vdt> tags is here
                character(len=20) :: cbuf ! string buffer 
                ! inner parameres of input.for
                
                
                write(logio,*)'Start reading passing' 
                
                !searching tag <passing> 
                myList=>getElementsByTagName(myNode0,"Passing")
                if (getLength(myList) == 0) then
                    call warning('No tag <passing> in input file',0)
                    return
                end if
                passing_node=>item(myList,0)
                if (.not.associated(passing_node)) then
                    call warning('cant link to passing')
                    return 
                end if 
            
                ! reading <Control_group> Tag 
                if(is_Tag(passing_node,stnode,"Control_group"))then
                
                ! reading N of nuliq and telq 
                call GIAt(stnode,"liq_v",liq_v,iread)
                if(iread/=0)call warning('no Liq_v !')
                
                ! reading <Nuliq> Tags -> 
                sub_list=>getElementsByTagName(stnode,"Nuliq")
                if(getLength(sub_list)/=liq_v) then
                    call warning('dismatch liq_v and nuliq')
                end if 
                do j=1,liq_v
                  sub_node=>item(sub_list,j-1)
                  call GIAr(sub_node,nuliq(:,j),check=nls(j))
                end do
                
                ! reading <Telq> Tags 
                sub_list=>getElementsByTagName(stnode,"Telq")
                if(getLength(sub_list)/=liq_v) then
                    call warning('dismatch liq_v and telq')
                end if
                do j=1,liq_v
                  sub_node=>item(sub_list,j-1)
                  call GRAr(sub_node,telq_ar, check = itlq )
                  if (iread==0 .and. itlq .ne. 3) then
                    call warning('<telq> should have triplet',0)
                  end if
                  do k = 1, itlq
                    tlqb(j,k) = telq_ar(k)
                  end do
                  telq1(j)=tlqb(j,1)
                  telq2(j)=tlqb(j,2)
                  telq3(j)=tlqb(j,3)
                end do
                
                !reading <Nsp> Tag                           !320
                sub_list=>getElementsByTagName(stnode,"Nsp")
                if (getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0)
                  call GIAr(sub_node,nnsp,check=nsp)
                  if (nsp>192)then 
                    call warning('nsp>192 -> nsp = 192',0)
                    nsp = 192 
                  else if (nsp < 0)then 
                    call warning('incorrect nsp! shold be >0 ')
                  end if 
                  if (nsp .ne. 0) then 
                    !check for no duplicates in <nsp> tag array
                    do j = 1,nsp-1
                      do k = j+1, nsp 
                        if (nnsp(j) == nnsp(k))then
                          write(logio,*)'duplicates in <nsp> on: ',j,k
                        end if 
                      end do 
                    end do
                  end if 
                end if 
                          
                call GIAt(stnode,"nhot",nhot,iread)          !321
                if(iread==0.and.nsp<nhot)then 
                  call warning('nhot > nsp! -> nhot = nsp',flag=0)
                  nhot=nsp
                end if 
                
                ! reading <Nmhot> Tag                        !322
                sub_list=>getElementsByTagName(stnode,"Nmhot")
                if (getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0) 
                  !call GIAt(sub_node,"n", nmhot)
                  call GIAr(sub_node, mhot(1:nmhot),check=nmhot)
                  if (nmhot < 0) then 
                    call warning('nmhot: n<0! we take it without -',0)
                    nmhot = -nmhot
                  else if (nmhot > nsp) then
                    call warning('nmhot > nsp! -> nmhot = nsp',0)
                    nmhot = nsp
                  end if 
                else 
                  !call warning('no <nmhot> Tag' , flag = 0) 
                end if 
                
                else
                    call warning('no Control_group Tag!')
                end if 
                
                
                ! reading <Control_parameters> Tag 
                if(is_Tag(passing_node,stnode,
     *                   "Control_parameters"))then
                
                call GRAt(passing_node,"toldn",toldn)        !326
                call GRAt(passing_node,"delay",delay)        !328
                
                ! reading <Trep> tag                         !318
                sub_list=>getElementsByTagName(passing_node,"Trep")
                if (getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0)
                  call GRAr (sub_node,trep(1:ntrep),check =ntrep)
                  if (ntrep>20) then
                    call warning ('ntrep>20! -> ntrep=20',flag=0)
                    ntrep = 20
                  end if 
                  trep=1.e20
                end if 
                
                !reading <Vdt> Tag                           !324
                vdt_ok = .not..true.
                sub_list=>getElementsByTagName(passing_node,"Vdt")
                if (getLength(sub_list) == 0) then
                  ! no <vdt> check for <vdt0>
                  sub_list=>getElementsByTagName(passing_node,"Vdt0")
                  if (getLength(sub_list) == 0) then
                    ! no <vdt0> check for <vdt_r>
                    sub_list=>getElementsByTagName(passing_node,"Vdt_r")
                    if (getLength(sub_list) == 0) then
                      call warning('no <Vdt> at passing')
                    else 
                      vdt_ok = .true.
                      timdt0=timin
                    end if 
                  else 
                    vdt_ok = .true.
                    timdt0=timin
                  end if 
                else 
                  vdt_ok = .true. 
                end if 
                
                !if get any of <Vdt> tags -> start read it 
                if (Vdt_ok) then
                  sub_node=>item(sub_list,0)
                  vdt = 0
                  call GRAr(sub_node, vdt, check=ndtb)
                  ndt = abs(ndtb) / 2 
                  if(ndt>50) then
                      call warning('ndt > 50! -> ndt = 50')
                      ndt = 50
                  end if 
                  !vdt(ndt*2) = 0 
                    
                  do j = 1 , ndt*2 -1 , 2 !only odd variants  
                      if (ndtb > 0) vdt(j) = vdt(j) - timdt0
                      if (ndtb < 0) then
                        timdt0 = 0
                        vdt(j) = vdt(j) + timin
                      end if 
                  end do 
                else 
                  call warning('no <vdt>/<vdt0>/<vdt_r> Tag',flag = 0) 
                end if 
                
                
                else 
                    call warning('no Control_paarameters Tag!')
                end if 
                

                
                ! reading <Fluxes> Tag                       !330 
                if(is_Tag(passing_node, sub_node, "Fluxes")) then 
                    call GIAr(sub_node,ifout, iread = iread, n = 4)  ! reading array 
                    call GIAt(sub_node,"nps0", nps0, iread) 
                    if(iread/=0) call GIAt_old(sub_node,nps0,5)
                    call GIAt(sub_node, "nps1" , nps1, iread) 
                    if(iread/=0)call GIAt_old(sub_node, nps0,6)
                end if 
                
                !reading <Melrel> Tag                        !332
                sub_list=>getElementsByTagName(passing_node,"Melrel")
                if (getLength(sub_list) > 0) then
                  sub_node=>item(sub_list,0)
                  call GIAt(sub_node,"melrel",melrel,iread)
                  if(iread.ne.0) call GIAt_old(sub_node, melrel,1)
                  call GRAt(sub_node,"zmelt1",zmelt(1),iread)
                  if(iread.ne.0) call GRAt_old(sub_node, zmelt(1),2)
                  call GRAt(sub_node,"zmelt2",zmelt(2),iread)
                  if(iread.ne.0) call GRAt_old(sub_node, zmelt(2),3)
                end if 
                
                !reading <Point> Tag                         !334
                if(is_Tag(passing_node,sub_node,"Point"))then
                  call GCAt(sub_node,"file",cbuf,iread)
                  if(iread/=0) then 
                    kan = 1555
                    call openf(cbuf,kan,0)      !!!!!!!!!!!!!!!!!!!!!!!!!!!!! here <------
                  end if 
                    
                  if (kan==0) then                                                      ^
                    npnt = npnt + 1
                    if ( npnt > 400) call warning('too big npnt')
                    call GRAt(sub_node,"xpnt1",xpnt(1,npnt),iread)                      ^
                    if(iread/=0)call GRAt_old(sub_node,xpnt(1,npnt),1)                  ^
                    call GRAt(sub_node,"xpnt2",xpnt(2,npnt),iread)                      ^
                    if(iread/=0)call GRAt_old(sub_node,xpnt(2,npnt),2)                  ^
                  else                                                                  ^
                    read(kan,*) npnt                                                    ^
                    do j = 1 , npnt                     ! reading from file opened in-> ^
                      read(kan,*)xpnt(1,j),xpnt(2,j)
                    end do 
                    close(kan,status='keep')
                  end if 
                end if 
                
                !reading <Critrel> Tag                       !336  ! critrel: array-10 of relocation criteria
                if(is_Tag(passing_node,sub_node,"Critrel"))then
                    call GIAt(sub_node,"incrit", incrit, iread)
                    if(iread/=0)call warning('no incrit in critrel!')
                    call GRAr(sub_node,critrel,n=incrit)
                end if 
                
                !reading <Melhom> Tag                        !338
                if (is_Tag(passing_node,sub_node,"Melhom"))then 
                    call GIAt(sub_node,"melhom",melhom,iread)
                    if(iread/=0)call GIAt_old(sub_node,melhom,1)
                    if(melhom.ne.0) melhom=-iabs(melhom)
                    call GRAt(sub_node,"zmelt1",zmelt(1),iread)
                    if(iread/=0)call GRAt_old(sub_node,zmelt(1),1)
                    call GRAt(sub_node,"zmelt2",zmelt(2),iread)
                    if(iread/=0)call GRAt_old(sub_node,zmelt(2),1)
                    
                end if 
          end subroutine read_passing
          
          ! reading <General> Tag
          subroutine read_general()
                implicit none
                character(20)    :: chem_mode_text,trpout_txt ! read strings 
                type(fnode),pointer::general_node,sub_node,mats_node !node of <General> Tag ; node for childe tags
                type(fnode),pointer::stnode ! node for childe nodes of <General>
                type(fnodeList), pointer ::sub_list      !list for reading subtags

                integer(4), pointer :: i_ar_p(:)  !pointer to read integer arrays 
                real, pointer :: r_ar_p(:)     !pointer to read real arrays 
                integer :: iread   !flag: iread = 0 if attr readed; iread = 1 if attr is unreaded
                integer :: n_array,j,k !counter of array mass; loop counters
                integer :: npzce, imnm! array masses 
                integer :: ar_i(20) ! array to read from static integer arrays
                real    :: ar_r(20) ! array to read from static real    arrays
                integer :: npof
                write (logio,*) 'Start reading <general> section'
              
                !searching tag <General>
                myList => getElementsByTagName(myNode0,"General")
                if (getLength(myList) == 0) then
                    call warning('No tag <General> in input file')
                end if 
                general_node => item(myList,0)
                if (.not.associated(general_node)) then
                    call warning ('cant link to general')
                end if 
            
            ! Reading <Input_control> Tag 
            if(is_Tag(general_node,stnode,"Input_control"))then
            
            call GIAt(stnode,"nummat",nummat)         !12
            call GIAt(stnode,"numnp",numnp)           !14
            call GIAt(stnode,"numel",numel)           !16
            call GIAt(stnode,"nstrat",nstrat)         !18
            call GIAt(stnode,"igeom",igeom)           !20
            call GIAt(stnode,"ncurv",ncurvin_xml,iread)!24  inner parameter of input.for
            if  (iread == 0) ncurvin_ok = .true.
            
            call GIAt(stnode,"mpcurv",mpcurv)         !26
            
            call GIAt(stnode,"nhgen",nhgen_xml,iread) !42  inner parameter of input.for
            if (iread ==0) nhgen_ok = .true.
            
            call GIAt(stnode,"nit",nit)               !44
            
            call GIAt(stnode,"ntbc",ntbc)             !46
            call GIAt(stnode,"nfbc",nfbc)             !48
            call GIAt(stnode,"ncbc",ncbc)             !50
            call GIAt(stnode,"nrbc",nrbc)             !52
            call GIAt(stnode,"nibc",nibc)             !54
            
            call GIAt(stnode,"nrseg",nrseg)            !246
            
             call GIAt(stnode,"mdum",mdum)             !34
           !call GIAt(general_node,"mdum2",mpcurv)          !35 (not used)
           
            else 
                call warning ("no input control!")    
            end if
            
            ! Reading <Composition> Tag 
            if(is_Tag(general_node,stnode,"Composition"))then
            
            call GRAt(stnode,"uzo",uzo)                !184
            call GRAt(stnode,"utol",utol)              !186
            
            call GRAt(stnode,"destrat",destrat)         !111
            call GRAt(stnode,"tstrat",tstrat)           !112
            
            call GIAt(stnode,"invers",invers)           !113
             if(invers.eq.0) then
                invers0=0
            else
                invers0=sign(1,invers)
            endif
            if(invers0.eq. 1) invers=2  ! no change of inversed stratification
            if(invers0.eq.-1) invers=-2 ! no change of normal stratification
            
            call GRAt(stnode,"tolst",tolst)            !234
            call GRAt(stnode,"rmcrat",rmcrat)          !242
            
            call GRAt(stnode,"urst",urst)              !161
            call GRAt(stnode,"uost",uost)              !156  not used
            call GRAt(stnode,"zost",zost)              !158  not used
            call GRAt(stnode,"stst",stst)              !160  not used
            
            ! reading <nummin> Tag                           !164 
            sub_list => getElementsByTagName(stnode,"Nummin")
            if (getLength(sub_list) == 0)then
                write(logio,*) 'No <nummin> tag'
            else 
                sub_node => item(sub_list,0)
                !call GIAt(sub_node,"n",nummin)            
                call GIAr(sub_node,mnin,check=nummin)
            end if 
            
            ! reading <matnam> Tag                           !166
            sub_list => getElementsByTagName(stnode,"Matnam")
            if (getLength(sub_list) == 0)then
                write(logio,*) 'No <matnam> tag'
            else 
                sub_node => item(sub_list,0)
                !call GIAt(sub_node,"n",imnm)
                call GCAr(sub_node,mname,10,check=imnm)             
                if (imnm.ne.nummin) then 
                  call warning ('mismatch of <nummin> and <matnam>')
                end if
                if (imnm > 18) then
                  call warning ('too many components, max=18', flag=1)
                end if 
                
            end if 
            
            !reading <compini> tag                           !190
            sub_list => getElementsByTagName(stnode,"Compini")
            do j = 0 , getLength(sub_list)-1
                sub_node=>item(sub_list,j)
                call GIAt(sub_node,"matn",matn,iread)
                if (iread == 0) then    ! read <compini> in new style 
                    !call GIAt(sub_node,"n",numcm)
                    !if(numcm.ne.nummin)then
                    !call warning('arr dismatch in <compini>/<nummin>')
                    !end if
                    if(matn<0)then
                        matn=-matn
                        m_v_f(matn) = 1  !  m_v_f - shows the way of input composition of material - volume or mass fractions
                    end if
                    call GRAr(sub_node,partin(matn,1:nummin),
     *                                               check=n_array)
                    if(n_array.ne.nummin)then 
                      call warning('arr dismatch in <compini>/<nummin>')
                    end if 
                    do k = 1,nummin
                        if (partin(matn,k) < 0)then
                         call warning ('negatve coponent in <compini>')
                        end if
                    end do 
                    if (abs(sum(partin(matn,1:nummin))-1.) > 1.e-6)then
                        call warning('non 100% composition inpur')
                    end if 
                    icmp = 1    ! composition material was really inputed 
                else ! read <compini> in old syle 
                    write(logio,*) 'reqading <compini> in old style '
                    call GetIntegerArray(sub_node,i_ar_p,n_array,iread) !here n_array -> n_dim 
                    call GetRealArray(sub_node,r_ar_p,n_array,iread)    !here n_array -> n_dim 
                    matn = i_ar_p(1)
                    if(matn<0)then
                        matn=-matn
                        m_v_f(matn) = 1  !  m_v_f - shows the way of input composition of material - volume or mass fractions
                    end if 
                    numcm = i_ar_p(2) 
                    if(numcm.ne.nummin.or.size(i_ar_p)-2.ne.nummin)then
                      call warning('arr dismatch in <compini>/<nummin>')
                    end if
                    do k = 1 , nummin 
                      if(r_ar_p(k+2)<0)call warning('neg.comp compini')
                    end do 
                    partin(matn,1:nummin) = r_ar_p(3:nummin)
                    if (abs(sum(partin(matn,1:nummin))-1.) > 1.e-6)then
                        call warning('non 100% composition inpur')
                    end if 
                    icmp = 1    ! composition material was really inputed
                end if 
            end do 
            
            !reading <pst_in> Tag                            !218
            sub_list=>getElementsByTagName(stnode,"Pst_in")
            if (getLength(sub_list) > 0)then
              sub_node=>item(sub_list,0)
              !call GIAt(sub_node,"n",ncompin)
              call GRAr(sub_node,pst_in,check=ncompin,iread=iread)
              if(ncompin>nummin.and.iread==0)then
                call warning('too large "N" in <pst_in>, > than nummin')
              end if
            end if 
            
            
            
            else 
                call warning ("no input Composition!")  
            end if
            
            ! Reading <Geometry> Tag 
            if(is_Tag(general_node,stnode,"Geometry"))then
            
            call GRAt(stnode,"zstop",zstop)           !56
            
            ! reading <zce> Tag                              !140 
            sub_list => getElementsByTagName(stnode,"zce")
            if (getLength(sub_list) == 0)then
                write(logio,*) 'No <zce> tag'
            else 
                sub_node => item(sub_list,0)
                !call GIAt(sub_node,"n",npzce)           
                call GRAr(sub_node,zce)
            end if 
            
            
            ! reading <rade> Tag                              !136 
            sub_list => getElementsByTagName(stnode,"rade")
            if (getLength(sub_list) == 0)then
                write(logio,*) 'No <rade> tag'
            else 
                sub_node => item(sub_list,0)
                !call GIAt(sub_node,"n",nprade)            
                call GRAr(sub_node,rade)
            end if 
            
            ! reading <be> Tag                               !138
            sub_list => getElementsByTagName(stnode,"be") 
            if (getLength(sub_list) == 0)then
                write(logio,*) 'No <be> tag'
            else 
                sub_node => item(sub_list,0)
                !call GIAt(sub_node,"n",npbe)             
                call GRAr(sub_node,be)
            end if 
            
            else 
                call warning ("no input Geometry!")  
            end if
            
            
            
            ! Reading <Numerics> Tag 
            if(is_Tag(general_node,stnode,"Numerics"))then
            
            call GIAt(stnode,"iband",iband)           !22
            
            call GIAt(stnode,"istep",istep)           !75
            
            call GIAt(stnode,"nref",nref)               !116
            call GIAt(stnode,"maxrf",maxrf)             !120
            call GIAt(stnode,"maxit",maxit)             !122
            call GRAt(stnode,"tol",tol)                 !124
            
            call GIAt(stnode,"noit",noit)              !170
            
            else 
                call warning ("no input Numerics!")  
            end if
            
            
            ! reading <Runtime> Tag
            if(is_Tag(general_node,stnode,"Runtime")) then 
            
            call GRAt(stnode,"timin",timin)             !96
            call GRAt(stnode,"timend",timend)           !98
            call GRAt(stnode,"dt",dt)                   !100
            call GRAt(stnode,"dtmin",dtmin)             !102
            if(dtmin < 2*gdt .and. dtmin <= clockdt) then
              write(*,*)'dtmin too small !! Reduce it via "clockdt"! - '
                !call err_messr('dtmin and gdt: ',dtmin, gdt)
                write(*,*) 'input new clockdt (*):'
                read(*,*) clockdt
                write(*,'(/a,1pe16.8/)') 'Acquired clockdt: ', clockdt
            endif       
            call GRAt(stnode,"dtmax",dtmax)             !104
            call GRAt(stnode,"dtemax",dtemax)           !106
            call GRAt(stnode,"dtinc",dtinc)             !108
            
            call GIAt(stnode,"nrace",nrace)             !118
            
            else 
                call warning('no Runtime tag!')
            end if 
            
            
            ! reading <Output> Tag
            if(is_Tag(general_node,stnode,"Output"))then
            
            call GIAt(stnode,"iprnt",iprnt)           !76
            call GIAt(stnode,"iplot",iplot)           !77
            call GIAt(stnode,"idump",idump)           !78
            call GIAt(stnode,"iodat",iodat)           !79  what is iodatb??
            
            call GIAt(stnode,"ipof",ipof)               !114
            
            call GRAt(stnode,"dtplt",dtplt)            !226
            
            call GIAt(stnode,"ndistre",ndistre)        !220
            if(ndistre.lt.3.or.ndistre.gt.15) then  
                call warning('ndistre should be between 3 and 15')
            end if
            
            call GIAt(stnode,"ndistrn",ndistrn)        !222
            
            call GCAt(stnode,"trapout",trpout_txt,iread)!208  opens file result.tpp
            if(iread==0.and.trpout_txt(1:3)=="yes")then
                open(39,file='result.tpp',status='unknown')
                itrap=+1
            end if 
            
            else
                call warning('no Output Tag')
            end if 
            
            
            ! reading <Externals> Tag
            if(is_Tag(general_node,stnode,"Externals"))then
            
            call GRAt(stnode,"tolminp",tolminp)        !240
            
            call GIAt(stnode,"powuo2",ipwuo2)          !228

            call GIAt(stnode,"ntempdata",ntempdata)     !444
                        
            call GIAt(stnode,"itinp",itinp,iread)      !440  
            if(iread==0) icmtrl = 0 ! see Minput(442)
            
            !reading <Minput> Tag                       !442
            if(is_Tag(stnode,sub_node,"Minput"))then
              ifilin = 1
              icmtrl = icmtrl + 1
              call GRAt(sub_node,"timtrans",timtrans(icmtrl),iread)
              if(timtrans(icmtrl)<0.) then
                if(icmtrl.eq.1)then
                 timtrans(icmtrl)=timin-timtrans(icmtrl)
                elseif(icmtrl.gt.1)then
                 timtrans(icmtrl)=timtrans(icmtrl-1)-timtrans(icmtrl)
                endif
              endif
              
              call GRAr(sub_node,tranmass(icmtrl,1:nummin),nummin)
              call GRat(sub_node,"tranenth",tranenth(icmtrl))
              call GRat(sub_node,"trantemp",trantemp(icmtrl))
            end if
            
            !reading <Newcom> Tag                            !168 
            sub_list => getElementsByTagName(stnode,"Newcom")
            if (getLength(sub_list) == 0)then
                write(logio,*) 'No <Newcom> tag'
            else 
                sub_node => item(sub_list,0)
                !call GIAt(sub_node,"n",newcom)             
                call GIAr(sub_node,mren,check=newcom)
                do j = 1,newcom
                    mreno(mren(j)) = j
                end do
            end if 
            
            !reading <Powintm> or <Powintm0> tag             !210  
            sub_list => getElementsByTagName(stnode,"Powintm")
            if (getLength(sub_list) == 0)then
              write(logio,*)'no <Powintm> Tag, search  <powintm0>'
              sub_list => getElementsByTagName(stnode,"Powintm0")
              if (getLength(sub_list) == 0)then
                write(logio,*)'no <powintm0> Tag! -> no curv or uo2pow'
              end if
              else 
            end if 
            if (getLength(sub_list) > 0)then
                sub_node => item(sub_list,0)
                call GIAt(sub_node,"curvnumber",ncpow,iread)
                if (iread .ne. 0)call GIAt_old(sub_node,ncpow,1)
                call GRAt(sub_node,"coef",pow_UO2_cf,iread)
                if (iread .ne. 0)call GRAt_old(sub_node,pow_UO2_cf,2)
                if(getNodeName(sub_node)=="Powintm0")then
                    !p=pow_UO2_cf ! need only for log?
                    pow_UO2_cf=-pow_UO2_cf
                end if
            end if 
            
            else
                call warning('no Externals Tag')
            end if
           
            
            ! reading <Melt_model> Tag
            if(is_Tag(general_node,stnode,"Melt_model"))then
            
            call GRAt(stnode,"tnz",tnz)                 !94
            call GIAt(stnode,"nzch",nzch)             !66
            
            call GRAt(stnode,"zrst",zrst)             !28
            call GRAt(stnode,"cpst",cpst)             !30
            
            call GIAt(stnode,"levoff",levoff)         !32
            levoff = - levoff
            
            call GRAt(stnode,"tecr",tecr)             !40
            
            ! reading <Lim> Tag
            sub_list => getElementsByTagName(stnode,"Lim")!92 this is actually an array
            if (getLength(sub_list) == 0)then
                write(logio,*) 'No <lim> tag'
            else 
                sub_node => item(sub_list,0)
                call GIAr(sub_node,lim)
                
                lim(1)=iabs(lim(1))
                lim0(1)=lim(1); lim0(2)=lim(2) 
              if(lim(1)==lim(2)) call warning('!!! lim(1)=lim(2)')
            end if 
            
            ! reading <M_vess> Tag
            sub_list => getElementsByTagName(stnode,"M_vess")!93 this is actually an array
            if (getLength(sub_list) == 0)then
                write(logio,*) 'No <m_vess> tag'
            else 
                sub_node => item(sub_list,0)
                call GIAr(sub_node,m_vess)
            end if 
            else
                call warning('no Melt_model Tag!')
            end if
            
            ! reading <Corecatcher> Tag
            if(is_Tag(general_node,stnode,"Corecatcher"))then
            
            call GCAt(stnode,"chem",chem_mode_text,iread)!230
            if(iread==0)then
              if(chem_mode_text(1:3) == 'off')then
                chem_mode=0
                chem_n_reactions_local = 0
                chem_n_reactions_volume = 0
              else if(chem_mode_text(1:4) == 'trap')then
                chem_mode = 1
              else if(chem_mode_text(1:8) == 'concrete')then
                chem_mode = 2
              else
                chem_mode=0
                chem_n_reactions_local = 0
                chem_n_reactions_volume = 0
               call warning('chem:unrecognized chemistru mode!',flag=0)
              end if
            end if
            
            call GIAt(stnode,"corecat",ncc,iread)       !232
            if(iread==0) then
              if(ncc==0)then
                chem_mode = 0
                chem_n_reactions_local = 0
                chem_n_reactions_volume = 0
              else if (ncc==1) then
                chem_mode = 1
              end if
            end if
            
            call GIAt(stnode,"ierbd",ierbd)         !534 
            if (iread==0 .and. ierbd > 0 .and. nrseg == 0) then 
                call warning('no enclosure b.c. inputed for rebuild')
            end if 
                
            else
                call warning('no Corecatcher Tag!')
            end if 
            
            !Reading all other <General> attributes
           
            call GIAt(general_node,"iphase",iphase)         !36
            call GIAt(general_node,"nexts",nexts)                !38  check if it is an array
            
            call GiAt(general_node,"waterbc",iread)            !57  check for more complicated structure
            call GiAt(general_node,"iwater",iwater)            !58  check for more complicated structure
            call GiAt(general_node,"nropt",nropt)              !60  not used?
            call GiAt(general_node,"ncopt",ncopt)              !62  not used?
            call GiAt(general_node,"ntp",ntp)               !64
            
            call GIAt(general_node,"ibaln",ibaln)           !68
            
            call GRAt(general_node,"diste",diste)           !70
            call GIAt(general_node,"numnpo",numnpo)         !72
            call GIAt(general_node,"itran",itran)           !74
            
            
            call GIAt(general_node,"testcase",ncase_xml,iread)  ! 'testcase' has no goto
            if (iread == 0) then 
                iHEF=0                          ! ncase is inner parameter of input.for , mb used to work with arrays 
                ncase_ok = .true.
            end if                                                 
            call GIAt(general_node,"in_cor_pow",in_cor_pow) !81
            call GIAt(general_node,"icore",icore)           !85
            if (icore .eq. 3) then                          ! for iext flag: oxidation and interface option - 
                iext = 1                                    ! there is oxidation and cooling: built-in
                icore = 2                                   ! two core components (phases)
            else if (icore .eq. 4) then
                iext = 2                                    ! oxidation and cooling: external interface
                icore = 2                                   ! two core components (phases)
            end if
            
            call GRAt(general_node,"clog",clog)               !95
            
            call GRAt(general_node,"vls",vls)                 !110
            
            call GRAt(general_node,"tcn",tcn)                 !126
            call GRAt(general_node,"dtc",dtc)                 !128
            call GRAt(general_node,"cnr",cnr)                 !130
            call GRAt(general_node,"cnz",cnz)                 !132
            call GRAt(general_node,"tcn0",tcn0)               !134         
            
            call GRAt(general_node,"str_int",str_int)        !142
            call GRAt(general_node,"vclosed",vclosed)        !144
            call GIAt(general_node,"mcorium",mcr)            !146
            call GIAt(general_node,"msteel",mst)             !148  
            
            !150 "masfile" ?
            !152 "mass" and "masscol" ?
            !154 "minp" ?
            
            
            
            call GIAt(general_node,"lsld",lsld)              !162
             
            
            !reading <mgas> Tag                              !167
            sub_list => getElementsByTagName(general_node,"mgas")
            if (getLength(sub_list) == 0)then
              write(logio,*) 'No <mgas> tag'
            else 
              sub_node => item(sub_list,0)
              call GIAt(sub_node,"mgas",mgas_xml,iread)    !inner input.for parameter 
              if (iread .ne. 0)then 
                call GIAt_old(sub_node,mgas_xml,1,iread)
                if(iread==0)mgas_ok = .true.
              else 
                mgas_ok = .true.
              end if 
              call GIAt(sub_node,"jgas",jgas_xml,iread)    !inner input.for parameter  
              if (iread .ne. 0) then
                call GIAt_old(sub_node,jgas_xml,2,iread)
                 if(iread==0)jgas_ok = .true.
              else 
                jgas_ok = .true.
              end if 
            end if 
            
            
            
            
            call GRAt(general_node,"coldoff",coldoff)        !172
            call GIAt(general_node,"itravel",itravel)        !174
            !call GIAt(general_node,"dumptxt",)              !178  nod used
            !call GCAt(general_node,"dumpnam",filnamp)              !180 no character reader?
            !call GIAt(general_node,"restnam",)                     !182 no character reader?
            
            call GRAt(general_node,"tbigd",tbigdiag)         !188
            
            
            call GIAt(general_node,"fluxfile",ifrad)         !192  not used
            call GRAt(general_node,"fupmax",fupmax)          !194
            call GIAt(general_node,"irupway",irupway)        !196  not used
            call GRAt(general_node,"ipowin",powinf)          !198  not used
            !call GRAt(general_node,"cfent",)                !200  not used
            call GRAt(general_node,"cflup",cflup)            !202  not used
            call GRAt(general_node,"tinmin",tinmin)          !204
            call GRAt(general_node,"tinmax",tinmax)          !206
            
            
            
            call GRAt(general_node,"startplot",plotini)      !212
            call GRAt(general_node,"cfpow",cfpow)            !214
            !call GRAt(general_node,"laymxp",)               !216  possibly a subTag
            
            call GIAt(general_node,"nvect",nvect)            !224
        
            call GIAt(general_node,"liqsee",liqsee)          !236
            call GIAt(general_node,"ihomo",ihomo)            !238
            if(ihomo.ne.0) normte=8
            
            call GRAt(general_node,"thomo",thomo)            !244
            
            !call GIAt(general_node,"bubgas",)                  !248  ! bubbles ... CorCat !!
            !call GIAt(general_node,"flood",)                   !250  ! CorCat Tag
            !call GIAt(general_node,"wflux",)                   !252  ! possiblu sub tag
            call GIAt(general_node,"intrfc",intrfc)          !254
            !call GIAt(general_node,"nodout",)                  !258  ! possiblu sub tag
            call GIAt(general_node,"in_side",in_side)        !260  not used
            
            !read subtag <stop_bc>                           !262 
            sub_list=>GetElementsByTagName(General_node,"stop_bc")
            if(getLength(sub_list)>0) then
              sub_node=>item(sub_list,0)
              call GRAt(sub_node,"argum1",stop_bc(1),iread)
              if(iread.ne.0)call GRAt_old(sub_node,stop_bc(1),1)
              call GRAt(sub_node,"argum2",stop_bc(2),iread)
              if(iread.ne.0)call GRAt_old(sub_node,stop_bc(2),2)
            end if
            
            !read subtag <wat_in>                           !264 
            sub_list=>GetElementsByTagName(General_node,"wat_in")
            if(getLength(sub_list)>0) then
              sub_node=>item(sub_list,0)
              call GRAt(sub_node,"argum1",wat_in(1),iread)
              if(iread.ne.0)call GRAt_old(sub_node,wat_in(1),1)
              call GRAt(sub_node,"argum2",wat_in(2),iread)
              if(iread.ne.0)call GRAt_old(sub_node,wat_in(2),2)
              call GRAt(sub_node,"argum3",wat_in(3),iread)
              if(iread.ne.0)call GRAt_old(sub_node,wat_in(3),3)
            end if
            
            ! start reading <Fpmc> Tag 
            ! here we change General_node, so reading <Fpmc> TagЕфп
            ! should be at the end of reading <General> Tag
            ! please add all other reading upper than reading <Fpmc> 
            sub_list=> GetElementsByTagName(General_node,"Fpmc")
            if(getlength(sub_list)>0) then
              general_node=>item(sub_list,0)
              call GIAt(general_node,"ifpmc",iFPMC)    !265
              
              !start reading <Parameters> Tag           ! ?
              sub_list=>GetElementsByTagName(General_node,"Parameters")
              if(getlength(sub_list)>0) then
                sub_node=>item(sub_list,0) 
                call GCAt(sub_node,"igasnam",igasnam)    !267
                call GRAt(sub_node,"suRad",suRad)               !270
                call GRAt(sub_node,"gaspress",gaspress)         !271
                call GIAt(sub_node,"meltupd",meltupd)           !273
                call GRAt(sub_node,"t_delay",t_delay)           !274
                call GRAt(sub_node,"tbigd",tbigdiag)        !188  why is it here? 
              end if 
              
              !start reading <Mats> Tag                      !266 
              sub_list=>GetElementsByTagName(General_node,"Mats")
              if(getlength(sub_list)>0) then
                mats_node=>item(sub_list,0)
                call GIAt(mats_node,"fprmass",iFP,iread)
                if (iread==0 .and. iFP > 90) then
                  call warning('N of FP should not exceed 90!') 
                end if
                sub_list=>GetElementsByTagName(General_node,"Mat")
                if (iread == 0) then 
                  if (iFP.ne.getlength(sub_list))then
                    call warning('dismatch N of Mat and fprmass')
                  end if
                end if 
                do j=1,getlength(sub_list)
                  sub_node=>item(sub_list,j-1)  !we call arrays by j --> item should be called by j-1 
                  call GCAt(sub_node,"name",fpnam(j))
                  call GRAt(sub_node,"mass",fp_mass4(j)) 
                end do
              end if 
              !end reading <mats> Tag    
              
              ! reading <Gas_comp> Tag                       !268           check if we need fris
              sub_list=>GetElementsByTagName(General_node,"Gas_comp")
              if(getlength(sub_list)>0) then
                sub_node=>item(sub_list,0) 
                call GRAt(sub_node,"x_h2o",gas_comp(1),iread)
                if (iread.ne.0) call GRAt_old(sub_node,gas_comp(1),1)
                call GRAt(sub_node,"x_air",gas_comp(2),iread)
                if (iread.ne.0) call GRAt_old(sub_node,gas_comp(2),2)
                call GRAt(sub_node,"x_h2",gas_comp(3),iread)
                if (iread.ne.0) call GRAt_old(sub_node,gas_comp(3),3)
              end if 
              
              ! reading <Flowpar> Tag                        !269
              sub_list=>GetElementsByTagName(General_node,"Flowpar")
              if(getlength(sub_list)>0) then
                sub_node=>item(sub_list,0) 
                call GRAt(sub_node,"flowpar_v",flowV,iread)
                if (iread.ne.0) call GRAt_old(sub_node,flowV,1)
                call GRAt(sub_node,"flowpar_d",hdiam4,iread)
                if (iread.ne.0) call GRAt_old(sub_node,hdiam4,2)
              end if 
              

              
              ! reading <Ncfpmc> Tag                       !272
              sub_list=>GetElementsByTagName(General_node,"Ncfpmc")
              if(getlength(sub_list)>0) then
                sub_node=>item(sub_list,0) 
                call GIAt(sub_node,"ncfpmc1",ncfpmc,iread)
                if(iread.ne.0) call GIAt_old(sub_node,ncfpmc,1)
                call GIAt(sub_node,"ncfpmc2",nprfpmc,iread)
                if(iread.ne.0) call GIAt_old(sub_node,nprfpmc,2)
                nprfpmc=max(nprfpmc,ncfpmc)
              end if 
              
              
              ! Reading <Tinifun> Tag                       !280 
              if(is_Tag(general_node,stnode,"Tinfun"))then
                call GIAt(stnode,"initype",initype,iread)
                if(iread/=0)call warning('no initype!')
                fr_tin(initype,1:18) = 0
                call GRAr(stnode,fr_tin(initype,:),check=npof)
              end if 
              
            end if 
            ! end reading <FPMC> Tag 
            
            ! end Reading all <General> attributes
                    
            ! check correspondences and return
            if(iFPMC > 0 .and. t_delay == 0.) t_delay=1.e-5 ! [s] - for runninh FPMC in POLYRUN 
          end subroutine read_general
          
          
      
!------------------------------------------------------------------------
          ! check if material already inputed 
          subroutine checkmatn(m,mnum)
            implicit none
            integer*4 :: m,mnum(190)
            integer :: j 
            character(len = 4)::numb
            do j=1,190
                if (m == mnum(j) .and. m.ne.0) then
                 write(numb,*)j
                 call warning('matrl '//numb// ' was already inputed',1)
                end if 
            end do 
          end subroutine checkmatn
          
          ! printing warning message 
          subroutine warning(warning_message,flag)
              implicit none
              integer,optional::flag        !if flag = 0, dont stop
              character(len = *) :: warning_message
              write (*,*) '------WARNING------' 
              write (*,*) warning_message
              write (logio,*) '------WARNING------' 
              write (logio,*) warning_message
              if (present(flag)) then
                  if(flag == 0) then
                    write (*,*)'-CONTINUE WITH NOTICE-' 
                    write (logio,*) '-CONTINUE WITH NOTICE-' 
                  else if (flag == 1) then 
                    write (*,*)'-END UP WITH ERROR-' 
                    write (logio,*) '-END UP WITH ERROR-' 
                    !stop
                  end if 
              else 
                write (*,*)'-END UP WITH ERROR-' 
                write (logio,*) '-END UP WITH ERROR-' 
                !stop
              end if
              return
          end subroutine warning
          
          ! printing warning message in "warn_CC" style 
          subroutine WCC(pname)
          character(len=*)::pname !name of parameter readed in <expert> CC part 
          character(len=50)::msg 
          msg = 'Warning! Check option  '//pname//'in CorCat section!'
          call warning(msg,0) 
          end subroutine WCC 
          
          ! initializing parameters before reading from file
          subroutine init_input
               implicit none
               write (*,*) '-------------------------------------------'
               write (*,*) '-Start input initialization with XML parse-'
               write (*,*) '-------------------------------------------'
          end subroutine init_input
          
          !opening log file log.txt
          subroutine open_log()
            implicit none
            logical::ex
            inquire(file="log.txt",exist=ex)
            if(ex) then
      open(newunit=logio,file="log.txt",status="replace",action="write")
            else
      open(newunit=logio, file="log.txt",status="new",action="write")
            end if
            write (logio,*) 'Hello, this is beta version of xml pars'
          end subroutine open_log
          
          ! closing log file log.txt
          subroutine close_log()
            implicit none
      !open(newunit=logio,file="log.txt",status="old",action="write")
            write (logio,*) 'Bye, this was beta version of xml pars'
            close(logio)
          end subroutine close_log
          
          !reading integer attribute 
          !if there is no such attribute, dont do anything
          subroutine GIAt (cnode,at_name,at,notreaded)
            implicit none
            integer,optional :: notreaded       !0 if readed, 1 if not
            integer,intent(inout)::at           !what need to change if attribute exist
            character(len=*)::at_name           !attribute name
            type(fnode),pointer :: cnode    !current node to search attribute
            integer::check                      !buffer before cheking if attribute exist
            integer::index_error                !flag of error/existing attribute
            if (present(notreaded)) notreaded = 1
            check = Get_IntegerAttribute(cnode,at_name,index_error)
            if (index_error == 0) then
                at = check
                if (present(notreaded)) notreaded = 0
            else if (index_error == 1) then
                call warning('ERROR OF VALUE IN: '//at_name)
            end if
          end subroutine GIAt 
          
          !reading real attribute 
          !if there is no such attribute, dont do anything
          subroutine GRAt (cnode,at_name,at,notreaded)
            implicit none
            integer,optional :: notreaded       !0 if readed, 1 if not
            real,intent(inout)::at              !what need to change if attribute exist
            character(len=*)::at_name           !attribute name
            type(fnode),pointer :: cnode    !current node to search attribute
            real::check                         !buffer before cheking if attribute exist
            integer::index_error                !flag of error/existing attribute
            if (present(notreaded)) notreaded = 1
            check = Get_RealAttribute(cnode,at_name,index_error)
            if (index_error == 0) then
                at = check
                if (present(notreaded)) notreaded = 0
            else if (index_error == 1) then
                call warning('ERROR OF VALUE IN: '//at_name)
            end if
          end subroutine GRAt 
          
          !reading string attribute 
          !if there is no such attribute, dont do anything
          subroutine GCAt (cnode,at_name,at,notreaded)
            implicit none
            integer,optional :: notreaded       !0 if readed, 1 if not
            !integer::n                         !max len of sttribute name
            character(len=*),intent(inout)::at  !what need to change if attribute exist
            character(len=*)::at_name           !attribute name
            type(fnode),pointer :: cnode    !current node to search attribute
            character(20)::check                !buffer before cheking if attribute exist
            integer::index_error                !flag of error/existing attribute
            if (present(notreaded)) notreaded = 1
            check = getAttribute(cnode,at_name,index_error)
            if (index_error == 0) then
                at = check
                if (present(notreaded)) notreaded = 0
            else if (index_error == 1) then
                !call warning('ERROR OF VALUE IN: '//at_name,0)
            end if
          end subroutine GCAt 
          
          !reading integer array 
          !if there is no such attribute, dont do anything
          ! optinal parameters:
          !     n      if present -> read static array with lenght = n
          !     iread  if present -> return info if array was readed (iread = 0) or not ( = 1)
          !     check  if present -> return leng of readed array  
          subroutine GIAr (cnode, ar, n, iread,check)
            implicit none
            integer::ar(:)                      !array to cnahge
            integer,optional::n                 !array mass
            type(fnode),pointer :: cnode    !current xml node 
            integer(4),pointer  :: Ar_p(:)      !array to read from node 
            integer:: ierr, n_dim, j, is_at     !error flag, dimension counter, loop counter, flag if "n" was readed as attribute 
            integer,optional :: iread           !flag if array was readed 
            integer,optional :: check           !if check is here, we verify if lenght of array equal to check 
            character(len=40)::er1,er2          !error masseges 
            integer:: leng                      !lenght of an array
            er1 = 'array dismatch in: ' //getNodeName(cnode)
            er2 = 'empty array in: ' //getNodeName(cnode)
            
            ! if we got "lenght" of array, just read static array 
            if (present(n)) then
                leng = n
                call GetIntegerArray(cnode,Ar_p,n_dim,ierr)
            ! if we dont get "lenght" of array, search for it 
            else if (.not. present(n)) then
                call GIAt(cnode,"lenght",leng,is_at) 
                if (is_at.ne.0)then
                    call GetIntegerArray(cnode,Ar_p,n_dim,ierr)
                    if (size(Ar_p)==0) then
                        call warning(er2,0)
                        if (present(iread)) iread = 1
                        return 
                    end if 
                    leng = Ar_p(1)
                end if 
                call GetIntegerArray(cnode,Ar_p,n_dim,ierr)
                if (is_at .ne.0) Ar_p=>Ar_p(2:)
            end if 
            
            if (present(iread)) then 
                if (ierr .ne. 0) then 
                    iread = ierr
                    return
                else 
                    iread = 0
                end if 
            end if 
            
            if (present(check)) then
                check = leng
            end if 
            
            if (size(ar_p) == 0 ) then 
                if (present(iread)) iread = 1
                return 
            end if 
            
            if(leng < 1) call warning(er2)
            if (size(Ar_p) == 0 ) then
                call warning(er2,flag=0)
            end if
            if (leng > size(Ar_p)) then 
                call warning (er1,flag = 1)
            else if (leng < size(Ar_p)) then
                call warning (er1,flag = 0)
            else if (leng > size(ar)) then 
                call warning (er1, flag = 1) 
            end if 
            
            do j = 1, leng
                ar(j) = Ar_p(j)
            end do
          end subroutine GIAr 
          
          !reading real array 
          !if there is no such attribute, dont do anything
!          subroutine GRAr (cnode, ar, n, iread)
!            implicit none
!            real::ar(:)                      !array to change
!            integer::n                       !array mass
!            real,pointer  :: Ar_p(:)         !array to read from node 
!            type(fnode),pointer :: cnode !current xml node 
!            integer :: ierr, n_dim, j        !error flag, dimension counter, loop counter
!            integer,optional :: iread                 !flag if array was readed 
!            character(len=19)::er1,er2 
!            er = 'array dismatch in: ' 
!            call GetRealArray(cnode,Ar_p,n_dim,ierr)
!            
!      if(n < 1) call warning('empty array in '//getNodeName(cnode))
!            if (size(Ar_p) == 0 ) then
!      call warning('empty array in '//getNodeName(cnode),flag=0)
!            end if
!            if (n > size(Ar_p)) then 
!                call warning (er//getNodeName(cnode),flag = 1)
!            else if (n < size(Ar_p)) then
!                call warning (er//getNodeName(cnode),flag = 0)
!            end if 
!            do j = 1, n
!                ar(j) = Ar_p(j)
!            end do
!          end subroutine GRAr 
          
          !reading real array 
          !if there is no such attribute, dont do anything
          ! optinal parameters:
          !     n      if present -> read static array with lenght = n
          !     iread  if present -> return info if array was readed (iread = 0) or not ( = 1)
          !     check  if present -> return leng of readed array  
          subroutine GRAr (cnode, ar, n, iread,check)
            implicit none
            real::ar(:)                      !array to change
            integer,optional::n              !array lenght, if (precent(n)) array is static and we dont search for it lenght 
            real,pointer  :: Ar_p(:)         !array to read from node 
            integer,pointer  :: Ar_i(:)      !array to read array mass
            type(fnode),pointer :: cnode !current xml node 
            integer:: ierr, n_dim, j, is_at  !error flag, dimension counter, loop counter, flag if "n" was readed as attribute 
            integer,optional :: iread        !flag if array was readed 
            integer,optional :: check        ! if check is here, we verify if lenght of array equal to check 
            character(len=40)::er1,er2       !error masseges 
            integer:: leng                   ! lenght of an array
            er1 = 'array dismatch in: ' //getNodeName(cnode)
            er2 = 'empty array in: ' //getNodeName(cnode)
            
            ! if we got "lenght" of array, just read static array 
            if (present(n)) then
                leng = n
                call GetRealArray(cnode,Ar_p,n_dim,ierr)
            ! if we dont get "lenght" of array, search for it 
            else if (.not. present(n)) then
                call GIAt(cnode,"lenght",leng,is_at) 
                if (is_at.ne.0)then
                    call GetIntegerArray(cnode,Ar_i,n_dim,ierr)
                    if (size(Ar_i)==0) then
                        call warning(er2,0)
                        if (present(iread)) iread = 1
                        return 
                    end if 
                    leng = Ar_i(1)
                end if 
                call GetRealArray(cnode,Ar_p,n_dim,ierr)
                if (is_at .ne.0) Ar_p=>Ar_p(2:)
            end if 
                        
            if (present(iread)) then 
                if (ierr .ne. 0) then 
                    iread = ierr
                    return
                else 
                    iread = 0
                end if 
            end if 
            
            if (present(check)) then
                check = leng
            end if 
            
            if (size(ar_p) == 0 ) then 
                if (present(iread)) iread = 1
                return 
            end if 
            
            if(leng < 1) call warning(er2)
            if (size(Ar_p) == 0 ) then
                call warning(er2,flag=0)
            end if
            if (leng > size(Ar_p)) then 
                call warning (er1,flag = 1)
            else if (leng < size(Ar_p)) then
                call warning (er1,flag = 0)
            else if (leng > size(ar)) then 
                call warning (er1, flag = 1) 
            end if 
            
            do j = 1, leng
                ar(j) = Ar_p(j)
            end do
          end subroutine GRAr 
          
          !reading string array 
          !if there is no such attribute, dont do anything
          ! optinal parameters:
          !     n      if present -> read static array with lenght = n
          !     iread  if present -> return info if array was readed (iread = 0) or not ( = 1)
          !     check  if present -> return leng of readed array  
          subroutine GCAr (cnode, ar, s_len, n, iread, check)
            implicit none
            character(len=*)::ar(:)                 !array to change
            integer:: s_len                         ! len of string
            integer,optional::n, iread, check       !array lencght (if present -> static array), falg if array was readed, flag if we need to check readed array lebght 
            character(len=s_len),pointer :: Ar_p(:) !array to read from node 
            type(fnode),pointer :: cnode            !current xml node 
            integer :: ierr, n_dim, j               !error flag, dimension counter, loop counter
            integer :: leng , is_at                 ! flag if lenght was readed as an attribute 
            character(len=40)::er1,er2
            er1 = 'array dismatch in: '//getNodeName(cnode) 
            er2 = 'empty array in '//getNodeName(cnode) 
            !call GetCharacterArray(cnode, Ar_p, n_dim, ierr,s_len)
            
            if (present(n)) then 
                leng = n
                call GetCharacterArray(cnode,Ar_p,n_dim,ierr,s_len)
            else 
                call GIAt(cnode, "lenght", leng, is_at)
                if (is_at.ne.0) then 
                    call GetCharacterArray(cnode,Ar_p,n_dim,ierr,s_len)
                    if (size(Ar_p)==0) then
                        call warning(er2,0)
                        if (present(iread)) iread = 1
                        return 
                    end if 
                    read (Ar_p(1),*) leng
                end if
                call GetCharacterArray(cnode,Ar_p,n_dim,ierr,s_len)
                if (is_at .ne.0) Ar_p=>Ar_p(2:)
            end if 
            
            if (present(iread)) then 
                if (ierr .ne. 0) then 
                    iread = ierr
                    return
                else 
                    iread = 0
                end if 
            end if 
            
            if (present(check)) then
                check = leng
            end if 
            
            if (size(ar_p) == 0 ) then 
                if (present(iread)) iread = 1
                return 
            end if 
            
            if(leng < 1) call warning(er2)
            if (size(Ar_p) == 0 ) then
                call warning(er2,flag=0)
            end if
            if (leng > size(Ar_p)) then 
                call warning (er1//getNodeName(cnode),flag = 1)
            else if (leng < size(Ar_p)) then
                call warning (er1//getNodeName(cnode),flag = 0)
            end if 
            do j = 1, leng
                ar(j) = Ar_p(j)
            end do
          end subroutine GCAr
          
          !subriutine read integer attribute from array (in old style)
          ! parameters of subroutine:
          !         cnode -> current node to search array 
          !         at    -> attribute to search in array
          !         n     -> place of the attribte in array
          !         iread -> flag if we read this attribure form array (optional)
          subroutine GIAt_old(cnode, at , n,iread )
            type(fnode),pointer :: cnode!current node to seacrh
            integer :: at               !parameter to change 
            integer :: n                !place of the attribute in array
            integer,optional :: iread   !flag if was readed 
            integer,pointer :: ar_p(:)     !array to read from node 
            integer :: n_dim, ierr      !dimension, error flag 
            character(len=70) ::er1,er2 !error message 

            write (er1,*) n
            er1 = 'Old style reading: array is shorter than ' // er1
            er1 = er1 // ' in ' //getNodeName(cnode)
            er2 = 'Old style reading: error of reading in  '
            er2 = er2 // getNodeName(cnode)
            call GetIntegerArray(cnode,ar_p,n_dim,ierr)
            if (size(ar_p) < n) then 
                call warning (er1,0)
                if (present(iread)) iread = 1
                return
            end if
            if (ierr .ne. 0) then 
                if (present(iread)) iread = ierr
                call warning(er2,1)
            end if 
            at = ar_p(n)
            if (present(iread)) iread = 0
          end subroutine GIAt_old
          
          !subriutine read real attribute from array (in old style)
          ! parameters of subroutine:
          !         cnode -> current node to search array 
          !         at    -> attribute to search in array
          !         n     -> place of the attribte in array
          !         iread -> flag if we read this attribure form array (optional)
          subroutine GRAt_old(cnode, at , n,iread )
            type(fnode),pointer :: cnode!current node to seacrh
            real :: at               !parameter to change 
            integer :: n                !place of the attribute in array
            integer,optional :: iread   !flag if was readed 
            real,pointer :: ar_p(:)     !array to read from node 
            integer :: n_dim, ierr      !dimension, error flag 
            character(len=70) ::er1,er2 !error messag
            write (er1,*) n
            er1 = 'Old style reading: array is shorter than ' // er1
            er1 = er1 // ' in ' //getNodeName(cnode)
            er2 = 'Old style reading: error of reading in  '
            er2 = er2 // getNodeName(cnode)
            call GetRealArray(cnode,ar_p,n_dim,ierr)
            if (size(ar_p) < n) then 
                call warning (er1,0)
                if (present(iread)) iread = 1
                return
            end if
            if (ierr .ne. 0) then 
                if (present(iread)) iread = ierr
                call warning(er2,1)
            end if 
            at = ar_p(n)
            if (present(iread)) iread = 0
          end subroutine GRAt_old
          
          !subriutine read char attribute from array (in old style)
          ! parameters of subroutine:
          !         cnode -> current node to search array 
          !         at    -> attribute to search in array
          !         n     -> place of the attribte in array
          !         iread -> flag if we read this attribure form array (optional)
          subroutine GCAt_old(cnode, at , n,iread,s_len )
            type(fnode),pointer :: cnode!current node to seacrh
            character(len=*) :: at      !parameter to change 
            integer,optional :: s_len   !lenght of string to read (default = 20)
            integer :: n                !place of the attribute in array
            integer,optional :: iread   !flag if was readed 
            character(len=20),pointer :: ar_p(:) !array to read from node 
            integer :: n_dim, ierr ,sl  !dimension, error flag , default s_len
            character(len=70) ::er1,er2 !error message 
            write (er1,*) n
            if(present(s_len)) then
                sl = s_len
            else 
                sl = 20
            end if 
            er1 = 'Old style reading: array is shorter than ' // er1
            er1 = er1 // ' in ' //getNodeName(cnode)
            er2 = 'Old style reading: error of reading in  '
            er2 = er2 // getNodeName(cnode)
            call GetCharacterArray(cnode,ar_p,n_dim,ierr,sl)
            if (size(ar_p) < n) then 
                call warning (er1,0)
                if (present(iread)) iread = 1
                return
            end if
            if (ierr .ne. 0) then 
                if (present(iread)) iread = ierr
                call warning(er2,1)
                return
            end if 
            at = ar_p(n)
            if (present(iread)) iread = 0
          end subroutine GCAt_old
          
          ! search for the first node with name (n_name) in node node_from
          ! if finded -> is_tag = .true.  ;   node_to -> became finded subnode 
          ! if not    -> is_tag = .false. ;   node_to -> do nothing 
          logical function is_Tag (node_from, node_to, n_name)
          implicit none 
          type(fnode), pointer :: node_from , node_to   !node where we search , finded node 
          character(len=*) :: n_name                    !node name to search
          type(fnodelist),pointer:: list                !list of nodes
          integer :: iread                              !flag
          
          list=>GetElementsByTagName(node_from, n_name) 
          if (getLength(list) == 0) then 
            write(logio,*)'No '//n_name//' in '//GetNodeName(node_from)
            is_Tag = .false.
            return
          else if (getLength(list) == 1) then
            node_to => item(list,0) 
            is_Tag = .true.
            return
          else if (getLength(list) > 1) then 
            node_to => item(list,0)
            call warning('More than 1 tags with same name',0)
            is_Tag = .true.
            return
          else 
            call warning('Breaked logic wuth Node List ')
          end if 
          end function is_Tag 
          
!-------------------------------------------------------------------------
!    open file with definite name whenever one exists;
!    op - channel of opening file
!    st - action parameter - 
! is:          file:
! is=1  -      exists  -> delete -> open new    - overwrite with asking (=1)
!              exists  ->      open old         - open with warning (/=1)
! is=1  -  not exists  ->      open new        
!
! is=0  -      exists  ->      open old         - open ...
! is=0  -  not exists  ->      does not open    -   stop calculation
!
! is=-1 -      exists  ->      open old         - status='unknown' - owerwrite
! is=-1 -  not exists  ->      open new         - status='unknown'
!
! is=-2 -      exists  -> delete -> open new    - overwrite anyway
! is=-2 -  not exists  ->      open new         - status='new'

          subroutine openf(fname,op,st)
            implicit none 
            integer :: op ! file manager 
            integer :: st ! status of opening  ; see higher
            character*(*):: fname , nam1*8 ! file name to open, second file name
            logical :: exi  ! flag if file exist
            integer :: idel ! user's flag if delete file -> 1 if need to delete
            
            inquire (file=fname, exist = exi)   ! check if file exist
            
            if (exi) then               ! if exist -> continue 
              if(st>0) then             ! if -> 
                write(*,*)'press 1 if you want to delete file?  ->'
                read (*,*)idel
                if (idel == 1) then 
                  open(unit=op,file=fname,status='old')  ! close existing file
                  close(unit=op,status='delete')        ! clsoe wiht deleting
                  open(unit=op,file=fname,status='new')  ! opening brand new file
                  write(logio,*) 'delete and creat file '//fname 
                else 
                  nam1=fname(1:6)//'new'                 ! opening new file 
                  open(unit=op,file=nam1,status='unknown')
                  write(*,*)'opened file -> nam1' 
                  write(logio,*)'opened file -> nam1' 
                endif
              elseif(st==0) then
                open(unit=op,file=fname,status='old')
                write(logio,*)'opening file '//fname
                write(*,*)'opening file '//fname
              elseif(st==-1) then
                open(unit=op,file=fname,status='unknown')
                write(logio,*)'opening file '//fname
                write(*,*)'opening file '//fname
              elseif(st==-2) then
                open(unit=op,file=fname,status='old')
                close(unit=op , status='delete')
                open(unit=op,file=fname,status='new')
                write(logio,*)'opening new file '//fname
                write(*,*)'opening new file '//fname
              endif 
            else
              if(st.ne.0) then  ! open new
                open(unit=op,file=fname,status='new')
              else              ! exit
                call warning('file '//fname//' does not exist')
              endif
            end if
             
          end subroutine openf 
          

          ! suboutine for reading massive from Block <Conditions>
          ! Firstly we read it from XML file
          ! Secondly we write it in special file 
          ! This file is readed by Input.for 
          subroutine array_reader(st,kan,fname)
            type(fnode)::st              ! node to read from
            integer :: kan               ! chanel to write 
            character(len = 30) :: fname ! file name to write 
          end subroutine array_reader
          
          ! subroutine for reading table from node,
          subroutine read_t(sn,curvx,curvy,inpc)
          !implicit none 
            real,dimension(1) :: curvx, curvy
            real :: inpc(1)
            type(fnode), pointer :: sn ! working node 

            integer:: ncurv, npoints , st ! num of curv ; amount of points ; start point to read
            real:: ar_r(mpcurv*2) ! buffer array
            integer :: ind , i , iread !index of filling curvx/y ; loop counter ; flag of  reading attribute
            st = 0      ! if all is attr start from 0 
            
            call GIAt(sn, "ncurv", ncurv , iread)       !   check if ncurv is attr
            if(iread/=0)then
                call GIAt_old(sn,ncurv,1)
                st = st + 1 
            end if 
            
            call GIAt(sn, "npoints" , npoints, iread)   !   check if npoints is attr
            if(iread/=0) then 
                call GIAt_old(sn,npoints,st+1)
                st = st+1 
            end if    
            
            if(npoints > mpcurv)call warning('too much points!')    ! check if mpcurv is too big 
            if(ncurv   > ncurvin_xml)  call warning('too big ncurv!')      ! check if ncurv  is too big
            
            call GRAr(sn, ar_r , 2*npoints + st , iread)            ! read array of table 
            
            if (iread/=0) then 
                call warning ('error of reading table!')
                return
            end if 
            
            inpc(ncurv) = npoints    ! write n of points 
            
            ind = 1 + (ncurv - 1) * mpcurv ! start filling cruvx/y from index 1 + ... 
            do i = st+1,st+npoints*2, 2
                curvx(ind)= ar_r(i)
                curvy(ind)= ar_r(i+1)
                ind = ind + 1           ! read point -> ind++
            end do 
            
          end subroutine read_t 
      end module xml_reader_Hef_CC
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
      
